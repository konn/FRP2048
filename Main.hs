{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, RecursiveDo   #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances                                    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind -fno-warn-orphans #-}
module Main where
import           Control.Applicative       ((<$>))
import           Control.Eff
import           Control.Eff.Exception     (Exc, runExc)
import           Control.Eff.Fresh         (Fresh, fresh)
import           Control.Eff.Lift          (Lift, lift, runLift)
import           Control.Eff.Random
import           Control.Eff.Reader.Strict (Reader)
import           Control.Lens              (set, (&), (.~), (^.))
import           Control.Monad             (forM_, void, (<=<))
import           Control.Monad.Fix         (mfix)
import           Data.Color
import           Data.Color.Names          (red)
import           Data.Default
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Typeable             (Typeable (..))
import           FRP.Sodium
import qualified FRP.Sodium                as FRP
import           FRP.Sodium.IO             (executeSyncIO)
import           GHCJS.Foreign
import           GHCJS.Types
import           JavaScript.JQuery         hiding (Event, not)

import Drawer
import Puzzle

keyLeftD :: Int
keyLeftD = 37
keyUpD :: Int
keyUpD   = 38
keyRightD :: Int
keyRightD = 39
keyDownD :: Int
keyDownD  = 40

accumMWith :: (Event (IO a) -> Event a) -- ^ execution method for IO
           -> a -> Event (a -> IO a)
           -> Reactive (Behaviour a)
accumMWith execIO z efa = mfix $ \ ~s -> hold z $ execIO $ snapshot ($) efa s

dic :: [(Int, Direction)]
dic = [(keyLeftD,  LeftD)
      ,(keyUpD,    UpD)
      ,(keyRightD, RightD)
      ,(keyDownD,  DownD)
      ]

newtype Cxt  = Cxt { unwrapCxt :: Context } deriving (Typeable)

main :: IO ()
main = runLift $ evalRandIO $ do
  bd <- newBoard
  lift $ do
    body <- select "body"
    container <- select "#main"
    c <- select $ T.concat ["<canvas id='theCanvas' width='"
                           ,T.pack $ show canvasSize
                           ,"px' height='"
                           ,T.pack $ show canvasSize
                           ,"px' />"
                           ]
    label <- select "<div />"
    appendJQuery c container
    appendJQuery label container
    cxt <- getContext =<< indexArray 0 (castRef c)
    setWidth  canvasSize c
    setHeight canvasSize c
    keyEvent <- keyDownEvent body
    sync $ do
      let updEvent = updater <$> filterJust (flip lookup dic <$> keyEvent)
      game <- accumMWith executeSyncIO (GS bd 0) updEvent
      stopUpd <- listen (value game) $ \gs -> do
        draw cxt (drawBoard $ gs ^. board)
        void $ setText ("Score: " <> T.pack (show $ gs ^. score)) label
      let isMovable = movable <$> game
      listen (once $ filterE not $ value isMovable) $ \_ -> do
        stopUpd
        draw cxt $ locally $ do
          let msg = "GAME OVER"
              pxs = canvasSize * 0.75 / fromIntegral (T.length msg)
          textBaseline Middle
          font $ "bold " <> T.pack (show pxs) <> "px roman"
          fillStyle 0 0 0 0.75
          fSize <- measureText msg
          fillText msg ((canvasSize - fSize) / 2) (canvasSize / 2)
  return ()

movable :: GameState -> Bool
movable gs = any (\dir -> shiftGS dir gs /= gs) [LeftD, RightD, UpD, DownD]

runFail :: Eff (Exc () :> r) a -> Eff r (Maybe a)
runFail act = either (const Nothing) Just <$> runExc act
{-# INLINE runFail #-}

updater :: Direction -> GameState -> IO GameState
updater dir gs = do
  let gs' = shiftGS dir gs
  if gs' == gs
    then return gs
    else do
    bd' <- runLift $ runFail $ evalRandIO $ randomPlace (gs' ^. board)
    return $ gs' & maybe id (set board) bd'

sqSize :: Double
sqSize   = 100

sqMargin :: Double
sqMargin = 5

sqOffset :: Double
sqOffset = sqSize + sqMargin

canvasSize :: Double
canvasSize = sqMargin*3 + sqSize*4

drawBoard :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Board -> Eff r ()
drawBoard b = locally $ do
  clearRect 0 0 canvasSize canvasSize
  forM_ (withIndex b) $ \((j, i), mint) -> locally $ do
    strokeStyle 0 0 0 1
    strokeRect
      (fromIntegral i * sqOffset)
      (fromIntegral j * sqOffset)
      sqSize sqSize
    fillStyle 0 0 0 1
    drawNumber (i, j) mint

logBase' :: Floating a => a -> a -> a
logBase' a b = log b / log a

drawNumber :: (Integral t, Integral a, Integral a1, Show t, SetMember Lift (Lift IO) r, Member (Reader Context) r) => (a, a1) -> Maybe t -> Eff r ()
drawNumber _ Nothing = return ()
drawNumber (i, j) (Just t) = locally $ do
  let str = T.pack $ show t
      pxs = floor $ (sqSize*3/4) / fromIntegral (T.length str)
  font $ "bold " <> T.pack (show pxs) <> "px roman"
  tw <- measureText str
  textBaseline Middle
  let x = (sqSize - tw) / 2
      Color r g b _ = red & _Hue .~ 360 * (logBase' 2 (fromIntegral t) - 1) / 10
  fillStyle (floor $ r*255) (floor $ g*255) (floor $ 255* b) 0.5
  fillRect
    (fromIntegral i * sqOffset)
    (fromIntegral j * sqOffset)
    sqSize sqSize
  fillStyle 0 0 0 1
  fillText str
    (fromIntegral i*sqOffset + x)
    (fromIntegral j*sqOffset+sqSize/2)

keyDownEvent :: JQuery -> IO (FRP.Event Int)
keyDownEvent par = do
  (ev, push) <- sync newEvent
  keydown (sync . push <=< which) def par
  return ev

reactiveCheckbox :: (SetMember Lift (Lift IO) r, Member (Fresh Int) r)
                 => JQuery -> Text -> Eff r (Behavior Bool)
reactiveCheckbox bdy labTxt = do
  num <- fresh
  let ident =  "chk" <> T.pack (show (num :: Int))
  lift $ do
    (bhvr, push) <- sync $ newBehaviour False
    chk <- select $ "<input id='" <> ident <> "' type='checkbox' value='0' />"
    lab <- select $ "<label for='" <> ident <> "' />"
    setText labTxt lab
    appendJQuery chk bdy
    appendJQuery lab bdy
    change (\_ -> sync . push =<< is ":checked" chk) def chk
    return bhvr
