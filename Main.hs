{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances, RecursiveDo                            #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind -fno-warn-orphans #-}
module Main where
import           Control.Applicative       ((<$>))
import           Control.Eff
import           Control.Eff.Exception     (runExc, Exc)
import           Control.Eff.Fresh         (Fresh, fresh)
import           Control.Eff.Lift          (Lift, lift, runLift)
import           Control.Eff.Random
import           Control.Eff.Reader.Strict (Reader, ask, runReader)
import           Control.Lens              ((&), (^.), set)
import           Control.Monad             (forM_, void, (<=<))
import           Data.Default
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import Data.Color
import           Data.Typeable             (Typeable (..))
import           FRP.Sodium
import qualified FRP.Sodium                as FRP
import           FRP.Sodium.IO             (executeSyncIO)
import           GHCJS.Foreign
import           GHCJS.Types
import           JavaScript.Canvas
import           JavaScript.JQuery         hiding (not, Event)

import Puzzle

#if MIN_VERSION_base(4,7,0)
#define Typeable1 Typeable
#else
import Data.Typeable (Typeable1 (..))
#endif
import Control.Lens ((.~))
import Data.Color.Names (red)

keyLeftD :: Int
keyLeftD = 37
keyUpD :: Int
keyUpD   = 38
keyRightD :: Int
keyRightD = 39
keyDownD :: Int
keyDownD  = 40

#ifdef __GHCJS__
foreign import javascript unsafe "$2.style.height = $1"
  setStyleHeight  :: Int -> Canvas -> IO ()
foreign import javascript unsafe "$2.style.width = $1"
  setStyleWidth  :: Int -> Canvas -> IO ()
#else
setStyleHeight :: Int -> Canvas -> IO ()
setStyleHeight _ _ = error "jsffi"

setStyleWidth :: Int -> Canvas -> IO ()
setStyleWidth _ _ = error "jsffi"
#endif

accumMWith :: (Event (IO a) -> Event a) -- ^ execution method for IO
           -> a -> Event (a -> IO a)
           -> Reactive (Behaviour a)
accumMWith execIO z efa = do
  rec s <- hold z $ execIO $ snapshot ($) efa s
  return s

dic :: [(Int, Direction)]
dic = [(keyLeftD,  LeftD)
      ,(keyUpD,    UpD)
      ,(keyRightD, RightD)
      ,(keyDownD,  DownD)
      ]

newtype Cxt  = Cxt { unwrapCxt :: Context } deriving (Typeable)

readerT :: (Typeable1 m, SetMember Lift (Lift m) r, Member (Reader Cxt) r)
        => (Context -> m b) -> Eff r b
readerT f = lift . f . unwrapCxt =<< ask

main :: IO ()
main = runLift $ evalRandIO $ do
  bd <- newBoard
  lift $ do
    body <- select "body"
    container <- select "#main"
    c <- select "<canvas id='theCanvas' width='180px' height='180px' />"
    label <- select "<div />"
    appendJQuery c container
    appendJQuery label container
    cxt <- getContext =<< indexArray 0 (castRef c)
    setWidth  180 c
    setHeight 180 c
    keyEvent <- keyDownEvent body
    sync $ do
      let updEvent = updater <$> filterJust (flip lookup dic <$> keyEvent)
      bhv <- accumMWith executeSyncIO (GS bd 0) updEvent
      listen (value bhv) $ \gs -> do
        runLift $ runReader (drawBoard $ gs ^. board) (Cxt cxt)
        void $ setText ("Score: " <> T.pack (show $ gs ^. score)) label
  return ()

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
sqSize    = 40

sqMargine :: Double
sqMargine = 5

drawBoard :: (SetMember Lift (Lift IO) r, Member (Reader Cxt) r) => Board -> Eff r ()
drawBoard b = do
  readerT $ clearRect 0 0 180 180
  forM_ (withIndex b) $ \((j, i), mint) -> do
    readerT save
    readerT $ strokeStyle 0 0 0 1
    readerT $ strokeRect
                (fromIntegral i * (sqSize + sqMargine))
                (fromIntegral j * (sqSize + sqMargine))
                sqSize sqSize
    readerT $ fillStyle 0 0 0 1
    drawNumber (i, j) mint
    readerT $ restore

drawNumber :: (SetMember Lift (Lift IO) r, Member (Reader Cxt) r)
           => (Int, Int) -> Maybe Int -> Eff r ()
drawNumber _ Nothing = return ()
drawNumber (i, j) (Just t) = do
  let str = T.pack $ show t
  tw <- readerT $ measureText str
  let x = (sqSize - tw) / 2
      Color r g b _ = red & _Hue .~ (fromIntegral t - 1) / 2
  readerT $ fillStyle (floor $ r*255) (floor $ g*255) (floor $ 255* b) 0.5
  readerT $ fillRect
    (fromIntegral i * (sqSize + sqMargine))
    (fromIntegral j * (sqSize + sqMargine))
    sqSize sqSize
  readerT $ fillText str
              (fromIntegral i*(sqSize+sqMargine) + x)
              (fromIntegral j*(sqSize + sqMargine)+sqSize/2)

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
