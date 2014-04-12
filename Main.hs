{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, RecursiveDo   #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances, CPP                              #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind -fno-warn-orphans #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Main where
import           Control.Applicative       ((<$>))
import           Control.Eff
import           Control.Eff.Exception     (Exc, runExc)
import           Control.Eff.Fresh         (Fresh, fresh)
import           Control.Eff.Lift          (Lift, lift, runLift)
import           Control.Eff.Random
import           Control.Lens              (set, (&), (^.))
import           Control.Monad             (void, (<=<))
import           Control.Monad.Fix         (mfix)
import           Data.Default
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           FRP.Sodium
import qualified FRP.Sodium                as FRP
import           FRP.Sodium.IO             (executeSyncIO)
import           GHCJS.Types
import           JavaScript.JQuery         hiding (Event, not)

import DiagramsBackend
import Puzzle
import GHCJS.DOM.Element (Element)

#ifdef __GHCJS__
-- create an element in the SVG namespace
foreign import javascript unsafe "document.createElementNS('http://www.w3.org/2000/svg',$1)"
   createSvg :: JSString -> IO Element
foreign import javascript unsafe "document.getElementsByTagName($1)"
   getElementsByTagName :: JSString -> IO (JSArray a)
foreign import javascript unsafe "$3.setAttribute($1,$2)"
   setAttribute :: JSString -> JSRef a -> JSRef b -> IO ()
foreign import javascript unsafe "$2.appendChild($1)"
   appendChild :: Element -> Element -> IO ()
#else
createSvg :: JSString -> IO Element
createSvg = undefined
appendChild :: Element -> Element -> IO ()
appendChild = undefined
getElementsByTagName :: JSString -> IO (JSArray a)
getElementsByTagName = undefined
setAttribute :: JSString -> JSRef a -> JSRef b -> IO ()
setAttribute = undefined
#endif

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

main :: IO ()
main = runLift $ evalRandIO $ do
  bd <- newBoard
  lift $ do
    body <- select "body"
    container <- select "#main"
    c <- selectElement =<< createSvg "svg"
    label <- select "<div />"
    appendJQuery c container
    appendJQuery label container
    keyEvent <- keyDownEvent body
    sync $ do
      let updEvent = updater <$> filterJust (flip lookup dic <$> keyEvent)
      game <- accumMWith executeSyncIO (GS bd 0) updEvent
      stopUpd <- listen (value game) $ \gs -> do
        renderBoard (gs ^. board) c
        void $ setText ("Score: " <> T.pack (show $ gs ^. score)) label
      let isMovable = movable <$> game
      listen (once $ filterE not $ value isMovable) $ \_ -> do
        stopUpd
{-
        draw cxt $ locally $ do
          let msg = "GAME OVER"
              pxs = canvasSize * 0.75 / fromIntegral (T.length msg)
          textBaseline Middle
          font $ "bold " <> T.pack (show pxs) <> "px roman"
          fillStyle 0 0 0 0.75
          fSize <- measureText msg
          fillText msg ((canvasSize - fSize) / 2) (canvasSize / 2)
-}
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
