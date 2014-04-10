{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances, ForeignFunctionInterface               #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind -fno-warn-orphans #-}
module Main where
import           Control.Applicative       ((<$>))
import           Control.Eff
import           Control.Eff.Exception     (runExc)
import           Control.Eff.Lift          (Lift, lift, runLift)
import           Control.Eff.Random
import           Control.Eff.Reader.Strict (Reader, ask, runReader)
import           Control.Lens              (view)
import           Control.Monad             (forM_, void, (<=<))
import           Data.Default
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Typeable             (Typeable (..))
import           FRP.Sodium
import qualified FRP.Sodium                as FRP
import           FRP.Sodium.IO             (executeSyncIO)
import           GHCJS.Foreign
import           GHCJS.Types
import           JavaScript.Canvas
import           JavaScript.JQuery         hiding (not)
import           Control.Eff.Fresh         (Fresh, fresh)

import Puzzle

#if MIN_VERSION_base(4,7,0)
#define Typeable1 Typeable
#else
import Data.Typeable (Typeable1 (..))
#endif

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
    body <-  select "body"
    label <- select "<div />"
    c <- select "<canvas id='theCanvas' width='180px' height='180px' />"
    appendJQuery c body
    cxt <- getContext =<< indexArray 0 (castRef c)
    setWidth  180 c
    setHeight 180 c
    appendJQuery label body
    keyEvent <- keyDownEvent body
    sync $ do
      let dirEvent = filterJust (flip lookup dic <$> keyEvent)
          updEvent = updater <$> dirEvent
      bhv <- accum (return bd) updEvent
      listen dirEvent $ \d ->
        void $ setText (T.pack $ show d) label
      listen (executeSyncIO $ value bhv) $ \b ->
        runLift $ runReader (drawBoard b) (Cxt cxt)
  return ()

updater :: Direction -> IO Board -> IO Board
updater dir bd0 = do
  bd_ <- bd0
  let bd = fst $ shift dir bd_
  if bd == bd_
    then return bd
    else do
    bd'  <- runLift $ runExc $ evalRandIO $ randomPlace bd  :: IO (Either ()Board)
    return $ either (const bd) id bd'

drawBoard :: (SetMember Lift (Lift IO) r, Member (Reader Cxt) r) => Board -> Eff r ()
drawBoard b = do
  readerT $ clearRect 0 0 180 180
  forM_ (withIndex b) $ \((j, i), mint) -> do
    readerT save
    readerT $ strokeStyle 0 0 0 1
    readerT $ strokeRect (fromIntegral i * 45) (fromIntegral j * 45) 40 40
    readerT $ fillStyle 0 0 0 1
    readerT $ maybe (const $ return ())
      (\t -> fillText (T.pack $ show t) (fromIntegral i*45+20) (fromIntegral j*45+20)) mint
    readerT $ restore

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
