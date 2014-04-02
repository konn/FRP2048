{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances                                         #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind -fno-warn-orphans #-}
module Main where
import           Control.Applicative       ((<$>))
import           Control.Eff
import           Control.Eff.Fresh         (Fresh, fresh, runFresh)
import           Control.Eff.Lift          (Lift, lift, runLift)
import           Control.Eff.Random
import           Control.Eff.Reader.Strict (Reader, ask, runReader)
import           Control.Monad             (forM_, void, (<=<))
import           Data.Default
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Typeable             (Typeable (..))
import           FRP.Sodium
import qualified FRP.Sodium                as FRP
import           GHCJS.Foreign
import           GHCJS.Types
import           JavaScript.Canvas
import           JavaScript.JQuery         hiding (not)
import           Puzzle

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
main = runLift $ flip runFresh (0 :: Int) $ do
  body  <- lift $ select "body"
  label <- lift $ select "<div />"
  c <- lift $ select "<canvas id='theCanvas' />"
  lift $ appendJQuery c body
  cxt <- lift $ getContext =<< indexArray 0 (castRef c)
  bd <- evalRandIO newBoard
  runReader (drawBoard bd) (Cxt cxt)
  chk <- reactiveCheckbox body "disabled"
  lift $ do
    appendJQuery label body
    event <- keyDownEvent body
    sync $ listen (flip lookup dic <$> event `gate` (not <$> chk)) $ \dir -> do
      void $ setText (T.pack $ show dir) label
  return ()

drawBoard :: (SetMember Lift (Lift IO) r, Member (Reader Cxt) r) => Board -> Eff r ()
drawBoard b = do
  forM_ (withIndex b) $ \((i, j), mint) -> do
    readerT save
    readerT $ strokeStyle 0 0 0 1
    readerT $ strokeRect (fromIntegral i * 41)
      (fromIntegral j * 41)
      (fromIntegral i * 41 + 40) (fromIntegral j*41 + 40)
    readerT $ fillStyle 0 0 0 1
    readerT $ maybe (const $ return ())
      (\t -> fillText (T.pack $ show t) (fromIntegral i*41+20) (fromIntegral j*41+20)) mint
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
