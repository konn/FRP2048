{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction, TypeOperators, CPP               #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif
module Drawer (Context(..), Canvas, TextAlign(..), TextBaseline(..), LineCap(..),
               LineJoin(..), getContext, save, restore, scale, rotate,
               translate, transform, setTransform, fill, fillRule, stroke,
               beginPath, closePath, clip, moveTo, lineTo, quadraticCurveTo,
               bezierCurveTo, arc, arcTo, rect, isPointInPath, fillStyle,
               strokeStyle, globalAlpha, lineJoin, lineCap, lineWidth,
               setLineDash, lineDashOffset, miterLimit, fillText, strokeText,
               font, measureText, textAlign, textBaseline, fillRect, strokeRect,
               clearRect, drawing, draw, locally, setStyleHeight, setStyleWidth
              ) where

import           Control.Eff               ((:>), Eff, Member, SetMember)
import           Control.Eff.Lift          (Lift, lift, runLift)
import           Control.Eff.Reader.Strict (Reader, ask, runReader)
import           Control.Monad             (liftM)
import           Data.Text                 (Text)
import           Data.Typeable             (Typeable)
import           GHCJS.Marshal             (ToJSRef)
import           JavaScript.Canvas         (Canvas, LineCap, LineJoin)
import           JavaScript.Canvas         (TextAlign, TextBaseline)
import qualified JavaScript.Canvas         as Cv

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

drawing :: (SetMember Lift (Lift IO) r, Member (Reader Context) r)
        => (Cv.Context -> IO b) -> Eff r b
drawing f = lift . f . runContext =<< ask
{-# INLINE drawing #-}

draw :: Context -> Eff (Reader Context :> Lift IO :> ()) a -> IO a
draw cxt act = runLift $ runReader act cxt
{-# INLINE draw #-}

locally :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Eff r a -> Eff r ()
locally act = save >> act >> restore
{-# INLINE locally #-}

newtype Context = Context { runContext :: Cv.Context } deriving Typeable

getContext :: Canvas -> IO Context
getContext = liftM Context . Cv.getContext
{-# INLINE getContext #-}

save :: (SetMember Lift (Lift IO) r, Member (Reader Context) r)
     => Eff r ()
save = drawing Cv.save
{-# INLINE save #-}

restore :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Eff r ()
restore = drawing Cv.restore
{-# INLINE restore #-}

scale :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Eff r ()
scale a b = drawing $ Cv.scale a b
{-# INLINE scale #-}

rotate :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Eff r ()
rotate rad = drawing $ Cv.rotate rad
{-# INLINE rotate #-}

translate :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Eff r ()
translate = (drawing .) .Cv.translate
{-# INLINE translate #-}

transform :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Double -> Double -> Double -> Double -> Eff r ()
transform a b c d e f = drawing $ Cv.transform a b c d e f
{-# INLINE transform #-}

setTransform :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Double -> Double -> Double -> Double -> Eff r ()
setTransform a b c d e f = drawing $ Cv.setTransform a b c d e f
{-# INLINE setTransform #-}

fill :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Eff r ()
fill = drawing Cv.fill
{-# INLINE fill #-}

fillRule :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Text -> Eff r ()
fillRule = drawing . Cv.fillRule
{-# INLINE fillRule #-}

stroke :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Eff r ()
stroke = drawing Cv.stroke
{-# INLINE stroke #-}

beginPath :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Eff r ()
beginPath = drawing Cv.beginPath
{-# INLINE beginPath #-}

closePath :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Eff r ()
closePath = drawing Cv.closePath
{-# INLINE closePath #-}

clip :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Eff r ()
clip = drawing Cv.clip
{-# INLINE clip #-}

moveTo :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Eff r ()
moveTo = (drawing .) . Cv.moveTo
{-# INLINE moveTo #-}

lineTo :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Eff r ()
lineTo = (drawing .) . Cv.lineTo
{-# INLINE lineTo #-}

quadraticCurveTo :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Double -> Double -> Eff r ()
quadraticCurveTo a b c d = drawing $ Cv.quadraticCurveTo a b c d
{-# INLINE quadraticCurveTo #-}

bezierCurveTo :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Double -> Double -> Double -> Double -> Eff r ()
bezierCurveTo a b c d e f = drawing $ Cv.bezierCurveTo a b c d e f
{-# INLINE bezierCurveTo #-}

arc :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Double -> Double -> Double -> Bool -> Eff r ()
arc a b c d e f = drawing $ Cv.arc a b c d e f
{-# INLINE arc #-}

arcTo :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Double -> Double -> Double -> Eff r ()
arcTo a b c d e = drawing $ Cv.arcTo a b c d e
{-# INLINE arcTo #-}

rect :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Double -> Double -> Eff r ()
rect a b c d = drawing $  Cv.rect a b c d
{-# INLINE rect #-}

isPointInPath :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Eff r ()
isPointInPath = (drawing .) . Cv.isPointInPath
{-# INLINE isPointInPath #-}

fillStyle :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Int -> Int -> Int -> Double -> Eff r ()
fillStyle a b c d = drawing $ Cv.fillStyle a b c d
{-# INLINE fillStyle #-}

strokeStyle :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Int -> Int -> Int -> Double -> Eff r ()
strokeStyle a b c d = drawing $ Cv.strokeStyle a b c d
{-# INLINE strokeStyle #-}

globalAlpha :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Eff r ()
globalAlpha = drawing . Cv.globalAlpha
{-# INLINE globalAlpha #-}

lineJoin :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => LineJoin -> Eff r ()
lineJoin = drawing . Cv.lineJoin
{-# INLINE lineJoin #-}

lineCap :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => LineCap -> Eff r ()
lineCap  = drawing . Cv.lineCap
{-# INLINE lineCap #-}

lineWidth :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Eff r ()
lineWidth = drawing . Cv.lineWidth
{-# INLINE lineWidth #-}

setLineDash :: (Num a, ToJSRef a, SetMember Lift (Lift IO) r, Member (Reader Context) r) => [a] -> Eff r ()
setLineDash = drawing . Cv.setLineDash
{-# INLINE setLineDash #-}

lineDashOffset :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Eff r ()
lineDashOffset = drawing . Cv.lineDashOffset
{-# INLINE lineDashOffset #-}

miterLimit :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Eff r ()
miterLimit = drawing . Cv.miterLimit
{-# INLINE miterLimit #-}

fillText :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Text -> Double -> Double -> Eff r ()
fillText a b c = drawing $ Cv.fillText a b c
{-# INLINE fillText #-}

strokeText :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Text -> Double -> Double -> Eff r ()
strokeText a b c = drawing $ Cv.strokeText a b c
{-# INLINE strokeText #-}

font :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Text -> Eff r ()
font = drawing . Cv.font
{-# INLINE font #-}

measureText :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Text -> Eff r Double
measureText = drawing . Cv.measureText
{-# INLINE measureText #-}

textAlign :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => TextAlign -> Eff r ()
textAlign = drawing . Cv.textAlign
{-# INLINE textAlign #-}

textBaseline :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => TextBaseline -> Eff r ()
textBaseline = drawing . Cv.textBaseline
{-# INLINE textBaseline #-}

fillRect :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Double -> Double -> Eff r ()
fillRect a b c d = drawing $ Cv.fillRect a b c d
{-# INLINE fillRect #-}

strokeRect :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Double -> Double -> Eff r ()
strokeRect a b c d = drawing $ Cv.strokeRect a b c d
{-# INLINE strokeRect #-}

clearRect :: (SetMember Lift (Lift IO) r, Member (Reader Context) r) => Double -> Double -> Double -> Double -> Eff r ()
clearRect a b c d = drawing $ Cv.clearRect a b c d
{-# INLINE clearRect #-}
