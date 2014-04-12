{-# LANGUAGE CPP, FlexibleContexts, ForeignFunctionInterface  #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings                                #-}
module DiagramsBackend (renderBoard) where
import           Control.Lens             ((&), (.~))
import           Data.Color               (Color (..), _Hue)
import           Data.Color.Names
import qualified Data.Colour              as C
import qualified Data.Colour.Names        as C
import           Data.Colour.SRGB.Linear  (rgb)
import           Data.Default             (def)
import           Data.Monoid              (mempty)
import qualified Data.Text.Lazy           as T
import           Diagrams.Backend.SVG
import           Diagrams.Prelude         (Any, Path, QDiagram, R2, Renderable)
import           Diagrams.Prelude         (SizeSpec2D (..), atop, fc, hcat', lc)
import           Diagrams.Prelude         (lw, renderDia, sep, square, text)
import           Diagrams.Prelude         (vcat', ( # ))
import qualified Diagrams.TwoD.Text       as D
import           Puzzle
import           Text.Blaze.Renderer.Text (renderMarkup)

import JavaScript.JQuery (setHtml)
import JavaScript.JQuery (JQuery)

sqSize :: Double
sqSize = 100

sqMargin :: Double
sqMargin = 5

canvasSize :: Double
canvasSize = sqMargin*3 + sqSize*4

renderBoard :: Board -> JQuery -> IO JQuery
renderBoard bd el =
   let opts = SVGOptions Absolute Nothing
       svg = T.toStrict $ renderMarkup $  renderDia SVG opts $ drawBoard $ bd
   in setHtml svg el

drawBoard :: (Renderable D.Text b, Renderable (Path R2) b)
          => Board -> QDiagram b R2 Any
drawBoard =
  vcat' (def & sep .~ sqMargin) .
  map (hcat' (def & sep .~ sqMargin) . map drawCell) .
  toLists

drawCell :: (Renderable D.Text b, Renderable (Path R2) b)
         => Maybe Int -> QDiagram b R2 Any
drawCell mint =
  let num  = maybe mempty (text . show) mint
      cell = square sqSize # fc (calcHue mint) # lc C.black # lw 1.0
  in num `atop` cell

calcHue :: Maybe Int -> C.Colour Double
calcHue Nothing = C.white
calcHue (Just t) =
  let Color r g b _ = red & _Hue .~ 360 * (logBase 2 (fromIntegral t) - 1) / 10
  in rgb (realToFrac r) (realToFrac g) (realToFrac b)

