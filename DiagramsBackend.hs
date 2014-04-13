{-# LANGUAGE CPP, FlexibleContexts, ForeignFunctionInterface  #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings                                #-}
module DiagramsBackend (drawBoard, renderBoard) where
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
import           Diagrams.Prelude         (Any, Diagram, Path, QDiagram, R2)
import           Diagrams.Prelude         (Renderable, SizeSpec2D (..), atop)
import           Diagrams.Prelude         (fc, hcat', lc, lw, renderDia, sep)
import           Diagrams.Prelude         (square, stroke, vcat', ( # ))
import           Graphics.SVGFonts
import           JavaScript.JQuery        (JQuery, setHeight, setHtml, setWidth)
import           JavaScript.JQuery        (replaceWith)
import           Puzzle
import           Text.Blaze.Renderer.Text (renderMarkup)

sqSize :: Double
sqSize = 100

sqMargin :: Double
sqMargin = 5

canvasSize :: Double
canvasSize = sqMargin*3 + sqSize*4

renderBoard :: Board -> JQuery -> IO JQuery
renderBoard bd el = do
  let opts = SVGOptions Absolute Nothing
      svg = T.toStrict $ renderMarkup $  renderDia SVG opts $ drawBoard $ bd
  replaceWith svg el
  setWidth  canvasSize el
  setHeight canvasSize el

drawBoard :: Renderable (Path R2) b => Board -> QDiagram b R2 Any
drawBoard =
  vcat' (def & sep .~ sqMargin) .
  map (hcat' (def & sep .~ sqMargin) . map drawCell) .
  toLists

drawCell :: Renderable (Path R2) b => Maybe Int -> QDiagram b R2 Any
drawCell mint =
  let num  = maybe mempty (myText . show) mint
      cell = square sqSize # fc (calcHue mint) # lc C.black # lw 1.0
  in num `atop` cell

myText :: Renderable (Path R2) b  => String -> Diagram b R2
myText s = stroke (textSVG' $ TextOpts s bit INSIDE_W KERN False (sqSize*0.5) (sqSize*0.5))
           # fc C.black

calcHue :: Maybe Int -> C.Colour Double
calcHue Nothing = C.white
calcHue (Just t) =
  let Color r g b _ = red & _Hue .~ 360 * (logBase 2 (fromIntegral t) - 1) / 10
  in rgb (realToFrac r) (realToFrac g) (realToFrac b)

