-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import Expr
import GHC.Float
import qualified Graphics.UI.Threepenny as UI

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

origScale = 0.04

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     diff    <- mkButton "Differentiate"      -- The diff button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     btns    <- row [pure draw, pure diff]
     getBody window #+ [column [pure canvas,pure formula,pure btns]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]
     pure btns  # set style [("align","center")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input canvas
     on valueChange' input $ \ _ -> readAndDraw input canvas
     on UI.click     diff  $ \ _ -> readAndDiff input canvas


readAndDraw :: Element -> Canvas -> UI ()
readAndDraw input canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     case readExpr formula of 
                            (Just exp) -> do
                                              path "blue" (points exp origScale (canHeight,canHeight)) canvas
                                              UI.fillText ((showExpr . simplify ) exp) (10,canHeight/2) canvas

                            _ -> UI.fillText "WRONG" (10,canHeight/2) canvas


readAndDiff :: Element -> Canvas -> UI ()
readAndDiff input canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     -- Clear the canvas
     --clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     case readExpr formula of 
                            (Just exp) -> do
                                              path "red" (points (differentiate exp) origScale (canHeight,canHeight)) canvas
                                              UI.fillText ((showExpr . simplify ) exp) (10,canHeight/2) canvas

                            _ -> UI.fillText "WRONG" (10,canHeight/2) canvas                            
type Point = (Double, Double)

points :: Expr          -- An expression 
        -> Double       -- A scaling value
        -> (Int,Int)    -- The width and height of the drawing area
        -> [Main.Point]
points exp scale (width, height) = [(pixel, cartesianToPixel (eval exp (pixelToCartesian pixel))) | pixel <- map fromIntegral [0..width]]
    where
      -- converts a pixel x-coordinate to a cartesian x-coordinate
      pixelToCartesian :: Double -> Double
      pixelToCartesian x = x*scale - (fromIntegral width * scale) / 2

      -- converts a cartesian y-coordinate to a pixel y-coordinate
      cartesianToPixel :: Double -> Double
      cartesianToPixel y = (y - (fromIntegral height * scale) / 2) / ((-1) * scale)



