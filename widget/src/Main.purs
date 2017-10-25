module Main where

import Prelude (class Ord, class Show, Unit, bind, const, discard, id, map, negate, otherwise, pure, unit, void, ($), (*), (+), (-), (/), (<), (<$>), (<*>), (<<<), (<>), (==), (>), (>>=))
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM, randomRange)
import Control.Monad.Except (runExcept)
import Conditional (ifelse)
import Data.Either (either)
import Data.Foreign (readString, toForeign)
import Data.Foreign.Index (index)
import Data.Foldable (class Foldable, maximum, sum, for_)
import Data.Generic (class Generic, gShow)
import Data.Int (hexadecimal, round, toNumber, toStringAs)
import Data.Array ((:), (..), null)
import Data.Maybe (fromJust)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Partial.Unsafe (unsafePartial)
import React (Event, ReactClass, ReactState, ReactThis, ReadWrite, createClass, createFactory, readState, spec, writeState)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, arc, fillPath, getCanvasDimensions, setCanvasDimensions, getCanvasElementById, getContext2D, rect, setFillStyle, withContext, fillRect)
import Math as Math

    
-- Truth corresponds to the reality which the data will measure

newtype Truth = Truth
   { location :: { x :: Number, y :: Number }
   }

derive instance genericTruth :: Generic Truth

instance showTruth :: Show Truth where
   show = gShow


defTruth :: Truth                 
defTruth = Truth { location: { x: 0.5, y: 0.1 } }

randomTruth :: forall eff. Eff (random :: RANDOM | eff) Truth
randomTruth = do
                 x <- randomRange 0.25 0.75
                 y <- randomRange 0.05 0.50
                 pure $ Truth { location: { x: x, y: y } }

-- paint datum s, centered on the given coordinates
-- the result should be smaller the spacing (defined in calcDatumGrid) 
paintTruth :: forall eff. String
              -> Truth
              -> Eff ( canvas :: CANVAS | eff) Unit
paintTruth canvasId (Truth { location: l } ) = void $ do
                 canvas <- unsafeGetCanvas canvasId

                 dims   <- getCanvasDimensions canvas

                 ctx <- getContext2D canvas
                                                 
                 _ <- setFillStyle "#000000" ctx
                 _ <- fillPath ctx $ circle ctx { x: dims.width * l.x, y: dims.height * (1.0 - l.y), r: 7.5 }
                 _ <- setFillStyle "#8080ff" ctx
                 fillPath ctx $ circle ctx { x: dims.width * l.x, y: dims.height * (1.0 - l.y), r: 5.0 }
                     
                 
-- Datum corresponds to a single reading (which might be an image)

newtype Datum = Datum {
      location :: Number
      , jitter   :: Number
    }

derive instance genericDatum :: Generic Datum

instance showDatum :: Show Datum where
   show = gShow

-- x is position (on [0,1])
newDatum :: Truth -> Number -> Number -> Datum
newDatum t x j = Datum { location: x, jitter: j }

-- theta is angle in radians                     
newDatumAngle :: Truth -> Number -> Number -> Datum
newDatumAngle t@(Truth { location: l }) theta j = newDatum t x j
   where x = l.x + l.y * Math.tan theta

--
-- random mutators
--
rndDatum :: forall eff.Truth -> Eff (random :: RANDOM | eff) Datum
rndDatum tt@(Truth t) = (newDatumAngle tt) <$> (randomRange theta0 theta1)
                                           <*> (randomRange (-1.0) 1.0)
    where theta0 = -0.5 * Math.pi -- Math.atan2 (1.0 - t.location.x) t.location.y
          theta1 =  0.5 * Math.pi -- Math.atan2 (0.0 - t.location.x) t.location.y

-- paint datum s, centered on the given coordinates
-- the result should be smaller the spacing (defined in calcDatumGrid) 
paintData :: forall eff. String
              -> Array Datum 
              -> Eff ( canvas :: CANVAS | eff) Unit
paintData canvasId theData = void $ do
                 canvas <- unsafeGetCanvas canvasId
                 fillCanvas canvas "#404080"                     

                 dims   <- getCanvasDimensions canvas
                 let y0 = 0.5 * dims.height                           

                 ctx <- getContext2D canvas

                 for_ theData \(Datum s) -> do
                     let x  = s.location * dims.width
                     let x' = clamp markRadius (dims.width - markRadius) x

                     let colour = if x' == x then "#ffff00" else "#ff0000"

                     _ <- setFillStyle colour ctx                      
                     void $ fillPath ctx $ circle ctx { x: x'
                                                      , y: y0 + s.jitter * jitterSize
                                                      , r: markRadius }
    where jitterSize = 10.0
          markRadius =  5.0
                     
--
-- application state
--
-- AppState is the main type

-- is Array the right choice here ? Seq ?               
newtype AppState = AppState
    { truth   :: Truth
    , theData :: Array Datum
    }

derive instance genericAppState :: Generic AppState

instance showAppState :: Show AppState where
   show = gShow

initialState :: AppState
initialState = AppState
               { truth:   defTruth
               , theData: []
               }

newRandomState :: forall eff. Eff (random :: RANDOM | eff) AppState
newRandomState = do
                   t <- randomTruth
                   pure $ AppState { truth: t, theData: [] } 

--
-- state mutators: should I use a lens here ?
--
addDatum :: AppState -> Datum -> AppState
addDatum (AppState as) s = AppState $ as { theData = s : as.theData }

addRndDatum :: forall eff. AppState -> Eff (random :: RANDOM | eff) AppState
addRndDatum state@(AppState as) = (addDatum state) <$> (rndDatum as.truth)

clearTheData :: AppState -> AppState
clearTheData (AppState as) = AppState $ as { theData = [] }

--
-- drawing
--
paintState :: forall eff. String -> String -> AppState
                          -> Eff (canvas :: CANVAS | eff) Unit                    
paintState canvasId posteriorId (AppState state) = do
                 let logPostGrid = gridFn nx ny (logPost state.theData)
                 let maxlogPost  = unsafeMaximum $ map (\r -> r.f) logPostGrid
                 let postGrid    = mapGrid (scaleLogPost maxlogPost) logPostGrid
                 

                 dcanvas <- unsafeGetCanvas canvasId
                 void $ setCanvasDimensions { height: dHeight, width: width } dcanvas

                 pcanvas <- unsafeGetCanvas posteriorId
                 void $ setCanvasDimensions { height: width, width: width } pcanvas

                 let dx = 1.0 / toNumber ny
                 let dy = 1.0 / toNumber nx

                 -- if we have no data, just clear the posterior rather than paint
                 -- the natural colour i.e. yellow.
                 ifelse (null state.theData)
                        (clearPost posteriorId)
                        (paintRects posteriorId dx dy postGrid heatCMap)
                          
                 paintTruth posteriorId state.truth
                 paintData canvasId state.theData
    where nx = 100
          ny = 100
          width = 800.0 -- canvas with: will be scaled by browser
          dHeight = 50.0

clearPost :: forall eff. String -> Eff (canvas :: CANVAS | eff) Unit                    
clearPost canvasId = void $ do
                       canvas <- unsafeGetCanvas canvasId
                       fillCanvas canvas "#000000"
                    
--
-- Calculate fn at midpoints of nx x ny grid on [0,1] x [0,1]
--
gridFn :: Int -> Int -> (Number -> Number -> Number)
               -> Array { x :: Number, y :: Number, f :: Number }
gridFn nx ny fn = do
              iy <- 0 .. (ny - 1)
              let y = (0.5 + toNumber iy) / (toNumber ny)
              ix <- 0 .. (nx - 1)
              let x = (0.5 + toNumber ix) / (toNumber nx)
              pure $ { x: x, y: y, f: fn x y }
                           
paintRects :: forall eff. 
                String
                -> Number -> Number 
                -> Array { x :: Number, y :: Number, f :: Number }
                -> (Number -> { r :: Number, g :: Number, b :: Number })
                -> Eff (canvas :: CANVAS | eff) Unit                    
paintRects canvasId dx dy rects shader = void $ do
                 canvas <- unsafeGetCanvas canvasId
                 dims   <- getCanvasDimensions canvas
                                 
                 ctx <- getContext2D canvas

                 let rectx = dims.width  * dx
                 let recty = dims.height * dy

                 -- using for_ here leads to stack space issues!
                 foreachE rects \{ x: x, y: y, f: f} -> do
                     let xr  = x         * dims.width  - 0.5 * rectx
                     let yr  = (1.0 - y) * dims.height - 0.5 * recty

                     let rgb = shader f

                     _ <- setFillStyle (calcStyleRGB rgb) ctx
                     void $ fillRect ctx { x: xr,  y: yr, h: recty, w: rectx }
                   
                                  
--
-- These calculate posterior, scaled so that 1.0 is MAP and
-- 0.0 is prob = 0
--
scaleLogPost :: Number -> Number -> Number
scaleLogPost m x = Math.exp $ x - m
                                  
logPost :: Array Datum -> Number -> Number -> Number
logPost theData x y = sum $ map (\d -> logPostDatum d x y) theData

logPostDatum :: Datum -> Number -> Number -> Number
logPostDatum (Datum s) x y = Math.log $ y / (y * y + dx * dx)
   where dx = x - s.location
              
mapGrid :: (Number -> Number)
             -> Array { x :: Number, y :: Number, f :: Number }
             -> Array { x :: Number, y :: Number, f :: Number }
mapGrid g = map g'
   where g' { x: x, y: y, f: f } = { x: x, y: y, f: g f }


              
--
-- application code follows
--
dataComponent :: forall props. ReactClass props
dataComponent = createClass dSpec
    where canvasId    = "GLHdCanvas"
          posteriorId = "GLHpostCanvas"
          buttonId    = "GLHbuttonBar"
          flashId     = "GLHbuttonFlash"
          resetId     = "GLHbuttonReset"
                                   
          dSpec = (spec initialState rend)
                  { componentDidUpdate = \ctx _ _ -> readState ctx >>= paintState canvasId posteriorId
                  , componentDidMount  = \ctx     -> readState ctx >>= paintState canvasId posteriorId
                  }

          rend ctx = do 
                       state <- readState ctx
                       pure $
                          D.div [ P.className "container" ]
                                [ D.canvas [ P._id posteriorId ] []          
                                , D.canvas [ P._id canvasId ] []
                                , D.div    [ P._id buttonId, P.className "container" ] 
                                           [ D.button [ P.className "getData", P._id resetId
                                                      , P.onClick (\_ -> effMutateState ctx (\_ -> newRandomState)) ]
                                                      [ D.text "Reset" ]
                                           , D.button [ P.className "getData", P._id flashId
                                                      , P.onClick (\_ -> effMutateState ctx addRndDatum) ]
                                                      [ D.text "Flash!" ]
                                           ]
                                ]
main :: Eff (console :: CONSOLE
            , dom    :: DOM
            , random :: RANDOM
            , canvas :: CANVAS
            ) Unit
main = do
          genericMain dataComponent (ElementId "data")

------------------------------------------------------------------------
       
-- Utility stuff below

-- a bit like updateState, but the mutator has type state -> Eff (..) state
effMutateState :: forall state eff props.                 
                      ReactThis props state                     
                      -> (state -> Eff (state :: ReactState ReadWrite | eff) state)
                      ->           Eff (state :: ReactState ReadWrite | eff) state
effMutateState ctx f = (readState ctx) >>= f >>= (writeState ctx)

-- abstract away all the boilerplate
-- is there an implicit constraint that the element specified by eltId is special ?
genericMain :: forall eff. ReactClass Unit -> ElementId -> Eff (dom :: DOM, canvas :: CANVAS | eff) Unit
genericMain cmpnt mainId = do
  let component = D.div [] [ createFactory cmpnt unit ]

  doc <- window >>= document
  ctr <- getElementById mainId (documentToNonElementParentNode (htmlDocumentToDocument doc))

  _ <- render component (unsafePartial fromJust ctr)

  pure unit

------------------------------------------------------------------------
       

-- Missing/helpful graphics primitives
       
-- get the canvas from Eff, assuming it will all work
unsafeGetCanvas :: forall eff. String -> Eff (canvas :: CANVAS | eff) CanvasElement
unsafeGetCanvas canvasId = do
                                 maybeCanvas <- getCanvasElementById canvasId
                                 pure $ unsafePartial fromJust maybeCanvas

-- why isn't this in the class
circle :: forall eff props.
            Context2D
            -> { x :: Number, y :: Number, r :: Number | props }
            -> Eff (canvas :: CANVAS | eff) Context2D
circle ctx { x: x, y: y, r: r } = arc ctx { r: r, x: x, y:y, start: 0.0, end: Math.pi * 2.0 }

-- cls
fillCanvas :: forall eff.
                CanvasElement -> String                 
                -> Eff ( canvas :: CANVAS | eff) Unit
fillCanvas canvas style = void $ do
                            d     <- getCanvasDimensions canvas
                            ctx <- getContext2D canvas
                                     
                            withContext ctx $ do 
                                   _ <- setFillStyle style ctx
                                   fillPath ctx $ rect ctx { x: 0.0, y: 0.0, w: d.width, h: d.height }


                                
                                 
-- given a scalar on [0,1] return a RGB record (unclamped)
--
-- based on the gist_heat colourmap from matplotlib
-- but with the blue toned down (4.0 -> 3.5)
heatCMap :: Number -> { r :: Number, g :: Number, b :: Number }
heatCMap x = { r: 1.5 * x, g: 2.0 * x - 1.0, b: 3.5 * x - 3.0 }
                                 
--
-- Given r,g,b all on [0,1] return a #rrggbb style
-- Colour components are clamped to [0,1]
--
calcStyleRGB :: { r :: Number, g :: Number, b :: Number } -> String
calcStyleRGB { r: r, b: b, g: g } = "#" <> (asHex r) <> (asHex g) <> (asHex b)

asHex :: Number -> String
asHex x = asHexInt $ round $ (clamp 0.0 1.0 x) * 255.0 

-- printf "%02x"                      
asHexInt :: Int -> String                      
asHexInt i | i < 16    = "0" <> toStringAs hexadecimal i
           | otherwise =        toStringAs hexadecimal i
           
--                                           
-- extracting a value from an event, returning the supplied default
-- in case of error
--
valueOf :: String -> Event -> String
valueOf def e = either (const def) id $ runExcept do
  target <- index (toForeign e) "target"
  value <- index target "value"
  readString value
          
------------------------------------------------------------------------

log10 :: Number -> Number
log10 x = (Math.log x) / (Math.log 10.0)

clamp :: Number -> Number -> Number -> Number
clamp a b x | x < a     = a
            | x > b     = b
            | otherwise = x

unsafeMaximum :: forall a f. Ord a => Foldable f => f a -> a
unsafeMaximum = unsafePartial fromJust <<< maximum                          

                
                
