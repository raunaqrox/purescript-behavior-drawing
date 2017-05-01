module Main where

import Prelude
import Control.Monad.Eff.Console
import FRP.Behavior
import FRP.Event
import Data.Maybe (Maybe(..), fromJust, maybe)
import Color (black, white)
import Graphics.Canvas (CANVAS, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Control.Parallel.Class (parallel, sequential)
import Control.Parallel (parTraverse_)
import Control.Monad.Cont.Trans (ContT(..), runContT)
import FRP
import Data.Set as Set
import FRP.Behavior.Keyboard (keys)
import FRP.Event.Time (interval)
import Partial.Unsafe (unsafePartial)
import Data.Traversable (sequence)
import Data.Foldable (foldMap)
import Graphics.Drawing (render, Drawing, filled, fillColor, rectangle, circle)
import Control.Monad.Eff
import Data.Int (toNumber)


foreign import move :: Event { x :: Int, y :: Int }

type Circle = { x :: Number, y :: Number, size :: Number }

position :: Behavior { x :: Number , y :: Number }
position = map (maybe {x:(-1.0), y:(-1.0)} id) (step Nothing (map Just (map (\o -> {x: toNumber o.x, y: toNumber o.y}) move)))

defaultSize :: Number
defaultSize = 10.0


scene :: { w :: Number, h :: Number } -> Behavior Drawing
scene { w, h } = pure background <> (drawPoint <$> position <*> keys) where
  background :: Drawing
  background = filled (fillColor white) (rectangle 0.0 0.0 w h)

  drawPoint :: {x :: Number, y :: Number} -> Set.Set Int -> Drawing
  drawPoint {x, y} inSet = case (Set.isEmpty inSet) of
                                true  -> filled (fillColor black) (rectangle x y 10.0 10.0)
                                false -> case (Set.member 32 inSet) of
                                              true  -> filled (fillColor black) (circle x y 10.0)
                                              false -> filled (fillColor white) (circle x y 10.0)
{-
scene2 :: { w :: Number, h :: Number } -> Behavior Drawing
scene2 { w, h } = (drawPoint <$> position <*> keys) where
  drawPoint :: {x :: Number, y :: Number} -> Set.Set Int -> Drawing
  drawPoint {x, y} inSet = case (Set.isEmpty inSet) of
                                true  -> filled (fillColor black) (rectangle x y 10.0 10.0)
                                false -> case (Set.member 32 inSet) of
                                              true  -> filled (fillColor black) (circle x y 10.0)
                                              false -> filled (fillColor white) (circle x y 10.0)
                                              -}

main :: forall eff. Eff (canvas :: CANVAS, frp :: FRP | eff) Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  _ <- setCanvasWidth w canvas
  _ <- setCanvasHeight h canvas
  sample_ (scene {w,h}) (interval 10) `subscribe` render ctx
--  runContT (parTraverse_ (\s -> ContT ((sample_ s (interval 60)) `subscribe` render ctx)) [scene {w,h} , scene2 {w,h}]) logShow



