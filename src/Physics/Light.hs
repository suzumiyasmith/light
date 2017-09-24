{-# LANGUAGE TemplateHaskell #-}

module Physics.Light where

import GJK

import Control.Arrow
import Control.Lens
import Control.Monad.Writer.Lazy
import Data.Fixed
import Data.Function
import qualified Data.IntMap.Lazy as IM
import Data.List
import Linear

data Object2D = Object2D
  { _position :: V3 Double
  , _velocity :: V3 Double
  , _acceleration :: V3 Double
  , _shape :: [Convex]
  }

makeLenses ''Object2D

instance Show Object2D where
  show o = show (o ^. position, o ^. velocity, o ^. acceleration)

type PhysicsWorld = (IM.IntMap Object2D, Int)

newPhysicsWorld :: PhysicsWorld
newPhysicsWorld = (IM.empty, 0)

addObject :: Object2D -> PhysicsWorld -> PhysicsWorld
addObject o (w, fresh) = (IM.insert fresh o w, fresh + 1)

addObjects :: [Object2D] -> PhysicsWorld -> PhysicsWorld
addObjects (o:os) w = addObjects os $ addObject o w
addObjects [] w = w

update :: Double -> PhysicsWorld -> PhysicsWorld
update dt = first (fmap w)
  where
    w :: Object2D -> Object2D
    w o = o
      & position %~ (+ o ^. velocity ^* dt)
      & velocity %~ (+ o ^. acceleration ^* dt)
      & position .  _z %~ (`mod'` (2 * pi))

detectCollision :: PhysicsWorld -> [(Int, Int)]
detectCollision (w, _) =
  execWriter $ IM.traverseWithKey (\k o -> tell $ fmap ((,) k) (detect w k o)) w

detect :: IM.IntMap Object2D -> Int -> Object2D -> [Int]
detect w k1 o1 =
  IM.keys $
  IM.filterWithKey (\k2 o2 -> k1 < k2 && (shapeCollide `on` realShape) o1 o2) w
  where
    realShape o =
      rotateConvex (o ^. position ^. _z) . moveConvex (o ^. position ^. _xy) <$>
      o ^. shape

shapeCollide :: [Convex] -> [Convex] -> Bool
shapeCollide v1 v2 = and $ convexIntersect <$> v1 <*> v2

ellipse :: Double -> Double -> Convex
ellipse a b = Convex $ \d -> V2 (a * cos d) (b * sin d)

circle :: Double -> Convex
circle r = ellipse r r

polygon :: [V2 Double] -> Convex
polygon vs = Convex $ \d -> maximumBy (compare `on` dot (angle d)) vs

moveConvex :: V2 Double -> Convex -> Convex
moveConvex p s = Convex $ \d -> p + support s d

rotateConvex :: Double -> Convex -> Convex
rotateConvex r s = Convex $ \d -> rotateMatrix2D r !* support s d

scaleConvex :: V2 Double -> Convex -> Convex
scaleConvex c s = Convex $ \d -> scaled c !* support s d

rotateMatrix2D :: Double -> V2 (V2 Double)
rotateMatrix2D r = V2 (V2 (cos r) (-sin r)) (V2 (sin r) (cos r))