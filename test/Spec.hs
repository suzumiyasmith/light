import Control.Lens
import Control.Arrow
import qualified Data.IntMap.Lazy as IM
import Linear
import Physics.Light

main :: IO ()
main = do
  print $ (detectCollision . fst) <$> (updateTest <$> [1 .. 1000] <*> [testWorld1, testWorld2])

testShape1 = [circle 1, ellipse 0.5 3]

testShape2 = polygon $ V2 <$> [-1, 1] <*> [-1, 1]

o1 = Object2D 0 (V3 0 0 1) 0 testShape1

o2 = Object2D (V3 1.5 0 0) (V3 0 0 0) 0 [testShape2]

o3 = Object2D (V3 (-1.5) 0 0) (V3 0 0 0) 0 [testShape2]

o4 = Object2D (V3 (-1.5) 0 0) (V3 1 0 0) 0 [testShape2]

testWorld1 = newPhysicsWorld & addObjects [o1, o2, o3]
testWorld2 = newPhysicsWorld & addObjects [o1, o3, o4]


updateTest :: Int -> PhysicsWorld -> PhysicsWorld
updateTest 0 w = w
updateTest n w = first (update 0.1) $ updateTest (n - 1) w
