import Data.Matrix
import qualified Data.Set as DS

-- Generator matrix G
g1 :: Matrix Int 
g1 = fromList 4 7 [1, 0, 0, 0, 2, 1, 2, 0, 1, 0, 0, 1, 2, 2, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 2, 2, 0]
-- Generator matrix G^⊥
g2 :: Matrix Int
g2 = fromList 3 7 [2, 1, 0, 2, 1, 0, 0, 1, 2, 1, 2, 0, 1, 0, 2, 2, 1, 0, 0, 0, 1]

-- all vectors in C as a Set of Lists of Ints
vs1 = DS.fromList [map (`mod` 3) (toList $ v*g1) | x1 <- [0..2], x2 <- [0..2], x3 <- [0..2], x4 <- [0..2], let v = fromList 1 4 [x1, x2, x3, x4]]
-- all vectors in C^⊥ as a Set of Lists of Ints
vs2 = DS.fromList [map (`mod` 3) (toList $ v*g2) | x1 <- [0..2], x2 <- [0..2], x3 <- [0..2], let v = fromList 1 3 [x1, x2, x3]]

-- prints
-- fromList [[0,0,0,0,0,0,0]]
-- i.e. the vector (0..0)
main :: IO()
main = print (vs1 `DS.intersection` vs2)