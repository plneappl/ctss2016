-- cabal install vector-space
import System.Random
import Data.List

type Z10_2 = [Z2]
data Z2 = Zero | One deriving(Show, Enum, Bounded, Eq)
instance Num Z2 where
  Zero + Zero   = Zero
  One  + One    = Zero
  _    + _      = One
  One  * One    = One
  _    * _      = Zero
  abs x         = x
  signum Zero   = Zero
  signum One    = One
  fromInteger 0 = Zero
  fromInteger _ = One
  negate x      = x

toInt :: Z2 -> Int
toInt Zero = 0
toInt One  = 1

r :: StdGen -> [Z2]
r sg = map fromInteger (randomRs (0, 1) sg)

vecs :: [Z2] -> [Z10_2]
vecs zs = 
  let v1  = take 10 zs
      zs' = drop 10 zs in
  v1 : vecs zs'

rVecs :: StdGen -> [Z10_2]
rVecs = vecs . r

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)
tuplify2 []     = error "Too few items!"
tuplify2 [_]    = error "Too few items!"
tuplify2  _     = error "Too many items!"

-- https://wiki.haskell.org/99_questions/Solutions/26
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

subsets :: [a] -> [[a]]
subsets xs = concatMap (`combinations` xs) [0..(length xs)]

subsets' :: [a] -> [[a]]
subsets' xs = filter (\ ys -> length ys >= 2) $ subsets xs

linearcombination :: [Z10_2] -> Z10_2
linearcombination = foldr (zipWith (+)) (take 10 zeros)

containsNoDuplicates :: Eq a => [a] -> Bool
containsNoDuplicates as = all (<= 1) $ map length $ group as

isVectorSpace :: [Z10_2] -> Bool
isVectorSpace vs = 
  let subs = subsets vs
      lcs  = map linearcombination subs in
  -- check for 1*v1+1*v2      check for v1 == v2   check for (0, ..., 0)
  containsNoDuplicates lcs

vectorSpaces :: [Z10_2] -> [[Z10_2]]
vectorSpaces vs = 
  let tvs  = take 4 vs
      vs'  = drop 4 vs
      tvs' = [tvs | isVectorSpace tvs] in
      tvs' ++ vectorSpaces vs'
  
zeros :: [Z2]
zeros = Zero : zeros

ones  :: [Z2]
ones  = One : ones

dist :: Z10_2 -> Z10_2 -> Int
dist v1 v2 = sum $ map toInt $ zipWith (+) v1 v2

minDist :: [Z10_2] -> Int
minDist vs = minimum $ map (uncurry dist . tuplify2) $ combinations 2 vs

main :: IO()
main = do
  rgen <- getStdGen
  let rVecs' = rVecs rgen
  let tries  = 10000
  let vs     = take tries $ vectorSpaces rVecs'
  let dists  = map (\ vs' -> (vs', minDist vs')) vs
  let dists' = map snd dists
  let avg    = fromIntegral (sum dists') / fromIntegral (length dists')
  let best   = maximumBy (\ (vs1, d1) (vs2, d2) -> compare d1 d2) dists
  print $ "Average distance: " ++ show avg
  print $ "Best code: " ++ show best

  
