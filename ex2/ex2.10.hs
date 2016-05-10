-- cabal install vector-space
import System.Random
import Data.List

-- this is Z_2 with addition and multiplication, implementing Num by hand.
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

-- for distance
toInt :: Z2 -> Int
toInt Zero = 0
toInt One  = 1

-- actually this is Z*_2, but writing 10-tuples is so cumbersome...
type Z10_2 = [Z2]

-- How to do random in Haskell:
r :: StdGen -> [Z2]
r sg = map fromInteger (randomRs (0, 1) sg)

vecs :: [Z2] -> [Z10_2]
vecs zs = 
  let v1  = take 10 zs
      zs' = drop 10 zs in
  v1 : vecs zs'

rVecs :: StdGen -> [Z10_2]
rVecs = vecs . r

-- list of two to a 2-tuple
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

-- P(M) = [[], [m_1], [m_2], ..., [m_1, m_2], [m_1, m_3], ...]
-- ==> M choose 0, M choose 1, ..., M choose |M|.
subsets :: [a] -> [[a]]
subsets xs = concatMap (`combinations` xs) [0..(length xs)]

-- computes the linear combination \sum x_i v_i with x_i = 1 for all i, since other linear combinations are the same as taking a smaller set of vectors.
linearcombination :: [Z10_2] -> Z10_2
linearcombination = foldr (zipWith (+)) (take 10 zeros)

-- group: groups into lists of same elements, no duplicates iff those are all of length <=1
containsNoDuplicates :: Eq a => [a] -> Bool
containsNoDuplicates as = all (<= 1) $ map length $ group as

-- vs is a base iff all linearcombinations are different
-- all linearcombinations are the linearcombination with all coefficients = 1 for all subsets
-- since linearcombination [] returns (0, ..., 0), this implicitly checks for that vector as well
isVectorSpaceBase :: [Z10_2] -> Bool
isVectorSpaceBase vs = 
  let subs = subsets vs
      lcs  = map linearcombination subs in
  -- check for 1*v1+1*v2      check for v1 == v2   check for (0, ..., 0)
  containsNoDuplicates lcs

-- Group into base candidates of length 4, filter all non-bases 
-- There's like a bazillion ways to do this, idk which one is more elegant/speedy
vectorSpaceBases :: [Z10_2] -> [[Z10_2]]
vectorSpaceBases vs = 
  let tvs  = take 4 vs
      vs'  = drop 4 vs 
      more = vectorSpaceBases vs' in
      if isVectorSpaceBase tvs 
        then tvs : more 
        else more

-- ALL the zeros
zeros :: [Z2]
zeros = Zero : zeros

-- NONE of the zeros
ones  :: [Z2]
ones  = One : ones

-- distance of v1 and v2 is the count of the ones in their sum in Z10_2
-- toInt to sum in Z, otherwise we would just get parity of the count of the ones
dist :: Z10_2 -> Z10_2 -> Int
dist v1 v2 = sum $ map toInt $ zipWith (+) v1 v2

-- just... go over all pairs, calculate the distance, take the minimum
minDist :: [Z10_2] -> Int
minDist vs = minimum $ map (uncurry dist . tuplify2) $ combinations 2 vs

-- M A I N
main :: IO()
main = do
  rgen <- getStdGen
  let rVecs' = rVecs rgen
  let tries  = 10000
  let vs     = take tries $ vectorSpaceBases rVecs'
  let dists  = map (\ vs' -> (vs', minDist vs')) vs
  let dists' = map snd dists
  let avg    = fromIntegral (sum dists') / fromIntegral (length dists')
  let best   = maximumBy (\ (_, d1) (_, d2) -> compare d1 d2) dists
  print $ "Average distance: " ++ show avg
  print $ "Best code: " ++ show best

  
