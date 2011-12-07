import Test.QuickCheck
import Control.Monad
import Data.Tree
import Data.List hiding (insert)
import System.Random

import Data.PKTree

main :: IO ()
main = mapM_ (\(n,p) -> do
	putStrLn n
	quickCheck p) [("K Instantiable",prop_k_instantiable)]

-- PROPERTIES

prop_k_instantiable :: ReasonableR Int -> ShortList (ReasonableR Int) -> Gen Bool
prop_k_instantiable (ReasonableR k) (ShortList r) = do
	let r' = map getInt r
	t <- tree k r' (k-1)
	return (length (subForest t) == (k-1))
	where
	getInt (ReasonableR i) = i

-- GENERATORS

newtype NonOne a = NonOne a deriving (Eq, Ord, Show, Read)
instance (Num a, Arbitrary a) => Arbitrary (NonOne a) where
	arbitrary = fmap NonOne $ arbitrary `suchThat` (/= 1)

newtype ReasonableR a = ReasonableR a deriving (Eq, Ord, Show, Read)
instance (Random a, Num a, Arbitrary a) => Arbitrary (ReasonableR a) where
	arbitrary = fmap ReasonableR $ choose (1, 100) `suchThat` (/= 1)

newtype ShortList a = ShortList [a] deriving (Eq, Ord, Show, Read)
instance (Arbitrary a) => Arbitrary (ShortList a) where
	arbitrary = fmap ShortList $ resize 10 (listOf1 arbitrary)

arbitraryPoint :: Int -> Gen Point
arbitraryPoint = vector

arbitraryPointIn :: Rectangle -> Gen Point
arbitraryPointIn (l, u) = mapM choose (zip l u)

uniquePointsIn :: Int -> Rectangle -> Gen [Point]
uniquePointsIn n r = suchThat (mapM arbitraryPointIn (replicate n r)) (\x -> length x == length (nub x))

arbitraryRect :: Int -> Gen Rectangle
arbitraryRect d = do
	l <- arbitraryPoint d
	u <- mapM (\a -> suchThat arbitrary (>a)) l
	return (l, u)

--tree :: (Arbitrary a, Eq a) => Int -> [Int] -> Int -> Gen (PKTree a)
tree :: Int -> [Int] -> Int -> Gen (PKTree ())
tree k r n = do
	root <- arbitraryRect (length r)
	points <- n `uniquePointsIn` root
	let tree' ps = case ps of
		[] -> return $ cell root
		(x:xs) -> do
			parent <- tree' xs
			liftM (insert k r parent x) arbitrary
	tree' points
