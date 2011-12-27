import Test.QuickCheck
import Control.Monad
import Data.Maybe
import Data.Tree
import Data.List hiding (insert)
import System.Random

import Data.PKTree

main :: IO ()
main = mapM_ (\(n,p) -> do
	putStrLn n
	quickCheck p) [("Small, unsplit root",prop_small_unsplit_root), ("K Instantiable", prop_k_instantiable)]

-- PROPERTIES

prop_small_unsplit_root :: SmallPositive Int -> ShortList (SmallPositive Int) -> Gen Bool
prop_small_unsplit_root (SmallPositive k) (ShortList r) = do
	let r' = map getInt r
	t <- tree k r' (k-1)
	return (length (subForest t) == (k-1))
	where
	getInt (SmallPositive i) = i

prop_k_instantiable :: SmallPositive Int -> ShortList (SmallPositive Int) -> Gen Bool
prop_k_instantiable (SmallPositive k) (ShortList r) = do
	root <- sized $ tree k r'
	return $ okNotSplit root && all check (subForest root)
	where
	getInt (SmallPositive i) = i
	r' = map getInt r
	maxNodes :: Integer
	maxNodes = fromIntegral k * product (map (fromIntegral . getInt) r)
	okNotSplit t = genericLength (subForest t) <= maxNodes
	kInstantiable Node { rootLabel = Inner {}, subForest = kids } = length kids >= k
	kInstantiable Node { rootLabel = Leaf {}} = True
	check t = okNotSplit t && kInstantiable t && all check (subForest t)

-- GENERATORS

newtype SmallPositive a = SmallPositive a deriving (Eq, Ord, Show, Read)
instance (Random a, Num a, Arbitrary a) => Arbitrary (SmallPositive a) where
	arbitrary = fmap SmallPositive $ choose (2, 100)

newtype ShortList a = ShortList [a] deriving (Eq, Ord, Show, Read)
instance (Arbitrary a) => Arbitrary (ShortList a) where
	arbitrary = fmap ShortList $ resize 10 (listOf1 arbitrary)

arbitraryPoint :: Int -> Gen Point
arbitraryPoint = vector

arbitraryPointIn :: Rectangle -> Gen Point
arbitraryPointIn (l, u) = mapM choose (zip l u)

uniquePointsIn :: Int -> Rectangle -> Gen [Point]
uniquePointsIn n r = uniquePoints [] n
	where
	uniquePoints points 0 = return points
	uniquePoints points remaining = do
		point <- arbitraryPointIn r
		if isNothing (find (== point) points) then do {
			uniquePoints (point:points) (remaining-1);
		} else
			uniquePoints points remaining

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
