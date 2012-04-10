import Test.QuickCheck
import Control.Monad
import Data.Maybe
import Data.Tree
import Data.List hiding (insert)
import System.Random

import Data.PKTree

main :: IO ()
main = mapM_ (\(n,p) -> putStrLn n >> quickCheck p) [
		("Small, unsplit root",prop_small_unsplit_root),
		("K Instantiable", prop_k_instantiable),
		("cubeSearch", prop_cube_search),
		("cubeSearch (plane)", prop_plane_search)
	]

-- PROPERTIES

prop_small_unsplit_root :: SmallPositive Int -> ShortList (SmallPositive Int) -> Gen Bool
prop_small_unsplit_root (SmallPositive k) (ShortList r) = do
	t <- tree k r' (k-1)
	return (length (subForest t) == (k-1))
	where
	r' = map getInt r

prop_k_instantiable :: SmallPositive Int -> ShortList (SmallPositive Int) -> Gen Bool
prop_k_instantiable (SmallPositive k) (ShortList r) = do
	root <- sized $ tree k r'
	return $ okNotSplit root && all check (subForest root)
	where
	r' = map getInt r
	maxNodes :: Integer
	maxNodes = fromIntegral k * product (map fromIntegral r')
	okNotSplit t = genericLength (subForest t) <= maxNodes
	kInstantiable Node { rootLabel = Inner {}, subForest = kids } = length kids >= k
	kInstantiable Node { rootLabel = Leaf {}} = True
	check t = okNotSplit t && kInstantiable t && all check (subForest t)

prop_cube_search :: SmallPositive Int -> ShortList (SmallPositive Int) -> Gen Bool
prop_cube_search (SmallPositive k) (ShortList r) = do
	root <- sized $ tree k r'
	therect <- arbitraryRect d
	thepoint <- arbitraryPointIn therect
	open <- arbitrary
	return $ elem (thepoint,()) $
		cubeSearch therect open $ insert k r' root thepoint ()
	where
	r' = map getInt r
	d = length r

prop_plane_search :: SmallPositive Int -> ShortList (SmallPositive Int) -> Gen Bool
prop_plane_search (SmallPositive k) (ShortList r) = do
	root <- sized $ tree k r'
	therect <- arbitraryRect d
	thepoint <- arbitraryPointIn therect
	let rects = zipWith (\i v ->
			(
				replicate i (-inf) ++ v : replicate (d-i-1) (-inf),
				replicate i inf ++ v : replicate (d-i-1) inf
			)
		) [0..] thepoint
	let tree = insert k r' root thepoint ()
	return $ all (elem (thepoint,())) $
		map (\r -> cubeSearch r True tree) rects
	where
	inf = 1/0.0
	r' = map getInt r
	d = length r

-- GENERATORS

getInt :: SmallPositive a -> a
getInt (SmallPositive i) = i

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
	tree' root points
	where
	tree' root [] = return $ cell root
	tree' root (x:xs) = do
		parent <- tree' root xs
		liftM (insert k r parent x) arbitrary
