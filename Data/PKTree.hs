-- | Implementation of the PKTree spatial index data structure
--
-- The reccomended way to import this module is:
--
-- > import qualified Data.PKTree as PKTree
-- > pkInsert = insert K [rx,ry,..]
module Data.PKTree (Point, Rectangle, PKTree, cell, pointCell, insert, radiusSearch) where

import Data.Tree
import Data.Maybe
import Data.List (find, partition)

-- | An n-dimensional point
type Point = [Float] -- http://hackage.haskell.org/package/tagged-list ?

-- | An n-dimensional hyperrectangle
type Rectangle = (Point, Point)

-- | A PKTree
type PKTree = Tree Rectangle

nreplace :: (a -> a) -> Int -> [a] -> [a]
nreplace _ _ [] = error "Tried to replace a member not in the list"
nreplace f i (l:ls)
	| i == 0 = f l : ls
	| otherwise = l : nreplace f (i - 1) ls

npartition :: (a -> Int) -> [a] -> [[a]]
npartition f = foldr (\x -> nreplace (x:) (f x)) (repeat [])

rectContains :: Rectangle -> Rectangle -> Bool
rectContains (l, u) (l', u') =
	and (compareEach (<) l l') && and (compareEach (>) u u')
	where
	compareEach _ [] _ = []
	compareEach _ _ [] = []
	compareEach f (a:as) (b:bs) = f a b : compareEach f as bs

-- | Contruct a tree with no children
cell :: Rectangle -> PKTree
cell rect = Node {
	rootLabel = rect,
	subForest = []
}

-- | Construct a leaf node representing a point
pointCell :: Point -> PKTree
pointCell p = cell (p, p)

-- | Insert a point into a PKTree
insert :: Int       -- ^ K, minimum number of nodes in subdivision
          -> [Int]  -- ^ r, number of divisions in each dimension
          -> PKTree -- ^ Root of PKTree
          -> Point  -- ^ Point to insert
          -> PKTree
insert k r (Node {rootLabel = (l, u), subForest = children}) p =
	Node {
		rootLabel = (l, u),
		subForest = instantiateDivisions k r (l, u) newKids
	}
	where
	newKids = insert' k r children p

-- Takes the list of children from some node and inserts a Point
-- either as a child, or into a child
insert' :: Int -> [Int] -> [PKTree] -> Point -> [PKTree]
insert' k r children p
	| children `contains` p =
		let (Just c) = children `maybecontain` p in
			let c' = insert k r c p in
				-- If c' has less than k children, it is not k-instantiable
				(if length (subForest c') < k then
					subForest c'
				else
					[c']
				) ++ filter (/= c) children
	| otherwise =
		pointCell p : children
	where
	contains f p = isJust (maybecontain f p)
	maybecontain f p = find (\x -> rectContains (rootLabel x) (p,p)) f

divideUp :: [Int] -> [Float] -> Point -> [PKTree] -> [[PKTree]]
divideUp r w l children =
	take (product r) $ npartition nodeBucket children
	where
	nodeBucket x = fst $ foldr bucket (0,1) (zip (nodePoint x) dimData)
	nodePoint = snd . rootLabel
	-- Subtract l, the lower bound, from x to make x positive
	-- floor (x-l)/w is the current dimension bucket
	-- factor is the multiplied size of previous dimensions
	-- Multiply factor by bucket to vectorize
	bucket (x, (r,w,l)) (_,factor) =
		(factor * floor ((x-l)/w), factor*(r+1))
	dimData = zip3 r w l

instantiateDivisions :: Int -> [Int] -> Rectangle -> [PKTree] -> [PKTree]
instantiateDivisions k r (l, u) children =
	concat rest ++ concatMap (\div ->
		let newKids = instantiateDivisions k r (divBox div) div in
			if length newKids < k then
				newKids
			else
				[Node {
					rootLabel = divBox div,
					subForest = newKids
				}]
	) subdivisions
	where
	divBox = box . fst . rootLabel . head
	box p = unzip $ map (\(x,w) ->
		let low = w * fFloor (x/w) in
			(low, low + w)
		) (zip p w)
	fFloor = fromIntegral . (floor :: Float -> Int)
	(subdivisions, rest) =
		partition (\x -> length x >= k) (divideUp r w l children)
	w = map (\(l,u,r) -> (u-l) / fromIntegral r) (zip3 l u r)

-- | Search for points in some hypercircle
radiusSearch :: Point     -- ^ Centre of hypercircle
                -> Float  -- ^ Radius of hypercircle
                -> PKTree -- ^ Tree to search in
                -> [Point]
radiusSearch p r tree =
	concatMap (\t -> case () of
		_ | circleContains (rootLabel t) -> getLeaves t
		  | circleIntersect (rootLabel t) ->
			radiusSearch p r t
		  | otherwise -> []
	) (subForest tree)
	where
	getLeaves (Node {rootLabel = (l, u), subForest = children})
		| l == u = [l]
		| otherwise = concatMap getLeaves children
	circleContains (l, u) =
		sqdist (zip l p) <= sq r && sqdist (zip u p) <= sq r
	-- Thanks to http://stackoverflow.com/questions/401847/circle-rectangle-collision-detection-intersection/402010#402010
	circleIntersect rect =
		let distWidth = zip (circleDistances rect) (halfRectDims rect) in
			((not . or) (map (\(d,w) -> d > (w+r)) distWidth) &&
			any (uncurry (<=)) distWidth) ||
			sqdist distWidth <= sq r
	sqdist = foldr (\(d,w) dist -> dist + sq (d-w)) 0
	sq = (^(2::Int))
	circleDistances rect@(l, _) = map (\(l,w,p) -> p - l - w) (zip3 l (halfRectDims rect) p)
	halfRectDims (l, u) = map (\(l,u) -> (u-l)/2) (zip l u)
