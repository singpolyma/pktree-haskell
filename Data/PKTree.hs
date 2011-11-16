-- | Implementation of the PKTree spatial index data structure
--
-- The reccomended way to import this module is:
--
-- > import qualified Data.PKTree as PKTree
-- > pkInsert = insert K [rx,ry,..]
module Data.PKTree (Point, Rectangle, PKTree, cell, pointCell, rect, insert, radiusSearch) where

import Data.Tree
import Data.Maybe
import Data.List (find, partition)

-- | An n-dimensional point
type Point = [Float] -- http://hackage.haskell.org/package/tagged-list ?

-- | An n-dimensional hyperrectangle
type Rectangle = (Point, Point)

-- | Inner nodes have rectangles, leaves are points and data
data Node a = Inner Rectangle | Leaf Point a deriving (Eq, Show, Read)

-- | A PKTree
type PKTree a = Tree (Node a)

nreplace :: (a -> a) -> Int -> [a] -> [a]
nreplace _ _ [] = error "Tried to replace a member not in the list"
nreplace f i (l:ls)
	| i == 0 = f l : ls
	| otherwise = l : nreplace f (i - 1) ls

npartition :: (a -> Int) -> [a] -> [[a]]
npartition f = foldr (\x -> nreplace (x:) (f x)) (repeat [])

rectContains :: Rectangle -> Rectangle -> Bool
rectContains (l, u) (l', u') =
	all (uncurry (<=)) (zip l l') && all (uncurry (>)) (zip u u')

-- | Contruct a tree with no children
cell :: Rectangle -> PKTree a
cell rect = Node {
	rootLabel = Inner rect,
	subForest = []
}

-- | Construct a leaf node representing a point
pointCell :: Point -> a -> PKTree a
pointCell p v = Node {
	rootLabel = Leaf p v,
	subForest = []
}

-- | Extract the rectangle from a node
rect :: PKTree a -> Rectangle
rect =
	rect' . rootLabel
	where
	rect' (Inner r) = r
	rect' (Leaf p _) = (p,p)

-- | Insert a point into a PKTree
insert :: (Eq a) =>
          Int         -- ^ K, minimum number of nodes in subdivision
          -> [Int]    -- ^ r, number of divisions in each dimension
          -> PKTree a -- ^ Root of PKTree
          -> Point    -- ^ Point to insert
          -> a        -- ^ Data that goes with point
          -> PKTree a
insert k r n@(Node {rootLabel = lbl, subForest = children}) p v =
	Node {
		rootLabel = lbl,
		subForest = instantiateDivisions k r (rect n) newKids
	}
	where
	newKids = insert' k r children p v

-- Takes the list of children from some node and inserts a Point
-- either as a child, or into a child
insert' :: (Eq a) => Int -> [Int] -> [PKTree a] -> Point -> a -> [PKTree a]
insert' k r children p v
	| children `contains` p =
		let (Just c) = children `maybecontain` p in
			let c' = insert k r c p v in
				-- If c' has less than k children, it is not k-instantiable
				(if length (subForest c') < k then
					subForest c'
				else
					[c']
				) ++ filter (/= c) children
	| otherwise =
		pointCell p v : children
	where
	contains f = isJust . maybecontain f
	maybecontain f p = find (\x -> rectContains (rect x) (p,p)) f

divideUp :: [Int] -> [Float] -> Point -> [PKTree a] -> [[PKTree a]]
divideUp r w l children =
	take (product r) $ npartition nodeBucket children
	where
	nodeBucket x = fst $ foldr bucket (0,1) (zip (nodePoint x) dimData)
	nodePoint = snd . rect
	-- Subtract l, the lower bound, from x to make x positive
	-- floor (x-l)/w is the current dimension bucket
	-- factor is the multiplied size of previous dimensions
	-- Multiply factor by bucket to vectorize
	bucket (x, (r,w,l)) (_,factor) =
		(factor * floor ((x-l)/w), factor*(r+1))
	dimData = zip3 r w l

instantiateDivisions :: Int -> [Int] -> Rectangle -> [PKTree a] -> [PKTree a]
instantiateDivisions k r (l, u) children =
	concat rest ++ concatMap (\div ->
		let newKids = instantiateDivisions k r (divBox div) div in
			if length newKids < k then
				newKids
			else
				[Node {
					rootLabel = Inner (divBox div),
					subForest = newKids
				}]
	) subdivisions
	where
	divBox = box . fst . rect . head
	box p = unzip $ map (\(x,w) ->
		let low = w * fFloor (x/w) in
			(low, low + w)
		) (zip p w)
	fFloor = fromIntegral . (floor :: Float -> Int)
	(subdivisions, rest) =
		partition (\x -> length x >= k) (divideUp r w l children)
	w = map (\(l,u,r) -> (u-l) / fromIntegral r) (zip3 l u r)

-- | Search for points in some hypercircle
radiusSearch :: Point       -- ^ Centre of hypercircle
                -> Float    -- ^ Radius of hypercircle
                -> PKTree a -- ^ Tree to search in
                -> [(Point, a)]
radiusSearch p r tree =
	concatMap (\t -> case () of
		_ | circleContains (rect t) -> getLeaves t
		  | circleIntersect (rect t) -> radiusSearch p r t
		  | otherwise -> []
	) (subForest tree)
	where
	getLeaves (Node {rootLabel = (Leaf p v), subForest = _}) = [(p, v)]
	getLeaves (Node {rootLabel = _, subForest = children}) =
		concatMap getLeaves children
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
