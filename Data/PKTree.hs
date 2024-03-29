-- | Implementation of the PKTree spatial index data structure
--
-- The reccomended way to import this module is:
--
-- > import qualified Data.PKTree as PKTree
-- > pkInsert = insert K [rx,ry,..]
module Data.PKTree (Point, Rectangle, PKTree, Node(..), cell, pointCell, rect, insert, radiusSearch, cubeSearch) where

import Data.Tree
import Data.Maybe
import Data.List hiding (insert)

-- | An n-dimensional point
type Point = [Float] -- http://hackage.haskell.org/package/tagged-list ?

-- | An n-dimensional hyperrectangle
type Rectangle = (Point, Point)

-- | Inner nodes have rectangles, leaves are points and data
data Node a = Inner Rectangle | Leaf Point a deriving (Eq, Show, Read)

-- | A PKTree
type PKTree a = Tree (Node a)

npartition :: (Integral b) => (a -> b) -> [a] -> [[a]]
npartition f =
	map (map snd) . groupBy (onFst (==)) .  sortBy (onFst compare) . map (\x -> (f x, x))
	where
	onFst f (a,_) (b,_) = f a b

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
divideUp r w l children = npartition nodeBucket children
	where
	nodeBucket :: PKTree a -> Integer
	nodeBucket x = sum $ zipWith4 bucket (upperPoint x) w l cumulDimSizes
	cumulDimSizes = map product (inits (map fromIntegral r))
	upperPoint = snd . rect
	-- Subtract l, the lower bound, from x to make x positive
	-- floor (x-l)/w is the current dimension bucket
	bucket x w l d = d * truncate ((x-l)/w)

instantiateDivisions :: Int -> [Int] -> Rectangle -> [PKTree a] -> [PKTree a]
instantiateDivisions k r (l, u) children =
	concat rest ++ concatMap (\div ->
		-- FIXME: Returns empty list on identical points without this
		let uDiv = nubBy (\x y -> rect x == rect y) div in
			let newKids = instantiateDivisions k r (divBox uDiv) uDiv in
				if length newKids < k then
					newKids
				else
					[Node {
						rootLabel = Inner (divBox uDiv),
						subForest = newKids
					}]
	) subdivisions
	where
	divBox = box . fst . rect . head
	box p = unzip $ zipWith (\x w ->
		let low = w * fFloor (x/w) in
			(low, low + w)
		) p w
	fFloor = fromIntegral . (floor :: Float -> Int)
	(subdivisions, rest) =
		partition (\x -> length x >= k) (divideUp r w l children)
	w = zipWith3 (\l u r -> (u-l) / fromIntegral r) l u r

getLeaves :: PKTree a -> [(Point, a)]
getLeaves (Node {rootLabel = (Leaf p v), subForest = _}) = [(p, v)]
getLeaves (Node {rootLabel = _, subForest = children}) =
	concatMap getLeaves children

protoSearch :: (PKTree a -> Bool) -> (PKTree a -> Bool) -> PKTree a -> [(Point, a)]
protoSearch contains intersect tree =
	concatMap (\t -> case () of
		_ | contains t -> getLeaves t
		  | intersect t -> protoSearch contains intersect t
		  | otherwise -> []
	) (subForest tree)

-- | Search for points in some hypercircle
radiusSearch :: Point       -- ^ Centre of hypercircle
                -> Float    -- ^ Radius of hypercircle
                -> PKTree a -- ^ Tree to search in
                -> [(Point, a)]
radiusSearch p r =
	protoSearch (circleContains . rect) (circleIntersect . rect)
	where
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
	circleDistances rect@(l, _) = zipWith3 (\l w p -> p - l - w) l (halfRectDims rect) p
	halfRectDims (l, u) = zipWith (\l u -> (u-l)/2) l u

-- | Search for points in some hypercube
cubeSearch :: Rectangle   -- ^ Hypercube to use as bounds
              -> Bool     -- ^ Open hypercube?
              -> PKTree a -- ^ Tree to search in
              -> [(Point, a)]
cubeSearch (l,u) open =
	protoSearch (contains . rect) (intersect . rect)
	where
	contains (l',u') = all (uncurry lt) (zip l l') && all (uncurry gt) (zip u u')
	intersect (l',u') = and $ zipWith4 (\l u l' u' ->
			lll l l' u || lll l' l u' || lll l u' u || lll l' u u'
		) l u l' u'
	lll a b c = lt a b && lt b c
	lt = if open then (<=) else (<)
	gt = if open then (>=) else (>)
