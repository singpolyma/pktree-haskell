-- http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.21.411
-- http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.21.883

import Data.Tree
import Data.Maybe
import Data.List (find)

type Point = [Float] -- http://hackage.haskell.org/package/tagged-list ?
type Rectangle = (Point, Point)
type PKTree = Tree Rectangle

k :: Int
k = 3

r :: [Int]
r = [2,2]

root :: PKTree
root = cell ([-180, -90], [180, 90])

test :: PKTree
test = Node {
	rootLabel = ([-180, -90], [180, 90]),
	subForest = [cell ([-5,-5], [-6,-6]), cell ([5,5], [6,6])]
}

nreplace :: (a -> a) -> Int -> [a] -> [a]
nreplace _ _ [] = error "Tried to replace a member not in the list"
nreplace f i (l:ls)
	| i == 0 = f l : ls
	| otherwise = l : nreplace f (i - 1) ls

npartition :: (a -> Int) -> [a] -> [[a]]
npartition f = foldr (\x -> nreplace (x:) (f x)) (repeat [])

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

insert :: PKTree -> Point -> PKTree
insert (Node {rootLabel = (l, u), subForest = children}) p =
	Node { rootLabel = (l, u), subForest = newKids }
	where
	subdivisions = filter (\x -> length x >= k) (divideUp r w l newKids)
	w = map (\(l,u,r) -> (u-l) / fromIntegral r) (zip3 l u r)
	newKids = insert' children p

-- Takes the list of children from some node and inserts a Point
-- either as a child, or into a child
insert' :: [PKTree] -> Point -> [PKTree]
insert' children p
	| children `contains` p =
		let (Just c) = children `maybecontain` p in
			let c' = insert c p in
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

cell :: Rectangle -> PKTree
cell r = Node {
	rootLabel = r,
	subForest = []
}

pointCell :: Point -> PKTree
pointCell p = cell (p, p)

rectContains :: Rectangle -> Rectangle -> Bool
rectContains (l, u) (l', u') =
	and (compareEach (<) l l') && and (compareEach (>) u u')
	where
	compareEach _ [] _ = []
	compareEach _ _ [] = []
	compareEach f (a:as) (b:bs) = f a b : compareEach f as bs
