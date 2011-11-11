-- http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.21.411
-- http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.21.883

import Data.Tree
import Data.List (unfoldr, findIndex)

type Point = [Float] -- http://hackage.haskell.org/package/tagged-list ?
type Rectangle = (Point, Point)
type PKTree = Tree Rectangle

k :: Int
k = 3

r :: [Int]
r = [2,2]

root :: PKTree
root = Node {
	rootLabel = ([-180, -90], [180, 90]),
	subForest = []
}

cell :: Rectangle -> PKTree
cell r = Node {
	rootLabel = r,
	subForest = []
}

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

dividingLines :: (Float, Float, Int) -> [Float]
dividingLines (l, u, r) =
	takeWhile (<u) $ unfoldr (\x -> Just (l+x, x+w)) w
	where
	w = (u-l)/(fromIntegral r)

divideOnLines :: [Float] -> Float -> Int
divideOnLines lines n =
	divisionFor (findIndex (n<=) lines)
	where
	divisionFor Nothing = length lines
	divisionFor (Just i) = i

divideUp :: [Int] -> [Float] -> Point -> Int -> [PKTree] -> [[PKTree]]
divideUp _ [] _ _ children = [children]
divideUp _ _ [] _ children = [children]
divideUp [] _ _ _ children = [children]
divideUp (r:rs) (w:ws) (l:ls) d children =
	concat $ map (divideUp rs ws ls (d+1)) $ take r $ npartition (\x -> floor (((nodeDim x d)-l) / w)) children
	where
	divisions = (length ws) + 1
	nodeDim n d = (snd (rootLabel n)) !! d

insert :: PKTree -> Point -> [[PKTree]]
insert (Node {rootLabel = (l, u), subForest = children}) p =
	divideUp r w l 0 newKids
	where
	w = map (\(l,u,r) -> (u-l)/(fromIntegral r)) (zip3 l u r)
--	lines = map dividingLines (zip3 l u r)
	newKids = children -- insert' children p

-- Takes the list of children from some node and inserts a Point
-- either as a child, or into a child
{-
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
-}

pointCell :: Point -> PKTree
pointCell p = Node {rootLabel = (p, p), subForest = []}

rectContains :: Rectangle -> Rectangle -> Bool
rectContains (l, u) (l', u') =
	and (compareEach (<) l l') && and (compareEach (>) u u')
	where
	compareEach _ [] _ = []
	compareEach _ _ [] = []
	compareEach f (a:as) (b:bs) = f a b : compareEach f as bs
