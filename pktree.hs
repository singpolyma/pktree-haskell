import Data.Tree
import Data.Maybe
import Data.List (find, partition)

type Point = [Float] -- http://hackage.haskell.org/package/tagged-list ?
type Rectangle = (Point, Point)
type PKTree = Tree Rectangle

k :: Int
k = 2

r :: [Int]
r = [2,2]

root :: PKTree
root = cell ([-180, -90], [180, 90])

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

instantiateSubdivisions :: Rectangle -> [PKTree] -> [PKTree]
instantiateSubdivisions (l, u) children =
	concat rest ++ concatMap (\div ->
		let newKids = instantiateSubdivisions (divBox div) div in
			if length newKids < k then
				newKids
			else
				[Node {
					rootLabel = divBox div,
					subForest = instantiateSubdivisions (divBox div) div
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

insert :: PKTree -> Point -> PKTree
insert (Node {rootLabel = (l, u), subForest = children}) p =
	Node {
		rootLabel = (l, u),
		subForest = instantiateSubdivisions (l, u) newKids
	}
	where
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
