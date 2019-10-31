
-- An implementation of segment trees for lists


type Range = (Int, Int)

data SegTree a = Leaf a Range | Node a Range (SegTree a) (SegTree a)

-- Construct a tree from a list and aggregation function
listToTree :: [a] -> (a -> a -> a) -> SegTree a

makeNode :: [a] -> (a -> a -> a) -> Range -> SegTree a
makeNode [x] agg (s,t) = makeLeaf x s
makeNode xs agg (s,t) = 

makeLeaf :: a -> Int -> SegTree a 
makeLeaf x n = Leaf x (n,n)

-- Query from a tree
query :: SegTree a -> Range -> a
query (Leaf x (n,n))
