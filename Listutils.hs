module Listutils
where

-- Ver si una lista es subconjunto de otra
containedIn :: Eq a => [a] -> [a] -> Bool
containedIn xs ys = foldl (\b x -> b && (elem x ys)) True xs


