module Listutils
where

-- Ver si una lista es subconjunto de otra
containedIn :: Eq a => [a] -> [a] -> Bool
containedIn xs ys = foldr (&&) True belong
            where belong = map (\a -> elem a ys) xs


