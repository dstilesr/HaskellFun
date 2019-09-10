module Listutils
where


-- Dividir lista en mitades
halves :: [a] -> ([a], [a])
halves as = (take n as, drop n as)
            where n = div (length as) 2


-- Ver si una lista es subconjunto de otra
containedIn :: Eq a => [a] -> [a] -> Bool
containedIn xs ys = foldl (\b x -> b && (elem x ys)) True xs


-- Calcular min. comun multiplo de una lista de numeros
lcmList :: Integral a => [a] -> a
lcmList = foldl lcm 1


