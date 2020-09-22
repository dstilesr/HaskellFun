

type Queen = (Int, Int)

queensAttack :: Queen -> Queen -> Bool
queensAttack (p1, p2) (q1, q2) = or [p1 == q1,
                                     p2 == q2,
                                     (abs (p1 - q1)) == (abs (p2 - q2))]


newQueen :: [Queen] -> Queen -> Bool
newQueen qs q = not $ or (map valid qs)
                where valid = queensAttack q


numsTo :: Int -> [Int]
numsTo n
      | n > 0     = [0..(n - 1)]
      | otherwise = []


countSol :: Int -> [Queen] -> Int
countSol 0 _ = 0
countSol n qs
           | (length qs) == (n - 1) = if or (map (\i -> newQueen qs (n - 1, i)) 
                                                 (numsTo n)) then 1 else 0
           | otherwise              = sum [countSol n $ (length qs, i):qs | 
                                           i <- (numsTo n),
                                           newQueen qs (length qs, i)]


main :: IO ()
main = do putStrLn "Enter Number of Queens:"
          num <- getLine
          n <- return $ read num
          putStrLn . show $ countSol n []

