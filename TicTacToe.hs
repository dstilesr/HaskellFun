module Main where
import Listutils
import Posutils
import System.IO


-- Movimientos posibles
validMoves :: [String]
validMoves = ["1 1", "1 2", "1 3",
              "2 1", "2 2", "2 3",
              "3 1", "3 2", "3 3"]

moveToPos :: String -> Pos
moveToPos str = (head ints, head (tail ints)) 
                where ints = map ((2*) . read) (words str)

validInput :: String -> [Pos] -> [Pos] -> Bool
validInput cs ps qs = case elem cs validMoves of
                           True -> not (elem (moveToPos cs) (ps ++ qs))
                           False -> False

-- Combinaciones ganadoras
col1 :: [Pos]
col2 :: [Pos]
col3 :: [Pos]
row1 :: [Pos]
row2 :: [Pos]
row3 :: [Pos]
diag1 :: [Pos]
diag2 :: [Pos]

col1 = [(2,2), (2,4), (2,6)]
col2 = [(a+2,b) | (a,b) <- col1]
col3 = [(a+4,b) | (a,b) <- col1]
row1 = map switch col1
row2 = map switch col2
row3 = map switch col3
diag1 = [(2*i, 2*i)|i <- [1..3]]
diag2 = [(2,6), (4,4), (6,2)]

winMoves :: [[Pos]]
winMoves = [col1, col2, col3, row1, row2, row3, diag1, diag2]

-- Determinar sin un jugador ganó
wonGame :: [Pos] -> Bool
wonGame ps = foldl (\b qs -> b || containedIn qs ps) False winMoves

winMsg :: Int -> IO ()
winMsg nm = displayMsg ("Congratulations! Player " ++ pl ++ " wins!\n")
          where pl = if mod nm 2 == 0 then "X" else "O"

checkWin :: Int -> ([Pos], [Pos]) -> Bool
checkWin n (xs, os) = wonGame ms
                  where ms = if mod n 2 == 0 then xs else os

failMsg :: String
failMsg = "Invalid move!\nValid moves:\n" ++ show validMoves ++"\n\
\Do not choose squares that are already taken!"

-- Turnos
turnPrompt :: Int -> IO ()
turnPrompt n = if (mod n 2) == 0 
               then displayMsg "X: "
               else displayMsg "O: "

addMove :: Int -> Pos -> ([Pos], [Pos]) -> ([Pos], [Pos])
addMove n p (xs, os) = if mod n 2 == 0 then (p:xs, os) else (xs, p:os)

nextTurn :: Int -> ([Pos], [Pos]) -> IO ()
nextTurn n (xs, os) = do
                      turnPrompt n
                      newMove <- getLine
                      if validInput newMove xs os
                      then do
                           p <- return (moveToPos newMove)
                           (nxs, nos) <- return (addMove n p (xs, os))
                           placeChars 'x' nxs
                           placeChars 'o' nos
                           if checkWin n (nxs, nos)
                           then winMsg n
                           else nextTurn (n + 1) (nxs, nos)
                      else if (length (xs ++ os)) == 9
                           then displayMsg "Draw!\n"
                           else do displayMsg failMsg
                                   getLine
                                   nextTurn n (xs, os)


-- Tablero inicial
board :: [String]
board = [" 1 2 3",
         "1 | | ",
         " -+-+-",
         "2 | | ",
         " -+-+-",
         "3 | | "]

-- Mostrar tablero actual
showBoard :: IO ()
showBoard = do clearScreen
               gotoPoint (1,1)
               seqn (map putStrLn board)
               gotoPoint (1,7)

-- Mostrar xs u os en el tablero
placeChars :: Char -> [Pos] -> IO ()
placeChars c ps = seqn (map (\p -> writeAt p [c]) ps) 

-- Mostrar un mensaje en la 'consola' (bajo el tablero)
displayMsg :: String -> IO()
displayMsg m = do writeAt (1,7) "\ESC[J"
                  writeAt (1,7) m


-- MAIN
main :: IO ()
main = do hSetBuffering stdout NoBuffering
          showBoard
          nextTurn 0 ([],[])

