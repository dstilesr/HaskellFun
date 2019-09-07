module Main where
import Posutils
import Listutils
import System.IO

-- Tablero de juego
type Board = [Int]

initBoard :: Board
initBoard = reverse [1..5]

-- Determinar si el tablero esta vacio (juego terminado)
emptyBoard :: Board -> Bool
emptyBoard = foldl (\b n -> b && (n == 0)) True

showBoard :: Board -> IO ()
showBoard bs = do clearScreen
                  gotoPoint (1,1)
                  seqn (map (\n -> putStrLn (replicate n '*')) bs)


-- Movimientos en el juego
-- Un movimiento sera un par (fila, elementos a remover)
type Move = (Int, Int)

-- Determinar si un movimiento es valido
isValid :: Board -> Move -> Bool
isValid bs (r, n) = (r < length bs) && ((bs !! r) >= n)

newBoard :: Board -> Move -> Board
newBoard bs (r, n) = hd ++ [x - n] ++ tl
         where hd = take r bs
               tl = drop (r + 1) bs
               x = bs !! r


-- Mensaje de fin de juego
winMsg :: Int -> IO ()
winMsg n = displayMsg ("Congratulations! Player " ++ show (mod n 2) ++ " Wins!\n")


displayMsg :: String -> IO ()
displayMsg s = do writeAt (1,6) "\ESC[J"
                  writeAt (1,6) s


-- Turnos
gameTurn :: Board -> Int -> IO ()
gameTurn bs tn = do 
                displayMsg ("Player " ++ show (mod tn 2) ++ ". Line: ")
                r <- readLn
                putStr "Items to remove: "
                n <- readLn
                if isValid bs (r,n)
                   then do
                        let nbs = newBoard bs (r,n)
                        showBoard nbs
                        checkWin nbs tn
                   else do displayMsg "Invalid move!"
                           getLine
                           gameTurn bs tn


checkWin :: Board -> Int -> IO ()
checkWin bs tn = if emptyBoard bs
                    then winMsg tn
                    else gameTurn bs (tn + 1)

-- MAIN
main :: IO ()
main = do hSetBuffering stdout NoBuffering 
          showBoard initBoard
          gameTurn initBoard 0


