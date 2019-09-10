module Main where
import Listutils
import Posutils
import System.IO



-- Movimientos posibles
validMoves :: [String]
validMoves = ["ul", "u", "ur",
              "l", "c", "r",
              "dl", "d", "dr"]

moveToPos :: String -> Pos
moveToPos cs 
            | cs == "ul" = (2,2)
            | cs == "u"  = (4,2)
            | cs == "ur" = (6,2)
            | cs == "l"  = (2,4)
            | cs == "c"  = (4,4)
            | cs == "r"  = (6,4)
            | cs == "dl" = (2,6)
            | cs == "d"  = (4,6)
            | cs == "dr" = (6,6)

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

winMsg :: String -> IO ()
winMsg nm = displayMsg ("Congratulations! Player " ++ nm ++ " wins!\n")
               
failMsg :: String
failMsg = "Invalid move!\nValid moves:\n\
\ul u ur\nl c r\ndl d dr\n\
\Do not choose squares that are already taken!"

-- Turnos
turnXs :: ([Pos], [Pos]) -> IO ()
turnOs :: ([Pos], [Pos]) -> IO ()

turnXs (xs,os) = do displayMsg "X: "
                    newx <- getLine
                    if validInput newx xs os
                       then do x <- return (moveToPos newx)
                               showXs (x:xs)
                               showOs os
                               if wonGame (x:xs)
                                 then winMsg "X"
                                 else turnOs (x:xs, os)
                       else if (length (xs ++ os)) == 9
                               then displayMsg "Draw!\n"
                               else do displayMsg failMsg
                                       getLine
                                       turnXs (xs, os)

turnOs (xs,os) = do displayMsg "O: "
                    newo <- getLine
                    if validInput newo xs os
                       then do o <- return (moveToPos newo)
                               showXs xs
                               showOs (o:os)
                               if wonGame (o:os)
                                 then winMsg "O"
                                 else turnXs (xs, o:os)
                       else if (length (xs ++ os)) == 9
                               then displayMsg "Draw!\n"
                               else do displayMsg failMsg
                                       getLine
                                       turnOs (xs, os)


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
showXs :: [Pos] -> IO ()
showXs ps = seqn (map (\p -> writeAt p "x") ps)

showOs :: [Pos] -> IO ()
showOs ps = seqn (map (\p -> writeAt p "o") ps)


-- Mostrar un mensaje en la 'consola' (bajo el tablero)
displayMsg :: String -> IO()
displayMsg m = do writeAt (1,7) "\ESC[J"
                  writeAt (1,7) m


-- MAIN
main :: IO ()
main = do hSetBuffering stdout NoBuffering
          showBoard
          turnXs ([],[])

