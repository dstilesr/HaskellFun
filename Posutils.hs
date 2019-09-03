module Posutils where

-- Funciones y tipo para manejar la pantalla
type Pos = (Int, Int)
gotoPoint :: Pos -> IO ()
gotoPoint (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

switch :: Pos -> Pos
switch (x,y) = (y,x)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

writeAt :: Pos -> String -> IO()
writeAt p cs = do gotoPoint p
                  putStr cs


