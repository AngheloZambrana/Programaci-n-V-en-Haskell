module TicTacToe where
import Data.Maybe (isJust)

type Casilla = Maybe Char
type Tablero = [Casilla]

data Jugador = X | O deriving (Eq, Show)

tableroVacio :: Tablero
tableroVacio = replicate 9 Nothing

turnoInicial :: Jugador
turnoInicial = X

obtenerMovimiento :: Tablero -> IO Int
obtenerMovimiento tablero = do
    putStrLn "Ingrese la posición (1-9):"
    input <- getLine
    let movimiento = read input :: Int
    if esMovimientoValido tablero movimiento
        then return movimiento
        else do
            putStrLn "Movimiento inválido. Inténtelo de nuevo."
            obtenerMovimiento tablero

esMovimientoValido :: Tablero -> Int -> Bool
esMovimientoValido tablero movimiento = movimiento >= 1 && movimiento <= 9 && tablero !! (movimiento - 1) == Nothing

actualizarTablero :: Tablero -> Int -> Jugador -> Tablero
actualizarTablero tablero movimiento jugador = 
    let (antes, _:despues) = splitAt (movimiento - 1) tablero
    in antes ++ [Just (simboloJugador jugador)] ++ despues

simboloJugador :: Jugador -> Char
simboloJugador X = 'X'
simboloJugador O = 'O'

hayGanador :: Tablero -> Jugador -> Bool
hayGanador t j = any (lineaCompleta j) posicionesGanadoras
    where
        lineaCompleta j linea = all (\pos -> t !! pos == Just (simboloJugador j)) linea
        posicionesGanadoras = [[0,1,2],[3,4,5],[6,7,8],[0,3,6],[1,4,7],[2,5,8],[0,4,8],[2,4,6]]

jugarTicTacToe :: IO ()
jugarTicTacToe = juego tableroVacio turnoInicial

juego :: Tablero -> Jugador -> IO ()
juego tablero jugador = do
    mostrarTablero tablero
    putStrLn $ "Turno del jugador " ++ show jugador
    movimiento <- obtenerMovimiento tablero
    let nuevoTablero = actualizarTablero tablero movimiento jugador
    if hayGanador nuevoTablero jugador
        then do
            mostrarTablero nuevoTablero
            putStrLn $ "¡El jugador " ++ show jugador ++ " ha ganado!"
        else if tableroCompleto nuevoTablero
            then do
                mostrarTablero nuevoTablero
                putStrLn "¡Es un empate!"
            else juego nuevoTablero (cambiarTurno jugador)

mostrarTablero :: Tablero -> IO ()
mostrarTablero tablero = putStrLn $ unlines $ map (concatMap mostrarCasilla) (chunksOf 3 tablero)
    where
        mostrarCasilla Nothing = " "
        mostrarCasilla (Just c) = [c]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

tableroCompleto :: Tablero -> Bool
tableroCompleto = all isJust

cambiarTurno :: Jugador -> Jugador
cambiarTurno X = O
cambiarTurno O = X
