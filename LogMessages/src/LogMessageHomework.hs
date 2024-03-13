module Ejercicio1Maybe where

import Log

-- | Parsea una cadena de entrada en un 'LogMessage' y devuelve un Maybe LogMessage.
-- La cadena debe tener uno de los siguientes formatos:
--    * "I <timestamp> <mensaje>" para mensajes de información.
--    * "W <timestamp> <mensaje>" para mensajes de advertencia.
--    * "E <código de error> <timestamp> <mensaje>" para mensajes de error.
-- Cualquier otro formato de cadena se considera un mensaje desconocido.
parseMessage :: String -> Maybe LogMessage
parseMessage input
    | isInfo input = Just $ parseInfo input
    | isWarning input = Just $ parseWarning input
    | isError input = Just $ parseError input
    | otherwise = Just $ Unknown input

-- | Verifica si la cadena representa un mensaje de información.
isInfo :: String -> Bool
isInfo ('I':' ':_) = True
isInfo _ = False

-- | Verifica si la cadena representa un mensaje de advertencia.
isWarning :: String -> Bool
isWarning ('W':' ':_) = True
isWarning _ = False

-- | Verifica si la cadena representa un mensaje de error.
isError :: String -> Bool
isError ('E':' ':_) = True
isError _ = False

parseInfo :: String -> LogMessage
parseInfo input = LogMessage Info (readTimestamp input) (getMessage input)

parseWarning :: String -> LogMessage
parseWarning input = LogMessage Warning (readTimestamp input) (getMessage input)

parseError :: String -> LogMessage
parseError input = let
  (errorCode:timestamp:message) = tail $ words input
  in LogMessage (Error (read errorCode)) (read timestamp) (unwords message)

readTimestamp :: String -> TimeStamp
readTimestamp input = read (takeWhile (/= ' ') (drop 2 input))

getMessage :: String -> String
getMessage input = drop 3 $ dropWhile (/= ' ') input

-- | Parsea una cadena de entrada en una lista de 'LogMessage'.
parseList :: String -> [Maybe LogMessage]
parseList = map parseMessage . lines


-- | Inserta un nuevo LogMessage en un MessageTree existente, produciendo un nuevo MessageTree.
-- Se asume que el MessageTree está ordenado por marca de tiempo.
-- Si el LogMessage dado es Unknown, devuelve el MessageTree original sin cambios.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree  -- Si el LogMessage es Unknown, devuelve el árbol original sin cambios
insert logMessage Leaf = Node Leaf logMessage Leaf  -- Si el árbol está vacío, crea un nuevo Node con el LogMessage
insert logMessage@(LogMessage _ marcaTiempo _) (Node treeLeft nodoMessage@(LogMessage _ marcaTiempoNodo _) treeRight)
    | marcaTiempo < marcaTiempoNodo = Node (insert logMessage treeLeft) nodoMessage treeRight  -- Inserta en el subárbol izquierdo
    | otherwise = Node treeLeft nodoMessage (insert logMessage treeRight)  -- Inserta en el subárbol derecho

-- | Construye un MessageTree a partir de una lista de LogMessage, insertando sucesivamente los mensajes en un MessageTree (comenzando con una Leaf).
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- | Toma un MessageTree ordenado y produce una lista de todos los LogMessage que contiene, ordenados por marca de tiempo de menor a mayor.
-- Esto se conoce como un recorrido en orden del MessageTree.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node treeLeft node treeRight) = inOrder treeLeft ++ [node] ++ inOrder treeRight

-- Con estas funciones, podemos eliminar los mensajes Unknown y ordenar los mensajes bien formados utilizando una expresión como:
-- inOrder (construir arbol)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage2 . filter isSevereError . inOrder . build
  where
    isSevereError (LogMessage (Error severity) _ _) = severity >= 50
    isSevereError _ = False

-- | Obtiene el mensaje de un LogMessage.
getMessage2 :: LogMessage -> String
getMessage2 (LogMessage _ _ msg) = msg
getMessage2 _ = ""  -- Por si hay mensajes Unknown
