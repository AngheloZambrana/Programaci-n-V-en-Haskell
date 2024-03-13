module Ejercicio1 where
    import Log
    --ESTE EJERCCICIO ESTA HECHO SIN MAYBE
    -- | Parsea una cadena de entrada en un 'LogMessage'
    -- La cadena debe tener uno de los siguientes formatos:
    --    * "I <timestamp> <mensaje>" para mensajes de información.
    --    * "W <timestamp> <mensaje>" para mensajes de advertencia.
    --    * "E <código de error> <timestamp> <mensaje>" para mensajes de error.
    -- Cualquier otro formato de cadena se considera un mensaje desconocido.
    parseMessage :: String -> LogMessage
    parseMessage input
        | isInfo input = parseInfo input
        | isWarning input = parseWarning input
        | isError input = parseError input
        | otherwise = Unknown input

    -- | Verifica si la cadena representa un mensaje de información.
    isInfo :: String -> Bool
    isInfo ('I':'_':_) = True
    isInfo _ = False

    -- | Verifica si la cadena representa un mensaje de advertencia.
    isWarning :: String -> Bool
    isWarning ('W':'_':_) = True
    isWarning _ = False

    -- | Verifica si la cadena representa un mensaje de error.
    isError :: String -> Bool
    isError ('E':'_':_) = True
    isError _ = False

    -- | Parsea un mensaje de información y crea un 'LogMessage' correspondiente.
    parseInfo :: String -> LogMessage
    parseInfo input = LogMessage Info (readTimestamp input) (getMessage input)

    -- | Parsea un mensaje de advertencia y crea un 'LogMessage' correspondiente.
    parseWarning :: String -> LogMessage
    parseWarning input = LogMessage Warning (readTimestamp input) (getMessage input)

    -- | Parsea un mensaje de error y crea un 'LogMessage' correspondiente.(Este lo hice con gpt xd)
    parseError :: String -> LogMessage
    parseError input = LogMessage (Error (readErrorCode input)) (readTimestamp input) (getMessage input)

    -- | Lee el timestamp de la cadena de entrada y lo convierte a un entero. (Este lo hice con gpt xd)
    readTimestamp :: String -> TimeStamp
    readTimestamp input = read (takeWhile (/= '_') (drop 2 input))



    -- | Lee el código de error de la cadena de entrada y lo convierte a un entero. (Este lo hice con gpt xd)
    readErrorCode :: String -> Int
    readErrorCode input = read (takeWhile (/=' ') (drop 4 input))

    -- | Obtiene el mensaje de la cadena de entrada. (Este lo hice con gpt xd)
    getMessage :: String -> String
    getMessage = dropWhile (/='_') . dropWhile (=='_')


