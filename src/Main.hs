{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Time (Day, getCurrentTime, utctDay)
import Data.Maybe (listToMaybe)
import Text.Regex.Posix ((=~))

-- Definición de la tabla persona
data Persona = Persona {
    idPersona :: Int,
    nombrePersona :: String,
    correoPersona :: String,
    fechaNacimientoPersona :: Maybe Day
} deriving (Show)

instance FromRow Persona where
    fromRow = Persona <$> field <*> field <*> field <*> field

data Prestamo = Prestamo {
    idPrestamo :: Int,
    idPersonaPrestamo :: Int,
    idLibroPrestamo :: Int,
    fechaPrestamo :: Day,
    fechaDevolucion :: Maybe Day
} deriving (Show)

instance FromRow Prestamo where
    fromRow = Prestamo <$> field <*> field <*> field <*> field <*> field

-- Conexión a la base de datos
connectDB :: IO Connection
connectDB = connect defaultConnectInfo {
    connectHost = "databasehaskell.ckd83boidbuw.us-east-1.rds.amazonaws.com",  -- Cambia según tu configuración
    connectPort = 5432,
    connectDatabase = "libraryDB",
    connectUser = "postgres",
    connectPassword = "fugfof-pYsva9-qixgap"
}

-- Función para obtener los primeros 10 registros de la tabla persona
obtenerPersonas :: Connection -> IO [Persona]
obtenerPersonas conn = query_ conn "SELECT id, nombre, correo, fecha_nacimiento FROM persona LIMIT 10"

-- Función para buscar personas que poseen un determinado libro
buscarPersonasPorLibro :: Connection -> String -> IO [Persona]
buscarPersonasPorLibro conn nombreLibroParcial = do
    query conn
        "SELECT persona.id, persona.nombre, persona.correo, persona.fecha_nacimiento \
        \FROM persona \
        \JOIN prestamo ON persona.id = prestamo.id_persona \
        \JOIN libro ON prestamo.id_libro = libro.id \
        \WHERE libro.titulo ILIKE ?" 
        [ "%" ++ nombreLibroParcial ++ "%" ]

-- Función para validar el formato del correo 
validarCorreo :: String -> Bool
validarCorreo correo = correo =~ ("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" :: String)

-- Función para ingresar un nuevo préstamo

ingresarPrestamo :: Connection -> Int -> Int -> IO ()
ingresarPrestamo conn idPersona idLibro = do
    fechaActual <- utctDay <$> getCurrentTime
    execute conn 
        "INSERT INTO prestamo (id_persona, id_libro, fecha_prestamo) VALUES (?, ?, ?)"
        (idPersona, idLibro, fechaActual)
    putStrLn "Préstamo ingresado exitosamente."

main :: IO ()
main = do
    conn <- connectDB
    putStrLn "Conectado a la base de datos PostgreSQL"

   

    -- Buscar personas que poseen un libro
    putStrLn "Ingrese parte del nombre del libro:"
    nombreLibroParcial <- getLine
    putStrLn "Estas personas tienen a prestamo ese libro:"
    personasLibro <- buscarPersonasPorLibro conn nombreLibroParcial
    mapM_ print personasLibro

    -- Ingresar nuevo préstamo
    putStrLn "Ingrese el correo de la persona que desea registrar el préstamo: "
    correoPersona <- getLine
    if validarCorreo correoPersona
        then do
            putStrLn "Ingrese el ID del libro que desea prestar: "
            idLibroStr <- getLine
            let idLibro = read idLibroStr :: Int
            -- Buscar persona por correo
            resultado <- query conn "SELECT id FROM persona WHERE correo = ?" (Only correoPersona) :: IO [Only Int]
            case listToMaybe resultado of
                Just (Only idPersona) -> ingresarPrestamo conn idPersona idLibro
                Nothing -> putStrLn "No se encontró una persona con ese correo."
        else
            putStrLn "El correo ingresado no es válido."

    -- Cierra la conexión
    close conn

