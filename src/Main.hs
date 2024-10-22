{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Time (Day)

-- Estructura de datos para representar una persona
data Persona = Persona {
    idPersona :: Int,
    nombrePersona :: String,
    correoPersona :: String,
    fechaNacimientoPersona :: Maybe Day
} deriving (Show)

-- Instancia para convertir las filas de la consulta en la estructura Persona
instance FromRow Persona where
    fromRow = Persona <$> field <*> field <*> field <*> field

-- Conectar a la base de datos PostgreSQL
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

main :: IO ()
main = do
    conn <- connectDB
    putStrLn "Conectado a la base de datos PostgreSQL"

    -- Ejecuta la consulta para obtener 10 personas
    personas <- obtenerPersonas conn

    -- Imprime los resultados
    mapM_ print personas

    -- Cierra la conexión
    close conn
