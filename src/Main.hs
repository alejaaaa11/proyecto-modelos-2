{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Simple
import Control.Monad (void)

-- Conectar a la base de datos PostgreSQL
connectDB :: IO Connection
connectDB = connect defaultConnectInfo {
    connectHost = "databasehaskell.ckd83boidbuw.us-east-1.rds.amazonaws.com",   -- Cambia según tu configuración
    connectPort = 5432,
    connectDatabase = "libraryDB",
    connectUser = "postgres",
    connectPassword = "fugfof-pYsva9-qixgap"
}

main :: IO ()
main = do
    conn <- connectDB
    putStrLn "Conectado a la base de datos PostgreSQL"
    -- Aquí irán las consultas que desees ejecutar
    close conn