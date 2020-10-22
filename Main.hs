{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Int (Int64)
import Data.Text (Text, pack)
import Data.Tuple.Only
import DataTypes
import Database.SQLite.Simple
  ( Only,
    close,
    execute,
    open,
    query,
    query_,
  )
import Database.SQLite.SimpleErrors (runDBAction)
import FileHandler (printSubLista, parsearLista, split)
import System.Exit
import System.IO

opcionesGenerales :: IO ()
opcionesGenerales = do
  putStrLn "\n\n=================================Opciones Generales=================================\n\n"
  putStrLn "1. Consultar bicicletas.\n2. Alquilar. \n3. Facturar. \n4. Consulta de factura. \n5. Volver.\n"
  putStrLn "Ingrese la opcion"
  putStr ">>"
  hFlush stdout
  opcion <- getLine
  if (opcion == "1")
    then do
      putStrLn "1"
      opcionesGenerales
    else
      if (opcion == "2")
        then do
          putStrLn "2"
          opcionesGenerales
        else
          if (opcion == "3")
            then do
              opcionesGenerales
            else
              if (opcion == "4")
                then do
                  opcionesGenerales
                else
                  if (opcion == "5")
                    then do
                      return ()
                    else do
                      putStrLn "ERROR: Opción incorrecta, intentelo de nuevo"
                      opcionesGenerales

opcionesOperativas :: IO ()
opcionesOperativas = do
  putStrLn "\n\n=================================Opciones Operativas=================================\n\n"
  putStrLn "1. Mostrar parqueos.\n2. Mostrar bicicletas. \n3. Mostrar usuarios. \n4. Estadisticas. \n5. Volver.\n"
  putStrLn "Ingrese la opcion"
  putStr ">>"
  hFlush stdout
  opcion <- getLine
  if (opcion == "1")
    then do
      printearParqueos
      opcionesOperativas
    else
      if (opcion == "2")
        then do
          printearBicicletas
          opcionesOperativas
        else
          if (opcion == "3")
            then do
              opcionesOperativas
            else
              if (opcion == "4")
                then do
                  opcionesOperativas
                else
                  if (opcion == "5")
                    then do
                      return ()
                    else do
                      putStrLn "ERROR: Opción incorrecta, intentelo de nuevo"
                      opcionesOperativas

---------------------------------------------------------------------------------------------
printBicicletasParqueo parqueo = do
  conn <- open "PR2.db"
  let nombre = getNombreParqueo (parqueo)
  q <- query conn "SELECT * from Bicicleta where Parqueo=? and estado=0;" (Only (nombre :: Text)) :: IO [Bicicleta]
  let headParqueo=["Nombre","Ubicacion","Provincia","X","Y"]
  let headBicicleta=["Identificador","Tipo","Parqueo"]
  printSubLista headParqueo 0
  putStr ("\n")
  printParqueos parqueo
  putStr ("\n")
  putStrLn "Bicicletas :\n"
  printSubLista headBicicleta 0
  putStr ("\n")
  mapM_ printBicicletas q
  putStr ("------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
  putStr ("\n\n")
  close conn

printearParqueos = do
  conn <- open "PR2.db"
  putStr ("\n\n")
  putStrLn "Ingrese la Provincia"
  putStr ">>"
  hFlush stdout
  provincia <- getLine
  putStr ("\n")
  q <- query conn "SELECT * from Parqueo where Provincia=?;" (Only (pack provincia :: Text)) :: IO [Parqueo]
  close conn
  if (null q)
    then do
      putStr "\n No hay parqueos en esta provincia \n"
    else mapM_ printBicicletasParqueo q

printearBicicletas=do
  putStr ("\n\n")
  putStrLn "Ingrese el comando a ejecutar"
  putStr ">>"
  hFlush stdout
  opcion <- getLine 
  conn <- open "PR2.db"
  let headBicicleta=["Identificador","Tipo","Parqueo"]
  putStr ("\n")
  printSubLista headBicicleta 0
  putStr ("\n")
  if(opcion=="#") then do
    q <- query_ conn "SELECT * from Bicicleta"  :: IO [Bicicleta]
    mapM_ printBicicletas q
  else if (opcion=="transito") then do
    q <- query_ conn "SELECT * from Bicicleta where  estado=1;"  :: IO [Bicicleta]
    mapM_ printBicicletas q
  else do   
    q <- query conn "SELECT * from Bicicleta where Parqueo=? and estado=0;" (Only (pack opcion :: Text)) :: IO [Bicicleta]
    if (null q)
    then do
      putStr "\n No hay parqueos con este nombre \n"
    else mapM_ printBicicletas q
  close conn



menuPrincipal :: IO ()
menuPrincipal = do
  putStrLn "\n\n=================================Menu general=================================\n\n"
  putStrLn "1. Opciones Operativas.\n2. Opciones Generales.\n3. Salir del programa.\n"
  putStrLn "Ingrese la opcion"
  putStr ">>"
  hFlush stdout
  opcion <- getLine
  if (opcion == "1")
    then do
      opcionesOperativas
      menuPrincipal
    else
      if (opcion == "2")
        then do
          opcionesGenerales
          menuPrincipal
        else
          if (opcion == "3")
            then do
              exitSuccess
            else do
              putStrLn "ERROR: Opción incorrecta, intentelo de nuevo"
              menuPrincipal

cargarUsuarios = do
  putStrLn "Ingrese la ruta del archivo de los usuarios"
  putStr ">>"
  hFlush stdout
  ruta <- getLine
  contenido <- readFile ruta
  let lista = split '\n' contenido
  let valores = parsearLista (lista, 0, [])
  mapM_ getUsuarioInsertar valores

cargarParqueos = do
  putStrLn "Ingrese la ruta del archivo de los Parqueos"
  putStr ">>"
  hFlush stdout
  ruta <- getLine
  contenido <- readFile ruta
  let lista = split '\n' contenido
  let valores = parsearLista (lista, 0, [])
  mapM_ getParqueoInsertar valores

cargarBicicletas = do
  putStrLn "Ingrese la ruta del archivo de las Bicletas"
  putStr ">>"
  hFlush stdout
  ruta <- getLine
  contenido <- readFile ruta
  let lista = split '\n' contenido
  let valores = parsearLista (lista, 0, [])
  mapM_ getBicicletaInsertar valores

main :: IO ()
main = do
  cargarUsuarios
  cargarParqueos
  cargarBicicletas
  menuPrincipal