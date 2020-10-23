{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Int (Int64)
import Data.List
import Data.Maybe
import Data.Text (Text, pack, unpack)
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
import FileHandler (parsearLista, printSubLista, split)
import System.Exit
import System.IO

distancia (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

calcularDistancias :: (Floating a1, Integral a2, Integral a3) => [Parqueo] -> [a1] -> Int -> a2 -> a3 -> [a1]
calcularDistancias parqueos res contador x1 y1 = do
  if (contador == length parqueos)
    then tail res
    else do
      let x2 = getXParqueo (parqueos !! contador)
      let y2 = getYParqueo (parqueos !! contador)
      let distance = distancia (fromIntegral x1, fromIntegral y1) (fromIntegral x2, fromIntegral y2)
      let respuesta = res ++ [distance]
      let conta = contador + 1
      calcularDistancias parqueos respuesta conta x1 y1

consultarBicicletas = do
  putStrLn "Ingrese un X "
  putStr ">>"
  hFlush stdout
  input1 <- getLine
  let x = read input1 :: Int64
  putStrLn "Ingrese un y "
  putStr ">>"
  hFlush stdout
  input2 <- getLine
  let y = read input2 :: Int64
  conn <- open "PR2.db"
  q <- query_ conn "SELECT * from Parqueo" :: IO [Parqueo]
  let listaRes = [1.0]
  let distancias = calcularDistancias q listaRes 0 x y
  let minimo = minimum distancias
  let indiceMinimoConJust = elemIndex minimo distancias
  let indiceMinimo = fromJust indiceMinimoConJust
  putStr ("\n")
  printBicicletasParqueo (q !! indiceMinimo)
  close conn

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
      consultarBicicletas
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
              printearUsuarios
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
  let headParqueo = ["Nombre", "Ubicacion", "Provincia", "X", "Y"]
  let headBicicleta = ["Identificador", "Tipo", "Parqueo"]
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

printearBicicletas = do
  putStr ("\n\n")
  putStrLn "Ingrese el comando a ejecutar"
  putStr ">>"
  hFlush stdout
  opcion <- getLine
  conn <- open "PR2.db"
  let headBicicleta = ["Identificador", "Tipo", "Parqueo"]
  putStr ("\n")
  printSubLista headBicicleta 0
  putStr ("\n")
  if (opcion == "#")
    then do
      q <- query_ conn "SELECT * from Bicicleta" :: IO [Bicicleta]
      mapM_ printBicicletas q
    else do
      q <- query_ conn "SELECT * from Bicicleta where  estado=1;" :: IO [Bicicleta]
      mapM_ printBicicletas q

  close conn

printearUsuarios = do
  putStr ("\n\n")
  putStrLn "Ingrese el comando a ejecutar"
  putStr ">>"
  hFlush stdout
  opcion <- getLine
  conn <- open "PR2.db"
  let headUsuario = ["Cedula", "Nombre completo"]
  putStr ("\n")
  printSubLista headUsuario 0
  putStr ("\n")
  if (opcion == "#")
    then do
      q <- query_ conn "SELECT * from Usuario" :: IO [Usuario]
      mapM_ printUsuarios q
    else do
      q <- query conn "SELECT * from Usuario where Cedula=? " (Only (read opcion :: Int64)) :: IO [Usuario]
      if (null q)
        then do
          putStr "\n No hay un Usuario que tenga esta Cedula \n"
        else mapM_ printUsuarios q

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