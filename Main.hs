{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.IO
import System.Exit

import Data.Int (Int64)
import Data.Text (Text, pack)
import DataTypes
import FileHandler ( parsearLista, split, printListaGeneral )
import Database.SQLite.Simple
  ( close,
    execute,
    open,
    query_,
  )


opcionesGenerales::IO()
opcionesGenerales=do
    putStrLn "\n\n=================================Opciones Generales=================================\n\n"
    putStrLn "1. Consultar bicicletas.\n2. Alquilar. \n3. Facturar. \n4. Consulta de factura. \n5. Volver.\n"
    putStrLn "Ingrese la opcion"
    putStr ">>";
    hFlush stdout
    opcion <- getLine
    if (opcion == "1") then do 
          putStrLn "1" 
          opcionesGenerales
    else if (opcion == "2") then do 
          putStrLn "2"
          opcionesGenerales
    else if (opcion == "3") then do 
         opcionesGenerales
    else if (opcion == "4") then do 
         opcionesGenerales
    else if (opcion == "5") then do 
         return ()
    else do
      putStrLn "ERROR: Opción incorrecta, intentelo de nuevo"
      opcionesGenerales


opcionesOperativas::IO()
opcionesOperativas=do
    putStrLn "\n\n=================================Opciones Operativas=================================\n\n"
    putStrLn "1. Mostrar parqueos.\n2. Mostrar bicicletas. \n3. Mostrar usuarios. \n4. Estadisticas. \n5. Volver.\n"
    putStrLn "Ingrese la opcion"
    putStr ">>";
    hFlush stdout
    opcion <- getLine
    if (opcion == "1") then do 
          putStrLn "1" 
          opcionesOperativas
    else if (opcion == "2") then do 
          putStrLn "2"
          opcionesOperativas
    else if (opcion == "3") then do 
         opcionesOperativas
    else if (opcion == "4") then do 
         opcionesOperativas
    else if (opcion == "5") then do 
         return ()
    else do
      putStrLn "ERROR: Opción incorrecta, intentelo de nuevo"
      opcionesOperativas


menuPrincipal::IO()
menuPrincipal=do
    putStrLn "\n\n=================================Menu general=================================\n\n"
    putStrLn "1. Opciones Operativas.\n2. Opciones Generales.\n3. Salir del programa.\n"
    putStrLn "Ingrese la opcion"
    putStr ">>";
    hFlush stdout
    opcion <- getLine
    if (opcion == "1") then do 
          opcionesOperativas
          menuPrincipal
    else if (opcion == "2") then do 
          opcionesGenerales
          menuPrincipal
    else if (opcion == "3") then do 
         exitSuccess
    else do
      putStrLn "ERROR: Opción incorrecta, intentelo de nuevo"
      menuPrincipal


main :: IO ()
main = do
    menuPrincipal