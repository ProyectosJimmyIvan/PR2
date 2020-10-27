{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.IORef
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

alquilar = do
  putStr ("\n\n")
  putStrLn "Ingrese un numero de cedula de usuario valido para empezar el tramite"
  putStr ">>"
  hFlush stdout
  cedula <- getLine
  putStr ("\n")
  conn <- open "PR2.db"
  usuario <- query conn "SELECT * from Usuario where Cedula=? " (Only (read cedula :: Int64)) :: IO [Usuario]
  close conn
  if (null usuario)
    then do
      putStrLn "\n No hay un usuario con esta cedula \n"
    else do
      conn <- open "PR2.db"
      q <- query_ conn "SELECT * from Parqueo" :: IO [Parqueo]
      putStrLn "Ingrese un X "
      putStr ">>"
      hFlush stdout
      input1 <- getLine
      putStr ("\n")
      let x = read input1 :: Int64
      putStrLn "Ingrese un y "
      putStr ">>"
      hFlush stdout
      input2 <- getLine
      putStr ("\n")
      let y = read input2 :: Int64
      let listaRes = [1.0]
      let distancias = calcularDistancias q listaRes 0 x y
      let minimo = minimum distancias
      let indiceMinimoConJust = elemIndex minimo distancias
      let indiceMinimo = fromJust indiceMinimoConJust
      let nombreParqueoSalida = getNombreParqueo (q !! indiceMinimo)
      putStr ("\n\n")
      print ("El parqueo mas cercano es: " ++ (unpack nombreParqueoSalida))
      putStr ("\n\n")
      let headParqueo = ["Nombre", "Ubicacion", "Provincia", "X", "Y"]
      printSubLista headParqueo 0
      mapM_ printParqueos q
      putStr ("\n\n")
      putStrLn "Ingrese un parqueo de llegada "
      putStr ">>"
      hFlush stdout
      nombreParqueoLlegada <- getLine
      putStr ("\n\n")
      parqueoLlegada <- query conn "SELECT * from Parqueo where  Nombre=?;" (Only (pack nombreParqueoLlegada :: Text)) :: IO [Parqueo]
      close conn
      if (null parqueoLlegada)
        then do
          putStrLn "\n No hay un parqueo con este nombre \n"
        else do
          if (unpack nombreParqueoSalida == nombreParqueoLlegada)
            then do
              putStrLn "\n No se puede llegar al parqueo del que salio\n"
            else do
              printBicicletasParqueo (q !! indiceMinimo)
              putStrLn "Ingrese el identificador de la bicicleta a alquilar "
              putStr ">>"
              hFlush stdout
              bicicletaName <- getLine
              putStr ("\n")
              putStr ("\n\n")
              connection <- open "PR2.db"
              bicletaLista <- query connection "SELECT * from Bicicleta where Identificador=? and Parqueo=? and estado=0;" (pack bicicletaName :: Text, nombreParqueoSalida :: Text) :: IO [Bicicleta]
              close connection
              if (null bicletaLista)
                then do
                  putStrLn "\n No hay bicicletas con este id  \n"
                else do
                  connection <- open "PR2.db"
                  let nombreParqueoLlegadaText = pack nombreParqueoLlegada
                  let bicicletaNameText = pack bicicletaName
                  let cedulaInt64 = read cedula :: Int64
                  let estado = 1 :: Int64
                  execute conn "Insert into Alquiler(Salida,Destino,Bicicleta,Estado,Usuario) values (?,?,?,?,?)" (Alquiler nombreParqueoSalida nombreParqueoLlegadaText bicicletaNameText estado cedulaInt64)
                  execute conn "Update Bicicleta set Estado=1 where identificador=?" (Only (pack bicicletaName :: Text))
                  listaIdAlquiler <- query_ conn "select max(ID) from Alquiler" :: IO [IdAlquiler]
                  let idAlquiler = getIdAlquiler2 (listaIdAlquiler !! 0)
                  putStrLn "\nSe ha creado el alquiler, estos son los datos: "
                  putStr ("\n\n")
                  let headAlquiler = ["ID", "Salida", "Llegada", "Bicicleta", "Usuario"]
                  printSubLista headAlquiler 0
                  let listaAlquiler = [show idAlquiler, unpack nombreParqueoSalida, nombreParqueoLlegada, bicicletaName, cedula]
                  putStr ("\n")
                  printSubLista listaAlquiler 0
                  close connection

facturar = do
  putStr ("\n\n")
  putStrLn "Ingrese el numero de alquiler valido para empezar el tramite"
  putStr ">>"
  hFlush stdout
  numeroAlquiler <- getLine
  putStr ("\n")
  conn <- open "PR2.db"
  listaAlquiler <- query conn "select * from Alquiler where estado=1 and Id=?" (Only (read numeroAlquiler :: Int64)) :: IO [AlquilerP]
  close conn
  if (null listaAlquiler)
    then do
      putStrLn "\n Lo sentimos, el alquiler que quiere facturar no existe o ya fue facturado \n"
    else do
      let bicicleta = getBicicletaAlquiler (listaAlquiler !! 0)
      let parqueoLlegada = getLlegadaAlquiler (listaAlquiler !! 0)
      let parqueoSalida = getSalidaAlquiler (listaAlquiler !! 0)
      let idAlquiler = getIdAlquiler (listaAlquiler !! 0)
      conn <- open "PR2.db"
      execute conn "Update Alquiler set Estado=0 where Id=?;" (Only (read numeroAlquiler :: Int64))
      execute conn "update Bicicleta set Estado=0, Parqueo=? where Identificador=?" (parqueoLlegada, bicicleta)
      bicletaLista <- query conn "SELECT * from Bicicleta where Identificador=?" (Only (bicicleta :: Text)) :: IO [Bicicleta]
      let tipoBici = getTipoBicicleta (bicletaLista !! 0)
      listaEmpresa <- query_ conn "select *  from Empresa" :: IO [Empresa]
      let pedalEmpresa = getPedalEmpresa (listaEmpresa !! 0)
      let electricoEmpresa = getElectricoEmpresa (listaEmpresa !! 0)
      let enteroElectrico = fromIntegral electricoEmpresa
      variableMutable <- newIORef (0 :: Int)
      if (unpack tipoBici == "AE")
        then do
          modifyIORef variableMutable (+ enteroElectrico)
          putStr "\n"
        else do
          modifyIORef variableMutable (+ fromIntegral pedalEmpresa)
          putStr ("\n")
      montoKilometro <- readIORef variableMutable
      let montoKilometroStr = show montoKilometro
      parqueoLlegadaLista <- query conn "SELECT * from Parqueo where  Nombre=?;" (Only (parqueoLlegada :: Text)) :: IO [Parqueo]
      parqueoSalidaLista <- query conn "SELECT * from Parqueo where  Nombre=?;" (Only (parqueoSalida :: Text)) :: IO [Parqueo]
      let x1 = getXParqueo (parqueoLlegadaLista !! 0)
      let x2 = getXParqueo (parqueoSalidaLista !! 0)
      let y1 = getYParqueo (parqueoLlegadaLista !! 0)
      let y2 = getYParqueo (parqueoSalidaLista !! 0)
      let distance = round (distancia (fromIntegral x1, fromIntegral y1) (fromIntegral x2, fromIntegral y2))
      let montoTotal = distance * read montoKilometroStr :: Int64
      execute conn "Insert into Factura(alquiler,cantidadkilometros,totalpagar,Estado) values (?,?,?,?)" (Factura idAlquiler distance montoTotal 1)
      listaMaxFactura <- query_ conn "SELECT max(Id) from  Factura;" :: IO [IdFactura]
      let idfactura = getIdFactura2 (listaMaxFactura !! 0)
      listaFactura <- query conn "SELECT * from  Factura where Id=?;" (Only (idfactura)) :: IO [FacturaP]
      putStrLn "\n Se ha facturado el alquiler correctamente!  \n\n"
      listaEmpresa <- query_ conn "select *  from Empresa" :: IO [Empresa]
      let alquilerid = getAlquilerFactura (listaFactura !! 0)
      listaAlquiler <- query conn "select * from Alquiler where   Id=?" (Only (alquilerid)) :: IO [AlquilerP]
      let bicicleta = getBicicletaAlquiler (listaAlquiler !! 0)
      bicletaLista <- query conn "SELECT * from Bicicleta where Identificador=?" (Only (bicicleta :: Text)) :: IO [Bicicleta]
      let tipobici = getTipoBicicleta (bicletaLista !! 0)
      let idbici = getIdBicicleta (bicletaLista !! 0)
      let parqueoLlegada = getLlegadaAlquiler (listaAlquiler !! 0)
      let parqueoSalida = getSalidaAlquiler (listaAlquiler !! 0)
      let kilometros = getKilometrosFactura (listaFactura !! 0)
      let totalColones = getTotalFactura (listaFactura !! 0)
      let pedalEmpresa = getPedalEmpresa (listaEmpresa !! 0)
      let electricoEmpresa = getElectricoEmpresa (listaEmpresa !! 0)
      let enteroElectrico = fromIntegral electricoEmpresa
      variableMutable <- newIORef (0 :: Int)
      if (unpack tipobici == "AE")
        then do
          modifyIORef variableMutable (+ enteroElectrico)
        else do
          modifyIORef variableMutable (+ fromIntegral pedalEmpresa)
      putStrLn ("\n ID de la Factura: " ++ show (idfactura) ++ "\n\n")
      putStrLn ("Empresa : \n")
      let headEmpresa = ["Nombre", "Website", "Contacto", "KM/Electrica", "KM/Tradicional"]
      printSubLista headEmpresa 0
      putStr ("\n")
      printEmpresas (listaEmpresa !! 0)
      putStr ("\n\n")
      tarifa <- readIORef variableMutable
      let listaFacturaImpresion = [unpack parqueoSalida, unpack parqueoLlegada, unpack idbici, unpack tipobici, show kilometros, show tarifa, show totalColones]
      let headFactura = ["Salida", "Llegada", "Bicicleta", "Tipo", "Cantidad de Kilometros", "Tarifa/KM", "Total Factura"]
      putStrLn ("Factura : \n")
      printSubLista headFactura 0
      putStr ("\n\n")
      printSubLista listaFacturaImpresion 0
      putStr ("\n")
      close conn

mostrarFactura = do
  putStr ("\n\n")
  putStrLn "Ingrese el numero de factura  para consultar"
  putStr ">>"
  hFlush stdout
  idFactura <- getLine
  putStr ("\n")
  conn <- open "PR2.db"
  let headEmpresa = ["Nombre", "Website", "Contacto", "KM/Electrica", "KM/Tradicional"]
  listaFactura <- query conn "SELECT * from  Factura where Id=?;" (Only (read idFactura :: Int64)) :: IO [FacturaP]
  if (null listaFactura)
    then do
      putStrLn "\n Lo sentimos, la factura no existe \n"
    else do
      listaEmpresa <- query_ conn "select *  from Empresa" :: IO [Empresa]
      let alquilerid = getAlquilerFactura (listaFactura !! 0)
      listaAlquiler <- query conn "select * from Alquiler where   Id=?" (Only (alquilerid)) :: IO [AlquilerP]
      let bicicleta = getBicicletaAlquiler (listaAlquiler !! 0)
      bicletaLista <- query conn "SELECT * from Bicicleta where Identificador=?" (Only (bicicleta :: Text)) :: IO [Bicicleta]
      let tipobici = getTipoBicicleta (bicletaLista !! 0)
      let idbici = getIdBicicleta (bicletaLista !! 0)
      let parqueoLlegada = getLlegadaAlquiler (listaAlquiler !! 0)
      let parqueoSalida = getSalidaAlquiler (listaAlquiler !! 0)
      let kilometros = getKilometrosFactura (listaFactura !! 0)
      let totalColones = getTotalFactura (listaFactura !! 0)
      let pedalEmpresa = getPedalEmpresa (listaEmpresa !! 0)
      let electricoEmpresa = getElectricoEmpresa (listaEmpresa !! 0)
      let enteroElectrico = fromIntegral electricoEmpresa
      variableMutable <- newIORef (0 :: Int)
      if (unpack tipobici == "AE")
        then do
          modifyIORef variableMutable (+ enteroElectrico)
        else do
          modifyIORef variableMutable (+ fromIntegral pedalEmpresa)
      putStrLn ("\n ID de la Factura: " ++ idFactura ++ "\n\n")
      putStrLn ("Empresa : \n")
      printSubLista headEmpresa 0
      putStr ("\n")
      printEmpresas (listaEmpresa !! 0)
      putStr ("\n\n")
      tarifa <- readIORef variableMutable
      let listaFacturaImpresion = [unpack parqueoSalida, unpack parqueoLlegada, unpack idbici, unpack tipobici, show kilometros, show tarifa, show totalColones]
      let headFactura = ["Salida", "Llegada", "Bicicleta", "Tipo", "Cantidad de Kilometros", "Tarifa/KM", "Total Factura"]
      putStrLn ("Factura : \n")
      printSubLista headFactura 0
      putStr ("\n\n")
      printSubLista listaFacturaImpresion 0
      putStr ("\n")

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
          alquilar
          opcionesGenerales
        else
          if (opcion == "3")
            then do
              facturar
              opcionesGenerales
            else
              if (opcion == "4")
                then do
                  mostrarFactura
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
        else do
          mapM_ printUsuarios q
          putStr ("\n\n")
          putStrLn "Alquileres:"
          putStr ("\n")
          let headAlquileres = ["ID", "Salida", "Destino", "Bicicleta"]
          alquileres <- query conn "SELECT * from Alquiler where Usuario=?;" (Only (read opcion :: Int64)) :: IO [AlquilerP]
          if (null alquileres)
            then do
              putStr "\n Este usaurio no tiene alquileres \n"
            else do
              printSubLista headAlquileres 0
              mapM_ printAlquileres alquileres

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
  putStr ("\n")
  cargarParqueos
  putStr ("\n")
  cargarBicicletas
  putStr ("\n")
  menuPrincipal