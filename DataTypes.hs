{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import Data.Int (Int64)
import Data.Text
import Database.SQLite.Simple
import Database.SQLite.SimpleErrors (runDBAction)
import FileHandler

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Data types
data Parqueo = Parqueo
  { nombreP :: Text,
    direccionP :: Text,
    provinciaP :: Text,
    ubicacionX :: Int64,
    ubicacionY :: Int64
  }
  deriving (Eq, Read, Show)

data Empresa = Empresa
  { nombreE :: Text,
    websiteE :: Text,
    contactoE :: Text,
    pedalE :: Int64,
    electricoE :: Int64
  }
  deriving (Eq, Read, Show)

data Bicicleta = Bicicleta
  { idB :: Text,
    tipoB :: Text,
    parqueoB :: Text,
    estadoB :: Int64
  }
  deriving (Eq, Read, Show)

data Usuario = Usuario
  { cedulaU :: Int64,
    nombreU :: Text
  }
  deriving (Eq, Read, Show)

data Alquiler = Alquiler
  { salidaA :: Text,
    llegadaA :: Text,
    bicicletaA :: Text,
    estadoA :: Int64,
    usuarioA :: Int64
  }

data AlquilerP = AlquilerP
  { idAP :: Int64,
    salidaAP :: Text,
    llegadaAP :: Text,
    bicicletaAP :: Text,
    estadoAP :: Int64,
    usuarioAP :: Int64
  }
  deriving (Eq, Read, Show)

data Factura = Factura
  { alquilerF :: Int64,
    kilometrosF :: Int64,
    totalF :: Int64,
    estadoF :: Int64
  }
  deriving (Eq, Read, Show)

data FacturaP = FacturaP
  { idFP :: Int64,
    alquilerFP :: Int64,
    kilometrosFP :: Int64,
    totalFP :: Int64,
    estadoFP :: Int64
  }
  deriving (Eq, Read, Show)

data IdAlquiler = IdAlquiler
  {idAlquiler :: Int64}
  deriving (Eq, Read, Show)

data IdFactura = IdFactura
  {idFactura :: Int64}
  deriving (Eq, Read, Show)

data Top5Parqueos = Top5Parqueos
  { nombreParqueoTop :: Text,
    cantidadParqueoTop :: Int64
  }
  deriving (Eq, Read, Show)

data Top5Usuarios = Top5Usuarios
  { cedulaUsuarioTop :: Int64,
    nombreUsuarioTop :: Text,
    cantidadUsuarioTop :: Int64
  }
  deriving (Eq, Read, Show)

data Top3Bicicletas = Top3Bicicletas
  { idBicicletaTop :: Text,
    cantidadkmBicicletaTop :: Int64
  }
  deriving (Eq, Read, Show)

data Resumen = Resumen
  { cantidadViajesResumen :: Int64,
    cantidadkmResumen :: Int64,
    cantidadpagarBicicletaTop :: Int64
  }
  deriving (Eq, Read, Show)

data CantidadViajesResumenConsulta = CantidadViajesResumenConsulta
  { viajescantidadconsulta :: Int64
  }
  deriving (Eq, Read, Show)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--gets de la empresa e instancias
getNombreEmpresa (Empresa nombre _ _ _ _) = nombre

getWebsiteEmpresa (Empresa _ website _ _ _) = website

getContactoEmpresa (Empresa _ _ contacto _ _) = contacto

getPedalEmpresa (Empresa _ _ _ pedal _) = pedal

getElectricoEmpresa (Empresa _ _ _ _ electrico) = electrico

instance FromRow Empresa where
  fromRow = Empresa <$> field <*> field <*> field <*> field <*> field

instance ToRow Empresa where
  toRow (Empresa nombre website contacto pedal electrico) = toRow (nombre, website, contacto, pedal, electrico)

printEmpresas :: Empresa -> IO ()
printEmpresas elemento = do
  let nombre = getNombreEmpresa (elemento)
  let website = getWebsiteEmpresa (elemento)
  let contacto = getContactoEmpresa (elemento)
  let pedal = getPedalEmpresa (elemento)
  let electrico = getElectricoEmpresa (elemento)
  let lista = [unpack nombre, unpack website, unpack contacto, show pedal, show electrico]
  printSubLista lista 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--get de parqueos e instancias
getNombreParqueo (Parqueo nombre _ _ _ _) = nombre

getDireccionParqueo (Parqueo _ direccion _ _ _) = direccion

getProvincia (Parqueo _ _ provincia _ _) = provincia

getXParqueo (Parqueo _ _ _ x _) = x

getYParqueo (Parqueo _ _ _ _ y) = y

instance FromRow Parqueo where
  fromRow = Parqueo <$> field <*> field <*> field <*> field <*> field

instance ToRow Parqueo where
  toRow (Parqueo nombre direccion provincia x y) = toRow (nombre, direccion, provincia, x, y)

printParqueos :: Parqueo -> IO ()
printParqueos elemento = do
  let nombre = getNombreParqueo (elemento)
  let direccion = getDireccionParqueo (elemento)
  let provincia = getProvincia (elemento)
  let x = getXParqueo (elemento)
  let y = getYParqueo (elemento)
  let lista = [unpack nombre, unpack direccion, unpack provincia, show x, show y]
  printSubLista lista 0

insertarParqueo pnombre pdireccion pprovincia px py = do
  let nombre = pack pnombre
  let direccion = pack pdireccion
  let provincia = pack pprovincia
  let x = px
  let y = py
  conn <- open "PR2.db"
  result <- runDBAction $ execute conn "INSERT into Parqueo VALUES (?,?,?,?,?)" (Parqueo nombre direccion provincia x y)
  close conn

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--gets de la empresa e instancias
getIdBicicleta (Bicicleta id _ _ _) = id

getTipoBicicleta (Bicicleta _ tipo _ _) = tipo

getParqueoBicicleta (Bicicleta _ _ parqueo _) = parqueo

instance FromRow Bicicleta where
  fromRow = Bicicleta <$> field <*> field <*> field <*> field

instance ToRow Bicicleta where
  toRow (Bicicleta identificador tipo parqueo estado) = toRow (identificador, tipo, parqueo, estado)

printBicicletas :: Bicicleta -> IO ()
printBicicletas elemento = do
  let id = getIdBicicleta (elemento)
  let tipo = getTipoBicicleta (elemento)
  let parqueo = getParqueoBicicleta (elemento)
  let lista = [unpack id, unpack tipo, unpack parqueo]
  printSubLista lista 0

insertarBicicleta pId pTipo pParqueo = do
  let id = pack pId
  let tipo = pack pTipo
  let parqueo = pack pParqueo
  conn <- open "PR2.db"
  result <- runDBAction $ execute conn "INSERT into Bicicleta VALUES (?,?,?,?)" (Bicicleta id tipo parqueo 0)
  close conn

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--gets de la empresa e instancias
getCedulaUsuario (Usuario cedula _) = cedula

getNombre (Usuario _ nombre) = nombre

instance FromRow Usuario where
  fromRow = Usuario <$> field <*> field

instance ToRow Usuario where
  toRow (Usuario cedula nombre) = toRow (cedula, nombre)

printUsuarios :: Usuario -> IO ()
printUsuarios elemento = do
  let cedula = getCedulaUsuario (elemento)
  let nombre = getNombre (elemento)
  let lista = [show cedula, unpack nombre]
  printSubLista lista 0

insertarUsuario pcedula pnombre = do
  let cedula = pcedula
  let nombre = pack pnombre
  conn <- open "PR2.db"
  result <- runDBAction $ execute conn "INSERT into Usuario VALUES (?,?)" (Usuario cedula nombre)
  close conn

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--parseadores de txt a base de datos

getUsuarioInsertar :: [[Char]] -> IO ()
getUsuarioInsertar lista = do
  let cedula = lista !! 0
  let cedulaParseada = read cedula :: Int64
  let nombre = lista !! 1
  insertarUsuario cedulaParseada nombre

getParqueoInsertar :: [[Char]] -> IO ()
getParqueoInsertar lista = do
  let nombre = lista !! 0
  let direccion = lista !! 1
  let provincia = lista !! 2
  let x = read (lista !! 3) :: Int64
  let y = read (lista !! 4) :: Int64
  insertarParqueo nombre direccion provincia x y

getBicicletaInsertar :: [[Char]] -> IO ()
getBicicletaInsertar lista = do
  let id = lista !! 0
  let tipo = lista !! 1
  let ubicacion = lista !! 2
  insertarBicicleta id tipo ubicacion

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow Alquiler where
  fromRow = Alquiler <$> field <*> field <*> field <*> field <*> field

instance ToRow Alquiler where
  toRow (Alquiler salidaA llegadaA bicicletaA estadoA usuarioA) = toRow (salidaA, llegadaA, bicicletaA, estadoA, usuarioA)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow AlquilerP where
  fromRow = AlquilerP <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow AlquilerP where
  toRow (AlquilerP idAP salidaA llegadaA bicicletaA estadoA usuarioA) = toRow (idAP, salidaA, llegadaA, bicicletaA, estadoA, usuarioA)

getIdAlquiler (AlquilerP id _ _ _ _ _) = id

getSalidaAlquiler (AlquilerP _ salida _ _ _ _) = salida

getLlegadaAlquiler (AlquilerP _ _ llegada _ _ _) = llegada

getBicicletaAlquiler (AlquilerP _ _ _ bicicleta _ _) = bicicleta

getUsuarioAlquiler (AlquilerP _ _ _ _ _ usuario) = usuario

printAlquileres :: AlquilerP -> IO ()
printAlquileres elemento = do
  let id = getIdAlquiler (elemento)
  let salida = getSalidaAlquiler (elemento)
  let llegada = getLlegadaAlquiler (elemento)
  let bicicleta = getBicicletaAlquiler (elemento)
  let lista = [show id, unpack salida, unpack llegada, unpack bicicleta]
  printSubLista lista 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow Factura where
  fromRow = Factura <$> field <*> field <*> field <*> field

instance ToRow Factura where
  toRow (Factura alquilerF kilometrosF totalF estadoF) = toRow (alquilerF, kilometrosF, totalF, estadoF)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
instance FromRow FacturaP where
  fromRow = FacturaP <$> field <*> field <*> field <*> field <*> field

instance ToRow FacturaP where
  toRow (FacturaP idFP alquilerFP kilometrosFP totalFP estadoFP) = toRow (idFP, alquilerFP, kilometrosFP, totalFP, estadoFP)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

getIdFactura (FacturaP id _ _ _ _) = id

getAlquilerFactura (FacturaP _ alquiler _ _ _) = alquiler

getKilometrosFactura (FacturaP _ _ kilometros _ _) = kilometros

getTotalFactura (FacturaP _ _ _ total _) = total

printFacturas :: FacturaP -> IO ()
printFacturas elemento = do
  let id = getIdFactura (elemento)
  let alquiler = getAlquilerFactura (elemento)
  let kilometros = getKilometrosFactura (elemento)
  let total = getTotalFactura (elemento)
  let lista = [show id, show alquiler, show kilometros, show total]
  printSubLista lista 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow IdAlquiler where
  fromRow = IdAlquiler <$> field

getIdAlquiler2 (IdAlquiler id) = id

instance FromRow IdFactura where
  fromRow = IdFactura <$> field

getIdFactura2 (IdFactura id) = id

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow Top5Parqueos where
  fromRow = Top5Parqueos <$> field <*> field

instance ToRow Top5Parqueos where
  toRow (Top5Parqueos nombre cantidad) = toRow (nombre, cantidad)

getNombreParqueoTop (Top5Parqueos name _) = name

getCantidadParqueoTop (Top5Parqueos _ cantidad) = cantidad

printTopParqueos :: Top5Parqueos -> IO ()
printTopParqueos elemento = do
  let name = getNombreParqueoTop (elemento)
  let cantidad = getCantidadParqueoTop (elemento)
  let lista = [unpack name, show cantidad]
  printSubLista lista 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow Top5Usuarios where
  fromRow = Top5Usuarios <$> field <*> field <*> field

getCedulaUsuarioTop (Top5Usuarios cedula _ _) = cedula

getNombreUsuarioTop (Top5Usuarios _ nombre _) = nombre

getCantidadUsuarioTop (Top5Usuarios _ _ cantidad) = cantidad

printTopUsuario :: Top5Usuarios -> IO ()
printTopUsuario elemento = do
  let nombre = getNombreUsuarioTop (elemento)
  let cedula = getCedulaUsuarioTop (elemento)
  let cantidad = getCantidadUsuarioTop (elemento)
  let lista = [show cedula, unpack nombre, show cantidad]
  printSubLista lista 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow Top3Bicicletas where
  fromRow = Top3Bicicletas <$> field <*> field

getIdBicicletasTop (Top3Bicicletas id _) = id

getCantidadBicicletasTop (Top3Bicicletas _ cantidad) = cantidad

printTopBicicletas :: Top3Bicicletas -> IO ()
printTopBicicletas elemento = do
  let id = getIdBicicletasTop (elemento)
  let cantidad = getCantidadBicicletasTop (elemento)
  let lista = [unpack id, show cantidad]
  printSubLista lista 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow Resumen where
  fromRow = Resumen <$> field <*> field <*> field

getViajesResumen (Resumen viajes _ _) = viajes

getKmResumen (Resumen _ km _) = km

getTotalResumen (Resumen _ _ total) = total

printResumen :: Resumen -> IO ()
printResumen elemento = do
  let viajes = getViajesResumen (elemento)
  let km = getKmResumen (elemento)
  let total = getTotalResumen (elemento)
  let lista = [show viajes, show km, show total]
  printSubLista lista 0

------------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow CantidadViajesResumenConsulta where
  fromRow = CantidadViajesResumenConsulta <$> field

getCantidadViajesConsulta (CantidadViajesResumenConsulta cantidad) = cantidad
