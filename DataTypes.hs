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
  let nombre =  getNombreEmpresa (elemento)
  let website =  getWebsiteEmpresa (elemento)
  let contacto = getContactoEmpresa (elemento)
  let pedal = getPedalEmpresa (elemento)
  let electrico = getElectricoEmpresa (elemento)
  print (unpack nombre ++ "       " ++ unpack website ++ "      " ++ unpack contacto ++ "     " ++ show pedal ++ "    " ++ show electrico)

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
  let lista= [unpack nombre,unpack direccion,unpack provincia,show x,show y]
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
  let lista= [unpack id,unpack tipo,unpack parqueo]
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
  print (show cedula ++ "  " ++ unpack nombre)

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