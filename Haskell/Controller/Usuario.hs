{-# LANGUAGE DeriveGeneric #-}

module Controller.Usuario where

import Data.Aeson 
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import System.Directory

data Usuario = Usuario {
    idUsuario :: String,
    senha :: String,
    seguidores :: [Usuario],
    seguindo :: [Usuario]
} deriving (Show, Generic)

instance ToJSON Usuario 
instance FromJSON Usuario

cadastraUsuario ::  String->  String-> IO ()
cadastraUsuario nome senha = do
    let usuario = Usuario nome senha [] []
    maybeJson <- recuperaUsuarios
    adicionaUsuario (fromJust maybeJson) usuario

adicionaUsuario :: [Usuario] -> Usuario -> IO()
adicionaUsuario usuarios usuario = do
    let novosUsuarios = encodePretty (usuario : usuarios)
    BS.writeFile "Data/temp.json" novosUsuarios
    removeFile "Data/usuarios.json"
    renameFile "Data/temp.json" "Data/usuarios.json"
        
recuperaUsuarios :: IO (Maybe [Usuario])
recuperaUsuarios = do
    arquivo <- BS.readFile "Data/usuarios.json"
    return (decode arquivo)


-- escreveUsuario :: Usuario -> IO()
--escreveUsuario usuario = BS.writeFile "Data/usuarios.json" (encode (concatenaUsuarios usuario)) where
 --   concatenaUsuarios usuario = usuario : recuperaUsuarios



