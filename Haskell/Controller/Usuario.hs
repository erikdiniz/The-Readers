{-# LANGUAGE DeriveGeneric #-}

module Controller.Usuario where

import Data.Aeson 
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import System.Directory
import System.IO.Unsafe
import Controller.Perfil

data Usuario = Usuario {
    idUsuario :: String,
    senha :: String,
    seguidores :: [Usuario],
    seguindo :: [Usuario]
} deriving (Show, Generic)

instance ToJSON Usuario 
instance FromJSON Usuario

{- Cria um usuário e chama o método de adicionar ele nos cadastrados-}
cadastraUsuario ::  String->  String-> IO ()
cadastraUsuario nome senha = do
    let usuario = Usuario nome senha [] []
    maybeJson <- recuperaUsuarios
    adicionaUsuario (fromJust maybeJson) usuario
    criarPerfil nome ""

loginUsuario :: String -> String -> Maybe Usuario
loginUsuario nome senha = do
    let usuario = pegaUsuario nome 
    case usuario of
        Nothing -> Nothing
        Just usuario -> valideSenha usuario senha

valideSenha :: Usuario -> String -> Maybe Usuario
valideSenha usuario senhaFornecida = if senha usuario == senhaFornecida then Just usuario else Nothing

{-Escreve na lista inteira de usuarios um usuario novo-}
adicionaUsuario :: [Usuario] -> Usuario -> IO()
adicionaUsuario usuarios usuario = do
    let novosUsuarios = encodePretty (usuario : usuarios)
    BS.writeFile "Data/temp.json" novosUsuarios
    removeFile "Data/usuarios.json"
    renameFile "Data/temp.json" "Data/usuarios.json"

{- Pega a lista inteira de usuários cadastrados -}
recuperaUsuarios :: IO (Maybe [Usuario])
recuperaUsuarios = do
    arquivo <- BS.readFile "Data/usuarios.json"
    return (decode arquivo)

{- Recupera usuarios cadastrados fora do contexto IO -}
recuperaUsuariosUnsafe :: [Usuario]
recuperaUsuariosUnsafe = do
    let arquivo = unsafePerformIO $ BS.readFile "Data/usuarios.json"
    let maybeJson = (decode arquivo) :: Maybe [Usuario]
    case maybeJson of
        Nothing -> []
        Just usuarios -> usuarios

pegaUsuario :: String -> Maybe Usuario
pegaUsuario nome = do
    let usuarios = recuperaUsuariosUnsafe
    procuraUsuario nome usuarios

procuraUsuario :: String -> [Usuario] -> Maybe Usuario
procuraUsuario nome [] = Nothing 
procuraUsuario nome (x:xs) = if idUsuario x == nome then Just x else procuraUsuario nome xs 



