{-# LANGUAGE DeriveGeneric #-}
module Controller.Perfil where

import System.Directory
import GHC.Generics
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Encode.Pretty (encodePretty)
import System.IO.Unsafe
import System.IO
import Data.Bool (Bool)
import Data.Maybe (fromMaybe)

instance FromJSON Perfil
instance ToJSON Perfil


data Perfil = Perfil {
    nome :: String,
    biografia :: String
} deriving (Show, Generic)

-- Cria um perfil no sistema
criarPerfil :: String -> String -> IO ()
criarPerfil nome biografia = do
  let perfil = Perfil nome biografia
  maybeJson <- recuperaPerfis
  salvaPerfil (fromJust maybeJson) perfil
  putStrLn "Perfil salvo com sucesso!"

-- Cria um perfil no arquivo Json
salvaPerfil :: [Perfil] -> Perfil -> IO ()
salvaPerfil perfis perfil = do
  let novoPerfil = encodePretty (perfil : perfis)
  BL.writeFile "Data/temp.json" novoPerfil
  removeFile "Data/perfis.json"
  renameFile "Data/temp.json" "Data/perfis.json"

recuperaPerfis :: IO (Maybe [Perfil])
recuperaPerfis = do
  arquivo <- BL.readFile "Data/perfis.json"
  return (decode arquivo)

visaoGeral :: IO ()
visaoGeral = do
  maybePerfis <- recuperaPerfis
  case maybePerfis of
    Nothing -> putStrLn "Nenhum perfil encontrado."
    Just perfis -> do
      let perfilUsuario = head perfis
      putStrLn $ "--------------------------------------" ++ "\n"
      putStrLn $ "|        MEU PERFIL THE READER        |" ++ "\n"
      putStrLn $ "--------------------------------------" ++ "\n"

      putStrLn $ "MEU NOME: " ++ nome perfilUsuario
      putStrLn $ "SOBRE MIM... " ++ biografia perfilUsuario