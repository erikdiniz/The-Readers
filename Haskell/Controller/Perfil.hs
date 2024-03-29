{-# LANGUAGE DeriveGeneric #-}
module Controller.Perfil where
import Controller.Usuario
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
    biografia :: String,
    id :: String
} deriving (Show, Generic)

-- Cria um perfil no sistema
criarPerfil :: String -> String -> String -> IO ()
criarPerfil nome biografia id = do
  let perfil = Perfil nome biografia id
  maybeJson <- recuperaPerfis
  salvaPerfil (fromJust maybeJson) perfil

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

visaoGeral :: String -> IO ()
visaoGeral userId = do
    maybePerfis <- recuperaPerfis
    case maybePerfis of
      Nothing -> putStrLn "Perfil não encontrado."
      Just perfis -> do
        perfilUsuario <- procuraPerfilUnsafe userId perfis
        putStrLn $ "--------------------------------------" ++ "\n"
        putStrLn $ "|        MEU PERFIL THE READER        |" ++ "\n"
        putStrLn $ "--------------------------------------" ++ "\n"

        putStrLn $ "MEU NOME: " ++ nome perfilUsuario
        putStrLn $ "SOBRE MIM... " ++ biografia perfilUsuario

visaoStalker :: String -> IO ()
visaoStalker userVisitado = do
    maybePerfis <- recuperaPerfis
    case maybePerfis of
      Nothing -> putStrLn "Perfil não encontrado."
      Just perfis -> do
        perfilVisitado <- procuraPerfilUnsafe userVisitado perfis
        putStrLn $ "--------------------------------------" ++ "\n"
        putStrLn $ "|        CONHEÇA ESSE READER :)       |" ++ "\n"
        putStrLn $ "--------------------------------------" ++ "\n"

        putStrLn $ "NOME: " ++ nome perfilVisitado
        putStrLn $ "SOBRE..." ++ biografia perfilVisitado


procuraPerfilUnsafe :: String -> [Perfil] -> IO Perfil
procuraPerfilUnsafe userId [] = error "Usuário não encontrado."
procuraPerfilUnsafe userId (x:xs) = if (Controller.Perfil.id) x == userId then return x else procuraPerfilUnsafe userId xs



