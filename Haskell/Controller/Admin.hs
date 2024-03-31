{-# LANGUAGE DeriveGeneric #-}

module Controller.Admin where
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (unwords)
import Data.Maybe
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import System.Directory
import System.IO.Unsafe

data Admin = Admin {
    idAdm :: String,
    senha :: String
} deriving (Show, Generic)

instance ToJSON Admin
instance FromJSON Admin

cadastraAdm :: String -> String -> IO ()
cadastraAdm user senha = do
    let admin = Admin user senha
    maybeJson <- recuperaAdms
    salvaAdm (fromJust maybeJson) admin


loginAdm :: String -> String -> Maybe Admin
loginAdm user senha = do
    let admin = pegaAdm user
    case admin of
        Nothing -> Nothing
        Just admin -> valideSenha admin senha

valideSenha :: Admin -> String -> Maybe Admin
valideSenha adm senhaFornecida = if senha adm == senhaFornecida then Just adm else Nothing

salvaAdm :: [Admin] -> Admin -> IO()
salvaAdm admins adm = do
    let novosAdms = encodePretty (adm : admins)
    BS.writeFile "Data/temp.json" novosAdms
    removeFile "Data/administradores.json"
    renameFile "Data/temp.json" "Data/administradores.json"


recuperaAdms :: IO (Maybe [Admin])
recuperaAdms = do
    arquivo <- BS.readFile "Data/administradores.json"
    return (decode arquivo)

pegaAdm :: String -> Maybe Admin
pegaAdm user = do
    let admins = recuperaAdmsUnsafe
    procuraAdm user admins

recuperaAdmsUnsafe :: [Admin]
recuperaAdmsUnsafe = do
    let arquivo = unsafePerformIO $ BS.readFile "Data/administradores.json"
    let maybeJson = (decode arquivo) :: Maybe [Admin]
    case maybeJson of
        Nothing -> []
        Just administradores -> administradores

procuraAdm :: String -> [Admin] -> Maybe Admin
procuraAdm user [] = Nothing
procuraAdm user (x:xs) = if idAdm x == user then Just x else procuraAdm user xs



