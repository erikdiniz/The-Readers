{-# LANGUAGE DeriveGeneric #-}
module Controller.Leitura (
    -- outros exports
    Leitura(..),
    recuperaLeituraDoUsuario,
    FromJSON(..)
) where

import Data.Aeson 
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (unwords)
import Data.Maybe
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import System.Directory
import System.IO.Unsafe
import Controller.Livro

{- Definindo os tipos de dados para representar um livro. -}
data Leitura = Leitura {
    idUsuario_lido :: String,
    titulo_lido :: String,
    autor_lido :: String,
    genero_lido :: String,
    dataLeitura :: String,
    numPaginas :: Int,
    nota :: Int
} deriving (Show, Generic)

instance ToJSON Leitura 
instance FromJSON Leitura

cadastrarLeitura :: String -> Livro -> String -> Int -> IO()
cadastrarLeitura idUsuario livro dataLeitura nota = do
    let leitura = Leitura idUsuario (nome livro) (autor livro) (genero livro) dataLeitura (n_paginas livro) nota
    let novasLeituras = encodePretty (leitura : recuperaLeituraUnsafe)
    BS.writeFile "Data/temp.json" novasLeituras
    removeFile "Data/leituras.json"
    renameFile "Data/temp.json" "Data/leituras.json"

recuperaLeituraUnsafe :: [Leitura]
recuperaLeituraUnsafe = do
    let arquivo = unsafePerformIO $ BS.readFile "Data/usuarios.json"
    let maybeJson = (decode arquivo) :: Maybe [Leitura]
    case maybeJson of
        Nothing -> []
        Just leituras -> leituras

recuperaLeituraDoUsuario :: String -> [Leitura]
recuperaLeituraDoUsuario idUsuario = [lido_por | lido_por <- recuperaLeituraUnsafe, idUsuario == (idUsuario_lido lido_por)]