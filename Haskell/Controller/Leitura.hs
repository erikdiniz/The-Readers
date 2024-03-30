{-# LANGUAGE DeriveGeneric #-}
module Controller.Leitura where

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
    let arquivo = unsafePerformIO $ BS.readFile "Data/leituras.json"
    let maybeJson = (decode arquivo) :: Maybe [Leitura]
    case maybeJson of
        Nothing -> []
        Just leituras -> leituras

recuperaLeitura :: String -> String -> Maybe Leitura
recuperaLeitura idUsuario titulo_leitura = do
    let leituras = recuperaLeituraDoUsuario idUsuario
    let maybeLeitura = fromJust (procuraLeitura idUsuario titulo_leitura leituras)
    return maybeLeitura

recuperaLeituraSafe :: IO (Maybe [Leitura])
recuperaLeituraSafe = do
    arquivo <- BS.readFile "Data/leituras.json"
    return (decode arquivo)


procuraLeitura :: String -> String -> [Leitura] -> Maybe Leitura
procuraLeitura idUsuario titulo_leitura [] = Nothing
procuraLeitura idUsuario titulo_leitura (x:xs) = if (idUsuario_lido x) == idUsuario && (titulo_lido x == titulo_leitura) then Just x else  procuraLeitura idUsuario titulo_leitura xs 

recuperaLeituraDoUsuario :: String -> [Leitura]
recuperaLeituraDoUsuario idUsuario = do 
    let tudo = recuperaLeituraUnsafe
    [lido_por | lido_por <- tudo, idUsuario == (idUsuario_lido lido_por)]

recuperaLeituraDoUsuarioSafe :: String -> [Leitura] -> [Leitura]
recuperaLeituraDoUsuarioSafe idUsuario todasAsLeituras = [lido_por | lido_por <- todasAsLeituras, idUsuario == (idUsuario_lido lido_por)]


recuperaNomeDasLeiturasDoUsuario :: String -> [String]
recuperaNomeDasLeiturasDoUsuario idUsuario = do
    let leituras = recuperaLeituraDoUsuario idUsuario
    let lista = concatenaLidos leituras []
    lista

concatenaLidos :: [Leitura] -> [String] -> [String]
concatenaLidos [x] titulos = titulos ++ [titulo_lido x]
concatenaLidos (x:xs) titulos = concatenaLidos xs (titulos ++ [titulo_lido x]) 

