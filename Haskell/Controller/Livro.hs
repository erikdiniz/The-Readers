{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Controller.Livro where

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

instance FromJSON Livro
instance ToJSON Livro

-- Criando um tipo de dado chamado "Livro".
data Livro = Livro {
    nome :: String,
    autor :: String,
    n_paginas :: Int,
    genero :: String
}   deriving (Show, Generic)

-- Cadastra um livro no sistema
cadastraLivro :: String -> String -> Int -> String -> IO()
cadastraLivro nome autor n_paginas genero = do
    if livroJaExiste nome getListaLivros then
        putStrLn "Livro já cadastrado no sistema."
    else do
        let livro = Livro nome autor n_paginas genero
        maybeJson <- getListaLivros
        escreveLivro (fromJust maybeJson) livro
        putStrLn "Livro cadastrado."


-- Escreve um livro em um arquivo Json
escreveLivro :: [Livro] -> Livro -> IO()
escreveLivro listaLivros livro = do
    let novoLivro = encodePretty (livro : listaLivros)
    BL.writeFile "Data/temp.json" novoLivro
    removeFile "Data/livros.json"
    renameFile "Data/temp.json" "Data/livros.json"

getListaLivros :: IO (Maybe [Livro])
getListaLivros = do
    arquivo <- BL.readFile "Data/livros.json"
    return (decode arquivo)

getLivro :: String -> [Livro] -> Maybe Livro
getLivro _ [] = Nothing
getLivro nomeProcurado (h:t)
    | nome h == nomeProcurado = Just h
    | otherwise = getLivro nomeProcurado t


livroJaExiste :: String -> IO(Maybe [Livro]) -> Bool
livroJaExiste nomeProcurado listaLivro =
    case maybeListaLivros of
        Just listaLivros -> case getLivro nomeProcurado listaLivros of
            Just _  -> True
            Nothing -> False
        Nothing -> False
    where
        maybeListaLivros = unsafePerformIO listaLivro
