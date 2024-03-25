{-# LANGUAGE DeriveGeneric #-}

module Controller.Estante where

import Data.Aeson 
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import System.Directory
import System.IO.Unsafe

data Estante = Lendo | Lidos | Pretendo_Ler | Abandonados {
    livros :: [Livro],
} deriving (Show, Generic)

instance ToJSON Usuario 
instance FromJSON Usuario
{- Adiciona um livro já existente no sistema  a um tipo de estante-}
adicionaLivro :: String -> Estante -> Livro
if procuraLivro nome Livro.livros  != Nothing then
    case estante of
        Lendo ->
            adicionaLivro nome estante = do
            let novoLivro = encodePretty (entrada : estante)
            BS.writefile "Data/temp.json" novoLivro
            removeFile "Data/lendo.json"
            renameFile "Data/temp.json" "Data/lendo.json"
        Lidos ->
            adicionaLivro nome estante = do
            let novoLivro = encodePretty (entrada : estante)
            BS.writefile "Data/temp.json" novoLivro
            removeFile "Data/lidos.json"
            renameFile "Data/temp.json" "Data/lidos.json"   
        Pretendo_Ler ->
            adicionaLivro nome estante = do
            let novoLivro = encodePretty (entrada : estante)
            BS.writefile "Data/temp.json" novoLivro
            removeFile "Data/pretendo_ler.json"
            renameFile "Data/temp.json" "Data/pretendo_ler.json"
        Abandonados ->
            adicionaLivro nome estante = do
            let novoLivro = encodePretty (entrada : estante)
            BS.writefile "Data/temp.json" novoLivro
            removeFile "Data/abandonados.json"
            renameFile "Data/temp.json" "Data/abandonados.json" 


{-Procura um livro no sistema e o recupera-}
procuraLivro :: String -> [Livro] -> Maybe Livro
procuraLivro [] _ = Nothing
procuraLivro nome (x:xs) | nome == (Livro.nome x) = livro
                                | otherwise = procuraLivro nome xs
                