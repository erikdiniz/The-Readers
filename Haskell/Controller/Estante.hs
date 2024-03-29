{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Controller.Estante where


import GHC.Generics
import System.Directory
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Encode.Pretty (encodePretty)
import System.IO.Unsafe
import System.IO
import Data.Bool (Bool)
import Data.Maybe (fromMaybe)
import Controller.Livro

{-Criando estantes, que podem ser dos subtipos: Lendo, Lidos, Pretendo Ler e Abandonados-}
data Estante = Lendo | Lidos | Pretendo_Ler | Abandonados {
    livros :: [Livro],
} deriving (Show, Generic)

instance ToJSON Estante 
instance FromJSON Estante

{- Adiciona um livro já existente no sistema  a um tipo de estante-}
adicionaLivro :: String -> Estante -> String -> IO()
adicionaLivro nome estante = do
    if verificaLivro nome then
        case estante of
            Lendo ->
                adicionaLivro nome estante = do
                    let novoLivro = encodePretty (entrada : estante)
                    BS.writefile "Data/temp.json" novoLivro
                    removeFile "Data/lendo.json"
                    renameFile "Data/temp.json" "Data/lendo.json"
                    putStrLn "Livro adicionado à estante dos livros que você está lendo atualmente."
            Lidos ->
                adicionaLivro nome estante = do
                    let novoLivro = encodePretty (entrada : estante)
                    BS.writefile "Data/temp.json" novoLivro
                    removeFile "Data/lidos.json"
                    renameFile "Data/temp.json" "Data/lidos.json"
                    putStrLn "Livro adicionado à estante dos livros que você está já leu."   
            Pretendo_Ler ->
                adicionaLivro nome estante = do
                    let novoLivro = encodePretty (entrada : estante)
                    BS.writefile "Data/temp.json" novoLivro
                    removeFile "Data/pretendo_ler.json"
                    renameFile "Data/temp.json" "Data/pretendo_ler.json"
                    putStrLn "Livro adicionado à estante dos livros que você ainda pretende ler."
            Abandonados ->
                adicionaLivro nome estante = do
                    let novoLivro = encodePretty (entrada : estante)
                    BS.writefile "Data/temp.json" novoLivro
                    removeFile "Data/abandonados.json"
                    renameFile "Data/temp.json" "Data/abandonados.json"
                    putStrLn "Livro adicionado à estante dos livros que você abandonou a leitura." 

{-Verifica se o livro já está cadastrado no sistema bem como se já foi adicionado a alguma das estantes-}
verificaLivro :: String -> Bool
if Livro.livroJaExiste nome listaLivro then    
    if nome not in Lendo.livros && Lidos.livros && Pretendo_Ler.livros && Abandonados.livros then False else True
else False
                