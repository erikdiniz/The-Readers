{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Controller.Estante where

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
import Controller.Livro

{-Criando estantes, que podem ser dos subtipos: Lendo, Lidos, Pretendo Ler e Abandonados-}
data Estante = Estante {
    lidos :: [Livro],
    lendo :: [Livro],
    pretendo_ler :: [Livro]
    abandonados :: [Livro]
} deriving (Show, Generic)

instance ToJSON Estante 
instance FromJSON Estante

{- Adiciona um livro já existente no sistema  a um tipo de estante-}
adicionaLivro :: String -> String -> Estante -> String -> IO()
adicionaLivro nome nomeestante estante = do
    if verificaLivro nome estante then
        case nomeestante of
            "lendo" ->
                adicionaLivro nome nomeestante estante = do
                    let novoLivro = encodePretty (entrada : estante)
                    BS.writefile "Data/temp.json" novoLivro
                    removeFile "Data/lendo.json"
                    renameFile "Data/temp.json" "Data/lendo.json"
                    putStrLn "Livro adicionado à estante dos livros que você está lendo atualmente."
            "lidos" ->
                adicionaLivro nome estante = do
                    let novoLivro = encodePretty (entrada : estante)
                    BS.writefile "Data/temp.json" novoLivro
                    removeFile "Data/lidos.json"
                    renameFile "Data/temp.json" "Data/lidos.json"
                    putStrLn "Livro adicionado à estante dos livros que você está já leu."   
            "pretendo_Ler" ->
                adicionaLivro nome estante = do
                    let novoLivro = encodePretty (entrada : estante)
                    BS.writefile "Data/temp.json" novoLivro
                    removeFile "Data/pretendo_ler.json"
                    renameFile "Data/temp.json" "Data/pretendo_ler.json"
                    putStrLn "Livro adicionado à estante dos livros que você ainda pretende ler."
            "abandonados" ->
                adicionaLivro nome estante = do
                    let novoLivro = encodePretty (entrada : estante)
                    BS.writefile "Data/temp.json" novoLivro
                    removeFile "Data/abandonados.json"
                    renameFile "Data/temp.json" "Data/abandonados.json"
                    putStrLn "Livro adicionado à estante dos livros que você abandonou a leitura." 

colocaLivroEstante :: String -> Estante -> String -> Estante
colocaLivroEstante nomeLivro estante nomeestante = do
    let jacadastrado = verificaLivro nomeLivro estante
    if jacadastrado == False then
        case nomeestante of
            "lidos" -> 
                let listadelivros = lidos estante
                let livro = getLivro nomelivro getListaLivros
                let novaLista = listadelivros ++ livro
            "lendo" -> 
                let listadelivros = lendo estante
                let livro = getLivro nomelivro getListaLivros
                let novaLista = listadelivros ++ livro
            "pretendo_ler" -> 
                let listadelivros = pretendo_ler estante
                let livro = getLivro nomelivro getListaLivros
                let novaLista = listadelivros ++ livro    
            "abandonados" -> 
                let listadelivros = abandonados estante
                let livro = getLivro nomelivro getListaLivros
                let novaLista = listadelivros ++ livro

{-Verifica se o livro já está cadastrado no sistema bem como se já foi adicionado a alguma das estantes-}
verificaLivro :: String -> Estante -> Bool
verificaLivro nomeLivro estante = do
    let lidos = lidos estante
    let lendo = lendo estante
    let pretendo_ler = pretendo_ler estante
    let abandonados = abandonados estante
    let livro = getLivro nomelivro getListaLivros
    if livro `elem` lidos && livro `elem` lendo && livro `elem` pretendo_ler && livro `elem` abandonados then True else False