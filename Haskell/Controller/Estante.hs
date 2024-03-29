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
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (mapMaybe)

{-Criando estantes, que podem ser dos subtipos: Lendo, Lidos, Pretendo Ler e Abandonados-}
data Estante = Estante {
    idusuario :: String,
    lidos :: [Livro],
    lendo :: [Livro],
    pretendo_ler :: [Livro]
    abandonados :: [Livro]
} deriving (Show, Generic)

instance ToJSON Estante 
instance FromJSON Estante

{- Adiciona um livro já existente na estante e atualiza o arquivo json com o livro cadastrado-}
adicionaLivro :: String -> String -> Estante -> String -> IO()
adicionaLivro nome nomeestante estante = do
    if verificaLivro nome estante then
        case nomeestante of
            "lendo" ->
                adicionaLivro nome nomeestante estante = do
                    {-Colocar o livro na estante, remover a estante antiga do json, adicionar a nova estante num [Estante] e escrever no json-}
                    colocaLivroEstante nome estante "lendo"
                    let novoLivro = encodePretty (entrada : estante)
                    BS.writefile "Data/temp.json" novoLivro
                    removeFile "Data/estante.json"
                    renameFile "Data/temp.json" "Data/estante.json"
                    putStrLn "Livro adicionado à estante dos livros que você está lendo atualmente."
            "lidos" ->
                adicionaLivro nome estante = do
                    colocaLivroEstante nome estante "lidos"
                    let novoLivro = encodePretty (entrada : estante)
                    BS.writefile "Data/temp.json" novoLivro
                    removeFile "Data/estante.json"
                    renameFile "Data/temp.json" "Data/estante.json"
                    putStrLn "Livro adicionado à estante dos livros que você está já leu."   
            "pretendo ler" ->
                adicionaLivro nome estante = do
                    colocaLivroEstante nome estante "pretendo ler"
                    let novoLivro = encodePretty (entrada : estante)
                    BS.writefile "Data/temp.json" novoLivro
                    removeFile "Data/estante.json"
                    renameFile "Data/temp.json" "Data/estante.json"
                    putStrLn "Livro adicionado à estante dos livros que você ainda pretende ler."
            "abandonados" ->
                adicionaLivro nome estante = do
                    colocaLivroEstante nome estante "abandonados"
                    let novoLivro = encodePretty (entrada : estante)
                    BS.writefile "Data/temp.json" novoLivro
                    removeFile "Data/estante.json"
                    renameFile "Data/temp.json" "Data/estante.json"
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
            "pretendo ler" -> 
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

{-Recupera uma estante-}
getEstante :: IO(Maybe Estante)
getEstante = do
    arquivo <- BL.readFile "Data/estante.json"
    return (decode arquivo)

{-Recupera a estante de livros lidos-}
getLidos :: IO(Maybe [Livro])
getLidos = do
    arquivo <- BL.readFile "Data/estante.json"
    let maybeEstante = decode arquivo :: Maybe Estante
    case maybeEstante of
        Just estante -> do
            let lidos = mapMaybe (parseMaybe (.: "lidos")) estante
            return (lidos)

{-Recupera a estante de livros que o usuário está lendo-}
getLendo :: IO(Maybe [Livro])
getLendo = do
    arquivo <- BL.readFile "Data/estante.json"
    let maybeEstante = decode arquivo :: Maybe Estante
    case maybeEstante of
        Just estante -> do
            let lendo = mapMaybe (parseMaybe (.: "lendo")) estante
            return (lendo)

{-Recupera a estante de livros que o usário pretende ler-}            
getPretendoLer :: IO(Maybe [Livro])
getPretendoLer = do
    arquivo <- BL.readFile "Data/estante.json"
    let maybeEstante = decode arquivo :: Maybe Estante
    case maybeEstante of
        Just estante -> do
            let pretendoLer = mapMaybe (parseMaybe (.: "pretendo ler")) estante
            return (pretendoLer)

{-Recupera a estante de livros abandonados pelo usuário-}            
getAbandonados :: IO(Maybe [Livro])
getAbandonados = do
    arquivo <- BL.readFile "Data/estante.json"
    let maybeEstante = decode arquivo :: Maybe Estante
    case maybeEstante of
        Just estante -> do
            let abandonados = mapMaybe (parseMaybe (.: "lidos")) estante
            return (abandonados)

{-Representação textual da estante de livros lidos pelo usuário-}            
toStringLidos :: [Livro] -> IO(String)
toStringLidos = do
    let lidos = nome getLidos
    let resultado = unlines lidos
    return (resultado)

{-Representação textual da estante de livros que o usuário está lendo -}
toStringLendo :: [Livro] -> IO(String)
toStringLendo = do
    let lendo = nome getLendo
    let resultado = unlines lendo
    return (resultado)

{-Representação textual da estante de livros que o usuário pretende ler-}    
toStringPretendoLer :: [Livro] -> IO(String)
toStringPretendoLer = do
    let pretendoLer = nome getPretendoLer
    let resultado =  unlines pretendoLer
    return (resultado)

{-Representação textual da estante de livros que o usuário abandonou a leitura-}     
toStringAbandonados :: [Livro] -> IO(String)
toStringAbandonados = do
    let abandonados = nome getAbandonados
    let resultado =  unlines abandonados
    return (resultado)
