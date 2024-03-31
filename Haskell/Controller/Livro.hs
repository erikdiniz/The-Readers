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
import Data.Bool (Bool)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.ReadP (get)

instance FromJSON Livro
instance ToJSON Livro

-- |Criando um tipo de dado representando um Livro.
-- |Um Livro tem nome, autor, um número de páginas e o gênero referente.
data Livro = Livro {
    nome :: String,
    autor :: String,
    n_paginas :: Int,
    genero :: String
}   deriving (Show, Generic)


-- |Método Interativo que cadastra um Livro no sistema,
-- |usa o nome para verificar se o Livro já está cadastrado,
-- |chama o método auxiliar para manipular a atualização do sistema de Livros.
cadastraLivro :: String -> String -> Int -> String -> IO()
cadastraLivro nome autor n_paginas genero = do
    if livroJaExiste nome getListaLivros then
        putStrLn "Livro já cadastrado no sistema."
    else do
        let livro = Livro nome autor n_paginas genero
        maybeJson <- getListaLivros
        escreveLivro (fromJust maybeJson) livro
        putStrLn "Livro cadastrado."

-- |Método Interativo auxiliar que escreve os dados do Livro no sistema,
-- |usasse um arquivo .json para armazenamento.
escreveLivro :: [Livro] -> Livro -> IO()
escreveLivro listaLivros livro = do
    let novoLivro = encodePretty (livro : listaLivros)
    BL.writeFile "Data/temp.json" novoLivro
    removeFile "Data/livros.json"
    renameFile "Data/temp.json" "Data/livros.json"

-- |Método interativo que deleta um Livro do sistema a partir do nome,
-- |se o Livro existir, chama o método auxiliar para excluí-lo da lista.
-- |Ao final, atualiza o sistema. 
deletaLivro :: String -> IO ()
deletaLivro nomeLivro = do
    if livroJaExiste nomeLivro getListaLivros then do
        maybeListaLivros <- getListaLivros
        let listaLivros = fromMaybe [] maybeListaLivros
        let novaListaLivros = removeLivro nomeLivro listaLivros
        BL.writeFile "Data/temp.json" (encodePretty novaListaLivros)
        removeFile "Data/livros.json"
        renameFile "Data/temp.json" "Data/livros.json"
        putStrLn "Livro excluido com sucesso."
    else
        putStrLn "Livro não existe no sistema."

-- |Método recursivo auxiliar que exclui um Livro da lista de Livros do sistema.
removeLivro :: String -> [Livro] -> [Livro]
removeLivro _ [] = []
removeLivro nomeLivro (h:t)
    |nome h == nomeLivro = t
    |otherwise = h : removeLivro nomeLivro t

-- |Método Interativo que recupera a lista de todos os Livros cadastrados no sistema.
getListaLivros :: IO (Maybe [Livro])
getListaLivros = do
    arquivo <- BL.readFile "Data/livros.json"
    return (decode arquivo)

-- |Método Interativo que recupera a lista de todos os Livros cadastrados no sistema fora do conteto IO.
getListaLivrosUnsafe :: [Livro]
getListaLivrosUnsafe = do
    let arquivo = unsafePerformIO (BL.readFile "Data/livros.json")
    let maybeJson = (decode arquivo) :: Maybe [Livro]
    case maybeJson of
        Nothing -> []
        Just livros -> livros

-- |Método recursivo que recupera um Livro cadastrado na lista de Livros a partir do nome.
getLivro :: String -> [Livro] -> Maybe Livro
getLivro _ [] = Nothing
getLivro nomeProcurado (h:t)
    | nome h == nomeProcurado = Just h
    | otherwise = getLivro nomeProcurado t

-- |Método recursivo que recupera o nome de todos os Livros cadastrados no sistema.
getNomeLivros :: [Livro] -> [String] -> [String]
getNomeLivros [] nomesLivros =  nomesLivros
getNomeLivros (h:t) nomesLivros = getNomeLivros t (nomesLivros ++ [nome h])

-- |Método Interativo que verifica se um Livro já existe no sistema.
livroJaExiste :: String -> IO(Maybe [Livro]) -> Bool
livroJaExiste nomeProcurado listaLivro =
    case maybeListaLivros of
        Just listaLivros -> case getLivro nomeProcurado listaLivros of
            Just _  -> True
            Nothing -> False
        Nothing -> False
    where
        maybeListaLivros = unsafePerformIO listaLivro

recuperaLivrosUnsafe :: IO [Livro]
recuperaLivrosUnsafe = do
    let arquivo = unsafePerformIO $ BL.readFile "Data/livros.json"
    let maybeJson = (decode arquivo) :: Maybe [Livro]
    case maybeJson of
        Nothing -> return []
        Just livros -> return livros
