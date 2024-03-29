{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller.Estatisticas where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.List (group, sortBy, maximumBy)
import Data.Function (on)
import Data.Ord (comparing)

{- Definindo os tipos de dados para representar um livro. -}
data Livro = Livro {
    titulo :: String,
    autor :: String,
    genero :: String,
    anoLancamento :: Int,
    numPaginas :: Int,
    avaliacao :: Float
} deriving (Show, Generic)

{- Instância FromJSON para Livro -}
instance FromJSON Livro

jsonFile :: FilePath
jsonFile = "Data/estanteLidos.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

{- Lê o conteúdo do arquivo JSON e decodifica em uma lista de livros -}
lerLivros :: FilePath -> IO [Livro]
lerLivros arquivo = do
    conteudo <- B.readFile arquivo
    case decode conteudo of
        Nothing -> error "Erro ao decodificar o JSON."
        Just livros -> return livros

{- ESTATÍSTICAS GERAIS -}

{- Retorna o total de páginas lidas pelo usuário. -}
getTotalPag :: [Livro] -> Int
getTotalPag [] = 0
getTotalPag (h:t) = numPaginas h + getTotalPag t

{- Retorna o total de livros lidos. -}
getTotalLivros :: [Livro] -> Int
getTotalLivros = Prelude.length

{- Função que retorna quantas vezes determinado genero foi lido -}
generoNumLeituras :: [Livro] -> String -> Int
generoNumLeituras [] _ = 0
generoNumLeituras (h:t) nomeGenero
    | genero h == nomeGenero = 1 + generoNumLeituras t nomeGenero
    | otherwise = generoNumLeituras t nomeGenero

{- Função que retorna a lista de gêneros que já foram lidos -}
listaGeneros :: [Livro] -> [String]
listaGeneros [] = []
listaGeneros livros = go livros []
  where
    go [] generos = generos
    go (h:t) generos
        | notElem (genero h) generos = go t (genero h : generos)
        | otherwise = go t generos

{- Função que retorna o genero mais lido de uma lista de livros -}
generoMaisLido :: [Livro] -> String
generoMaisLido livros =
    let generos = listaGeneros livros
        contagemGeneros = map (\gen -> (gen, generoNumLeituras livros gen)) generos
        generoMaisLido = fst $ maximumBy (compare `on` snd) contagemGeneros
    in generoMaisLido

{- Função que retorna quantas vezes determinado autor foi lido -}
autorNumLeituras :: [Livro] -> String -> Int
autorNumLeituras [] _ = 0
autorNumLeituras (h:t) nomeAutor
    | autor h == nomeAutor = 1 + autorNumLeituras t nomeAutor
    | otherwise = autorNumLeituras t nomeAutor

{- Função que retorna a lista de autores que já foram lidos -}
listaAutores :: [Livro] -> [String]
listaAutores [] = []
listaAutores livros = go livros []
  where
    go [] autores = autores
    go (h:t) autores
        | notElem (autor h) autores = go t (autor h : autores)
        | otherwise = go t autores

{- Função que retorna o autor mais lido de uma lista de livros -}
autorMaisLido :: [Livro] -> String
autorMaisLido livros =
    let autores = listaAutores livros
        contagemAutores = map (\aut -> (aut, autorNumLeituras livros aut)) autores
        autorMaisLido = fst $ maximumBy (compare `on` snd) contagemAutores
    in autorMaisLido   

{- Função que retorna o primeiro livro de melhor avaliação -}
melhorLivro :: [Livro] -> Livro
melhorLivro livros =
    let sortedLivros = sortBy (flip (comparing avaliacao)) livros
    in head sortedLivros

{- Função para centralizar uma string em uma largura específica -}
centerString :: Int -> String -> String
centerString width str
    | length str >= width = str
    | otherwise = let padding = replicate ((width - length str) `div` 2) ' '
                      extra = if odd (width - length str) then " " else ""
                  in padding ++ str ++ padding ++ extra

{- Exibição das estatísticas gerais do usuário -}
estatisticasGerais :: IO()
estatisticasGerais = do 
    totalLivros <- getTotalLivros <$> lerLivros jsonFile
    totalPaginas <- getTotalPag <$> lerLivros jsonFile
    generoLido <- generoMaisLido <$> lerLivros jsonFile
    autorLido <- autorMaisLido <$> lerLivros jsonFile
    livroPreferido <- melhorLivro <$> lerLivros jsonFile
    let livroTitulo = titulo livroPreferido
    let livroNota = show (avaliacao livroPreferido)
    let linha = "." ++ replicate 58 '-' ++ "."  {- Linha de separação -}

    putStrLn $ "\n" ++ linha
    putStrLn $ "|" ++ centerString 58 "ESTATÍSTICAS GERAIS" ++ "|"
    putStrLn $ "|" ++ centerString 58 "" ++ "|"
    putStrLn $ "|" ++ centerString 58 "NÚMERO DE LIVROS LIDOS:" ++ "|"
    putStrLn $ "|" ++ centerString 58 (show totalLivros) ++ "|"
    putStrLn $ "|" ++ centerString 58 "" ++ "|"
    putStrLn $ "|" ++ centerString 58 "NÚMERO DE PÁGINAS LIDAS:" ++ "|"
    putStrLn $ "|" ++ centerString 58 (show totalPaginas) ++ "|"
    putStrLn $ "|" ++ centerString 58 "" ++ "|"
    putStrLn $ "|" ++ centerString 58 "GÊNERO MAIS LIDO:" ++ "|"
    putStrLn $ "|" ++ centerString 58 generoLido ++ "|"
    putStrLn $ "|" ++ centerString 58 "" ++ "|"
    putStrLn $ "|" ++ centerString 58 "AUTOR MAIS LIDO:" ++ "|"
    putStrLn $ "|" ++ centerString 58 autorLido ++ "|"
    putStrLn $ "|" ++ centerString 58 "" ++ "|"
    putStrLn $ "|" ++ centerString 58 "LIVRO PREFERIDO:" ++ "|"
    putStrLn $ "|" ++ centerString 58 (livroTitulo ++ (" - Nota " ++ livroNota)) ++ "|"
    putStrLn $ "|" ++ centerString 58 "" ++ "|"
    putStrLn $ linha ++ "\n"