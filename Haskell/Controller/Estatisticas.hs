{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller.Estatisticas where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.List (group, sortBy, maximumBy)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (unfoldr)

import Controller.Leitura
import Controller.Usuario

jsonFile :: FilePath
jsonFile = "Data/leituras.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

{- Lê o conteúdo do arquivo JSON e decodifica em uma lista de livros -}
lerLivros :: FilePath -> IO [Leitura]
lerLivros arquivo = do
    conteudo <- B.readFile arquivo
    case decode conteudo of
        Nothing -> error "Erro ao decodificar o JSON."
        Just livros -> return livros

{- ESTATÍSTICAS GERAIS -}

{- Retorna o total de páginas lidas pelo usuário com o ID especificado. -}
getTotalPag :: [Leitura] -> String -> Int
getTotalPag [] _ = 0
getTotalPag leituras idUsuario = sum $ map numPaginas (recuperaLeituraDoUsuario idUsuario)

{- Retorna o total de livros lidos pelo usuário com o ID especificado. -}
getTotalLivros :: [Leitura] -> String -> Int
getTotalLivros leituras idUsuario = Prelude.length (recuperaLeituraDoUsuario idUsuario)

{- Função que retorna quantas vezes determinado genero foi lido pelo usuário com o ID especificado -}
generoNumLeituras :: [Leitura] -> String -> String -> Int
generoNumLeituras [] _ _ = 0
generoNumLeituras (h:t) idUsuario nomeGenero
    | genero_lido h == nomeGenero && idUsuario == idUsuario_lido h = 1 + generoNumLeituras t idUsuario nomeGenero
    | otherwise = generoNumLeituras t idUsuario nomeGenero

{- Função que retorna a lista de gêneros que já foram lidos pelo usuário com o ID especificado -}
listaGeneros :: [Leitura] -> String -> [String]
listaGeneros [] _ = []
listaGeneros leituras idUsuario = go leituras []
  where
    go [] generos = generos
    go (h:t) generos
        | idUsuario == idUsuario_lido h && notElem (genero_lido h) generos = go t (genero_lido h : generos)
        | otherwise = go t generos


{- Função que retorna o gênero mais lido pelo usuário com o ID especificado -}
generoMaisLido :: [Leitura] -> String -> String
generoMaisLido leituras idUsuario =
    let generos = listaGeneros leituras idUsuario
        contagemGeneros = map (\gen -> (gen, generoNumLeituras leituras gen idUsuario)) generos
        generoMaisLido = fst $ maximumBy (compare `on` snd) contagemGeneros
    in generoMaisLido

{- Função que retorna quantas vezes determinado autor foi lido pelo usuário com o ID especificado -}
autorNumLeituras :: [Leitura] -> String -> String -> Int
autorNumLeituras leituras idUsuario nomeAutor =
    length [1 | leitura <- leituras, idUsuario == idUsuario_lido leitura && autor_lido leitura == nomeAutor]

{- Função que retorna a lista de autores que já foram lidos pelo usuário com o ID especificado -}
listaAutores :: [Leitura] -> String -> [String]
listaAutores leituras idUsuario =
    go leituras []
    where
        go [] autores = autores
        go (leitura:resto) autores
            | idUsuario == idUsuario_lido leitura && notElem (autor_lido leitura) autores =
                go resto (autor_lido leitura : autores)
            | otherwise = go resto autores

{- Função que retorna o autor mais lido pelos usuários com o ID especificado -}
autorMaisLido :: [Leitura] -> String -> String
autorMaisLido leituras idUsuario =
    let autores = listaAutores leituras idUsuario
        contagemAutores = map (\aut -> (aut, autorNumLeituras leituras aut idUsuario)) autores
        autorMaisLido = fst $ maximumBy (compare `on` snd) contagemAutores
    in autorMaisLido  

{- Função que retorna o primeiro livro de melhor avaliação lido por um usuário específico -}
melhorLivro :: [Leitura] -> String -> Leitura
melhorLivro leituras idUsuario =
    let livrosLidos = filter (\leitura -> idUsuario_lido leitura == idUsuario) leituras
        sortedLivros = sortBy (flip (comparing nota)) livrosLidos
    in case sortedLivros of
        (livro:_) -> livro
        _ -> error "Nenhum livro lido pelo usuário encontrado."


{- Função para centralizar uma string em uma largura específica -}
centerString :: Int -> String -> String
centerString width str
    | length str >= width = str
    | otherwise = let padding = replicate ((width - length str) `div` 2) ' '
                      extra = if odd (width - length str) then " " else ""
                  in padding ++ str ++ padding ++ extra

{- Define a função wrapText -}
wrapText :: Int -> String -> [String]
wrapText width = unfoldr $ \s ->
    if null s
        then Nothing
        else Just (splitAt width s)

{- Exibição das estatísticas gerais do usuário -}
estatisticasGerais :: Usuario -> IO()
estatisticasGerais usuario = do 
    leituras <- lerLivros jsonFile
    let usuarioId = idUsuario usuario
        totalLivros = getTotalLivros leituras usuarioId
        totalPaginas = getTotalPag leituras usuarioId
        generoLido = generoMaisLido leituras usuarioId
        autorLido = wrapText 33 (autorMaisLido leituras usuarioId)
        livroPreferido = melhorLivro leituras usuarioId
        livroTitulo = wrapText 33 (titulo_lido livroPreferido)
        livroNota = show (nota livroPreferido)

    putStrLn $ "-------------------------------------" ++ "\n"
    putStrLn $ "|" ++ centerString 35 "Estatísticas gerais" ++ "|"
    putStrLn $ "|" ++ centerString 35 "" ++ "|"
    putStrLn $ "|" ++ centerString 35 "Total de livros lidos:" ++ "|"
    putStrLn $ "|" ++ centerString 35 (show totalLivros) ++ "|"
    putStrLn $ "|" ++ centerString 35 "" ++ "|"
    putStrLn $ "|" ++ centerString 35 "Total de páginas lidas:" ++ "|"
    putStrLn $ "|" ++ centerString 35 (show totalPaginas) ++ "|"
    putStrLn $ "|" ++ centerString 35 "" ++ "|"
    putStrLn $ "|" ++ centerString 35 "Gênero mais lido:" ++ "|"
    putStrLn $ "|" ++ centerString 35 generoLido ++ "|"
    putStrLn $ "|" ++ centerString 35 "" ++ "|"
    putStrLn $ "|" ++ centerString 35 "Autor mais lido:" ++ "|"
    mapM_ putStrLn $ map (\line -> "|" ++ centerString 35 line ++ "|") autorLido
    putStrLn $ "|" ++ centerString 35 "" ++ "|"
    putStrLn $ "|" ++ centerString 35 "Livro preferido:" ++ "|"
    mapM_ putStrLn $ map (\line -> "|" ++ centerString 35 line ++ "|") livroTitulo
    putStrLn $ "|" ++ centerString 35 ("Nota " ++ livroNota) ++ "|" ++ "\n"
    putStrLn $ "-------------------------------------"