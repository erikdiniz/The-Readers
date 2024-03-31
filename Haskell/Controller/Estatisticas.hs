{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller.Estatisticas where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.List (group, sortBy, maximumBy, sort, groupBy, nub)
import Data.List (unfoldr)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Char (toUpper)

import Controller.Leitura
import Controller.Usuario
import Controller.Admin

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

{- AUTORES -}

{- Retorna a lista de autores e a quantidade de livros lidos por autor pelo usuário específico -}
autoresQuantidadeLivrosUsuario :: Usuario -> [(String, Int)]
autoresQuantidadeLivrosUsuario usuario =
    let usuarioId = idUsuario usuario
        leiturasUsuario = recuperaLeituraDoUsuario usuarioId
        autores = listaAutores leiturasUsuario usuarioId
        agrupados = group (sort autores)
    in map (\xs -> (head xs, length xs)) agrupados

{- Formata a lista de autores e a quantidade de livros lidos por autor pelo usuário específico -}
exibeAutoresQuantidadeLivrosUsuario :: Usuario -> IO ()
exibeAutoresQuantidadeLivrosUsuario usuario = do
    let usuarioId = idUsuario usuario
        leituras = recuperaLeituraUnsafe
        autoresQuantidade = autoresQuantidadeLivrosUsuario usuario
    putStrLn $ "-------------------------------------" ++ "\n"
    putStrLn $ "|              Autores              |"
    putStrLn $ "|                                   |"
    mapM_ (\(autor, quantidade) -> putStrLn $ "|" ++ centerString 34 (autor ++ " - " ++ show quantidade ++ " livro(s)") ++ " |" ++ "\n|                                   |") autoresQuantidade
    putStrLn $ "\n" ++ "-------------------------------------"

{- GÊNEROS -}

{- Retorna uma lista de tuplas onde o primeiro elemento é o gênero e o segundo elemento é uma lista de livros desse gênero lidos pelo usuário. -}
generosLivrosUsuario :: Usuario -> [(String, [String])]
generosLivrosUsuario usuario =
    let usuarioId = idUsuario usuario
        leiturasUsuario = recuperaLeituraDoUsuario usuarioId
        generos = listaGeneros leiturasUsuario usuarioId
        livrosPorGenero = groupByGenero leiturasUsuario generos
    in map (\gen -> (gen, livrosPorGenero gen)) generos

{- Função auxiliar que agrupa os livros lidos por gênero. -}
groupByGenero :: [Leitura] -> [String] -> String -> [String]
groupByGenero leituras generos gen =
    let livros = map titulo_lido $ filter (\leitura -> genero_lido leitura == gen) leituras
    in sort livros

{- Exibe os livros lidos por cada gênero -}
exibeLivrosPorGenero :: Usuario -> IO ()
exibeLivrosPorGenero usuario = do
    let usuarioId = idUsuario usuario
        leituras = recuperaLeituraUnsafe
        generosLivros = generosLivrosUsuario usuario
    putStrLn $ "-------------------------------------" ++ "\n"
    putStrLn $ "|              Gêneros              |" ++ "\n"
    mapM_ (\(genero, livros) -> do
        let generoUpper = map toUpper genero
        putStrLn $ "|" ++ centerString 35 generoUpper ++ "|"
        mapM_ (\livro -> mapM_ (\line -> putStrLn $ "|" ++ centerString 35 line ++ "|") (wrapText 33 livro)) livros
        putStrLn $ "") generosLivros
    putStrLn $ "-------------------------------------"

{- LIDOS POR ANO -}

-- Função para extrair o ano de uma data no formato "dd/mm/aaaa"
anoDaData :: String -> String
anoDaData dataStr = drop 6 dataStr

-- Função para agrupar os livros por ano
livrosPorAnoUsuario :: Usuario -> [(String, [String])]
livrosPorAnoUsuario usuario =
    let usuarioId = idUsuario usuario
        leiturasUsuario = recuperaLeituraDoUsuario usuarioId
        leiturasOrdenadas = reverse $ sortBy (compare `on` dataLeitura) leiturasUsuario
        leiturasAgrupadas = groupBy ((==) `on` anoDaData . dataLeitura) leiturasOrdenadas
        livrosPorAno = map (\grupo -> (anoDaData (dataLeitura (head grupo)), map titulo_lido grupo)) leiturasAgrupadas
    in livrosPorAno

exibeLivrosPorAno :: Usuario -> IO ()
exibeLivrosPorAno usuario = do
    let usuarioId = idUsuario usuario
        livrosPorAno = sortBy (flip compare `on` fst) $ livrosPorAnoUsuario usuario -- Ordena por ano
    putStrLn $ "-------------------------------------" ++ "\n"
    putStrLn $ "|           Livros por Ano          |" ++ "\n"
    mapM_ (\(ano, livros) -> do
        putStrLn $ "|" ++ centerString 35 ano ++ "|"
        mapM_ (\livro -> mapM_ (\line -> putStrLn $ "|" ++ centerString 35 line ++ "|") (wrapText 33 livro)) livros
        putStrLn $ "") livrosPorAno
    putStrLn $ "-------------------------------------"

{- ADMINISTRADOR -}

-- Retorna uma lista de todos os gêneros presentes nas leituras de todos os usuários
listaGenerosTodosUsuarios :: [Leitura] -> [String]
listaGenerosTodosUsuarios leituras = nub $ map genero_lido leituras

-- Conta o número de leituras de um determinado gênero em todas as leituras de todos os usuários
generoNumLeiturasTodosUsuarios :: [Leitura] -> String -> Int
generoNumLeiturasTodosUsuarios leituras genero = length $ filter (\leitura -> genero_lido leitura == genero) leituras

{- Função que retorna o gênero mais lido entre todos os usuários -}
generoMaisLidoGeral :: [Leitura] -> String
generoMaisLidoGeral leituras =
    let generos = listaGenerosTodosUsuarios leituras
        contagemGeneros = map (\gen -> (gen, generoNumLeiturasTodosUsuarios leituras gen)) generos
        generoMaisLidoGeral = fst $ maximumBy (compare `on` snd) contagemGeneros
    in generoMaisLidoGeral

autorMaisLidoGeral :: [Leitura] -> String
autorMaisLidoGeral leituras =
    let autores = nub $ map autor_lido leituras
        contagemAutores = map (\autor -> (autor, length $ filter (\leitura -> autor_lido leitura == autor) leituras)) autores
        autorMaisLidoGeral = fst $ maximumBy (compare `on` snd) contagemAutores
    in autorMaisLidoGeral

-- Função que retorna o livro com a melhor avaliação entre todos os usuários
melhorLivroAvaliadoGeral :: [Leitura] -> String
melhorLivroAvaliadoGeral leituras =
    let livros = nub $ map titulo_lido leituras
        avaliacoes = map (\livro -> (livro, maximum $ map (\leitura -> if titulo_lido leitura == livro then nota leitura else 0) leituras)) livros
        melhorLivro = fst $ maximumBy (compare `on` snd) avaliacoes
    in melhorLivro

-- Função que calcula o total de páginas lidas no geral
totalPaginasLidasGeral :: [Leitura] -> Int
totalPaginasLidasGeral leituras =
    sum [numPaginas leitura | leitura <- leituras]


exibeEstatisticasAdmin :: Admin -> IO ()
exibeEstatisticasAdmin adm = do
    let leituras = recuperaLeituraUnsafe
        genero = generoMaisLidoGeral leituras
        autor = autorMaisLidoGeral leituras
        melhorLivro = melhorLivroAvaliadoGeral leituras
        numLivrosLidos = Prelude.length leituras
        numPaginasLidas = totalPaginasLidasGeral leituras
    
    putStrLn $ "--------------------------------------" ++ "\n"
    putStrLn $ "|         Estatísticas Gerais        |"
    putStrLn $ "|                                    |"
    putStrLn $ "|" ++ centerString 36 "Gênero mais lido" ++ "|"
    putStrLn $ "|" ++ centerString 36 genero ++ "|"
    putStrLn $ "|                                    |"
    putStrLn $ "|" ++ centerString 36 "Autor mais lido" ++ "|"
    putStrLn $ "|" ++ centerString 36 autor ++ "|"
    putStrLn $ "|                                    |"
    putStrLn $ "|" ++ centerString 36 "Livro com melhor avaliação" ++ "|"
    mapM_ (\line -> putStrLn $ "|" ++ centerString 36 line ++ "|") (wrapText 34 melhorLivro)
    putStrLn $ "|                                    |"
    putStrLn $ "|" ++ centerString 36 "Total de livros lidos" ++ "|"
    putStrLn $ "|" ++ centerString 36 (show numLivrosLidos) ++ "|"
    putStrLn $ "|                                    |"
    putStrLn $ "|" ++ centerString 36 "Total de páginas lidas" ++ "|"
    putStrLn $ "|" ++ centerString 36 (show numPaginasLidas) ++ "|" ++ "\n"
    putStrLn $ "--------------------------------------"

-- Função para alinhar uma string à esquerda dentro de uma largura fixa
leftAlignString :: Int -> String -> String
leftAlignString width str = str ++ replicate (max 0 $ width - length str) ' '
