{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Controller.Avaliacao where
import GHC.Genericsimport 
import Data.Aeson
import Data.Maybe
import Controller.Estante
import Controller.Livro
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Encode.Pretty (encodePretty)

data Avaliacao = Avaliacao {
    usuarioId :: String,
    livroId :: String, 
    nota :: Int, 
    resenha :: Maybe String, 
    curtida :: Int,
    comentarios :: [String]
} deriving (Show, Generic)

instance ToJSON Avaliacao
instance FromJSON Avaliacao

-- Faz uma avaliação apenas para os livros já lidos
fazAvaliacao :: String -> String -> Int -> Maybe String -> IO ()
fazAvaliacao usuarioId livroId nota resenha = do 
    let avaliacao = Avaliacao usuarioId livroId nota resenha 0 [] 
    adicionaAvaliacao avaliacao 
    putStrLn "Avaliação realizada com sucesso!"

-- Função para adicionar a avaliação ao arquivo JSON
adicionaAvaliacao :: Avaliacao -> IO ()
adicionaAvaliacao avaliacao = do 
    let novaAvaliacao = encodePretty (avaliacao : avaliacoes) 
    BL.writeFile "Data/temp.json" novaAvaliacao 
    removeFile "Data/avaliacoes.json" 
    renameFile "Data/temp.json" "Data/avaliacoes.json"

-- Faz uma visão de avaliação
mostraAvaliacao :: Avaliacao -> String
mostraAvaliacao avaliacao = 
    putStrLn $ "Livro: " ++ livro avaliacao ++ 
    putStrLn $ "\nNota: " ++ show (nota avaliacao) ++ 
    putStrLn $ "\nResenha: " ++ maybe "Sem resenha" textoResenha (resenha avaliacao) ++ 
    putStrLn $ "\nCurtidas: " ++ show (curtidas avaliacao) ++ 
    putStrLn $ "\nComentários: " ++ show (comentarios avaliacao)

-- Adiciona uma curtida a uma avaliação
adicionaCurtida :: Avaliacao -> Avaliacao
adicionaCurtida avaliacao = 
    avaliacao { curtidas = curtidas avaliacao + 1 } 


-- Função para adicionar um comentário a uma avaliação
adicionarComentarioAvaliacao :: Avaliacao -> String -> Avaliacao
adicionarComentarioAvaliacao avaliacao novoComentario =
    avaliacao { comentarios = comentarios avaliacao ++ [novoComentario] }

-- Função para buscar uma avaliação com base no usuarioId e livroId
buscarAvaliacao :: String -> String -> [Avaliacao] -> Maybe Avaliacao
buscarAvaliacao usuarioId livroId [] = Nothing
buscarAvaliacao usuarioId livroId (y :ys) =
    if usuarioId == usuarioId y && livroId == livroId y
        then Just y  
        else buscarAvaliacao usuarioId livroId ys  