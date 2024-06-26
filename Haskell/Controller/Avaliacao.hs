{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Controller.Avaliacao where
import GHC.Generics
import Data.Aeson
import Data.Maybe
import Controller.Livro
import Controller.Leitura
import Controller.Usuario
import System.Directory
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as BS
import Data.Aeson.Encode.Pretty (encodePretty)

data Avaliacao = Avaliacao {
    usuarioId :: String,
    livroId :: String, 
    nota_avaliada :: Int, 
    resenha :: String, 
    curtida :: Int,
    comentarios :: [String]
} deriving (Show, Generic)

instance ToJSON Avaliacao
instance FromJSON Avaliacao
instance Eq Avaliacao where 
    (Avaliacao nome_usuario1 titulo1 nota1 resenha1 curtida1 comentario1) == (Avaliacao nome_usuario2 titulo2 nota2 resenha2 curtida2 comentario2) = 
        (nome_usuario1 == nome_usuario2) && (titulo1 == titulo2) && (nota1 == nota2)


-- Faz uma avaliação apenas para os livros já lidos
fazAvaliacao :: String -> Leitura -> String -> IO ()
fazAvaliacao usuarioId leitura resenha = do 
    let avaliacao = Avaliacao usuarioId (titulo_lido leitura) (nota leitura) resenha 0 [] 
    adicionaAvaliacao avaliacao 
    putStrLn "Avaliação realizada com sucesso!"

-- Função para adicionar a avaliação ao arquivo JSON
adicionaAvaliacao :: Avaliacao -> IO ()
adicionaAvaliacao avaliacao = do 
    let avaliacoes = recuperaAvaliacoesUnsafe
    let novaAvaliacao = encodePretty (avaliacao : avaliacoes) 
    BS.writeFile "Data/temp.json" novaAvaliacao 
    removeFile "Data/avaliacoes.json" 
    renameFile "Data/temp.json" "Data/avaliacoes.json"

recuperaAvaliacoesUnsafe :: [Avaliacao]
recuperaAvaliacoesUnsafe = do
    let arquivo = unsafePerformIO $ BS.readFile "Data/avaliacoes.json"
    let maybeJson = (decode arquivo) :: Maybe [Avaliacao]
    case maybeJson of
        Nothing -> []
        Just avaliacoes -> avaliacoes

{- Carrega as avaliações que pertencem a uma lista de usuários-}
recuperaAvaliacoesUsuario :: [Usuario] -> [Avaliacao] -> [Avaliacao]
recuperaAvaliacoesUsuario usuarios todasAvaliacoes = do
    let nomes = recuperaNomeDeUsuarios usuarios []
    [avaliacoes | avaliacoes <- todasAvaliacoes, (usuarioId avaliacoes) `elem` nomes]

{- Carrega as avaliações que pertencem a uma lista de usuários pelo id deles-}
recuperaAvaliacoesPorNome :: [String] -> [Avaliacao] -> [Avaliacao]
recuperaAvaliacoesPorNome nomes todasAvaliacoes = [avaliacoes | avaliacoes <- todasAvaliacoes, (usuarioId avaliacoes) `elem` nomes]

recuperaAvaliacoesSafe :: IO(Maybe [Avaliacao])
recuperaAvaliacoesSafe = do
    arquivo <- BS.readFile "Data/avaliacoes.json"
    return (decode arquivo) 

simuladorFeed :: Usuario -> [Avaliacao] -> IO()

simuladorFeed usuario [] = do 
    putStrLn "Você chegou ao fim do feed"

simuladorFeed usuario (x:xs) = do
    let str = mostraAvaliacao x 
    putStrLn "--------------------------------------"
    let nomeUsuario = "Resenha de: " ++ (usuarioId x) 
    putStrLn nomeUsuario
    putStrLn "--------------------------------------\n"
    putStrLn str
    putStrLn "\n--------------------------------------\n"
    putStrLn "[C] Curtir \n[A] Adicionar Comentário\n[P] Próximo post \n[V] voltar"
    opcao <- getLine
    selecionaOpcaoFeed opcao usuario (x:xs)

selecionaOpcaoFeed :: String -> Usuario -> [Avaliacao] -> IO()
selecionaOpcaoFeed "C" usuario (x:xs) = do
    adicionaCurtida x
    let avaliacaoAtualizada = x {curtida = curtida x + 1 }
    simuladorFeed usuario (avaliacaoAtualizada:xs)

selecionaOpcaoFeed "A" usuario (x:xs) = do
    putStrLn "Digitar o comentário: "
    comentario <- getLine
    let str = idUsuario usuario ++ ": " ++ comentario
    adicionarComentarioAvaliacao x str
    let comentarioAtualizado = x {comentarios = comentarios x ++ [str]}
    simuladorFeed usuario (comentarioAtualizado:xs)

selecionaOpcaoFeed "P" usuario (x:xs) = do
    simuladorFeed usuario (xs)

selecionaOpcaoFeed "V" usuario avaliacoes = do 
    putStrLn "-----------------------------"

-- Faz uma visão de avaliação
mostraAvaliacao :: Avaliacao -> String
mostraAvaliacao avaliacao =
    "Livro: " ++ livroId avaliacao ++
    "\nNota: " ++ show (nota_avaliada avaliacao) ++
    "\nResenha: " ++ (resenha avaliacao)  ++
    "\nCurtidas: " ++ show (curtida avaliacao) ++
    "\nComentários: " ++ show (comentarios avaliacao)

-- Adiciona uma curtida a uma avaliação
adicionaCurtida :: Avaliacao -> IO()
adicionaCurtida avaliacao = do 
    let atualizacao = avaliacao { curtida = curtida avaliacao + 1 } 
    let listaAtualizada = removeAvaliacao recuperaAvaliacoesUnsafe [avaliacao]
    let novaAvaliacao = encodePretty (atualizacao : listaAtualizada) 
    BS.writeFile "Data/temp.json" novaAvaliacao 
    removeFile "Data/avaliacoes.json" 
    renameFile "Data/temp.json" "Data/avaliacoes.json"

-- Função para adicionar um comentário a uma avaliação
adicionarComentarioAvaliacao :: Avaliacao -> String -> IO()
adicionarComentarioAvaliacao avaliacao novoComentario = do
    let atualizacao = avaliacao { comentarios = comentarios avaliacao ++ [novoComentario] }
    let listaAtualizada = removeAvaliacao recuperaAvaliacoesUnsafe [avaliacao]
    let novaAvaliacao = encodePretty (atualizacao : listaAtualizada) 
    BS.writeFile "Data/temp.json" novaAvaliacao 
    removeFile "Data/avaliacoes.json" 
    renameFile "Data/temp.json" "Data/avaliacoes.json"


-- Função para buscar uma avaliação com base no usuarioId e livroId
buscarAvaliacao :: String -> String -> [Avaliacao] -> Maybe Avaliacao
buscarAvaliacao usuarioid livroid [] = Nothing
buscarAvaliacao usuarioid livroid (y:ys) =
    if usuarioid == (usuarioId y) && (livroid == livroId y)
        then Just y  
        else buscarAvaliacao usuarioid livroid ys  

removeAvaliacao :: [Avaliacao] -> [Avaliacao] -> [Avaliacao]
removeAvaliacao todas remover = [avaliacao | avaliacao <- todas, not (avaliacao `elem` remover)]
