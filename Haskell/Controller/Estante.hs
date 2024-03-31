
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller.Estante where

import System.Directory
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Control.Monad (when)
import Control.Exception (catch)
import Data.Aeson.Encode.Pretty (encodePretty)
import Controller.Livro

-- Definição do tipo de dados Estante
data Estante = Estante {
    idusuario :: String,
    lidos :: [Livro],
    lendo :: [Livro],
    pretendo_ler :: [Livro],
    abandonados :: [Livro]
} deriving (Show, Generic)

-- Instâncias para permitir a serialização/desserialização JSON
instance ToJSON Estante
instance FromJSON Estante

-- Caminho para o arquivo JSON da estante
estanteFile :: FilePath
estanteFile = "Data/estante.json"

-- Função para carregar a estante a partir do arquivo JSON
getEstante :: IO (Maybe Estante)
getEstante = do
    result <- tryReadFile estanteFile
    case result of
        Right bytes -> return (decode bytes)
        Left err -> do
            putStrLn $ "Erro ao ler o arquivo de estante: " ++ show err
            return Nothing
    where
        tryReadFile :: FilePath -> IO (Either IOError BL.ByteString)
        tryReadFile path = catch (Right <$> BL.readFile path) handleFileError

        handleFileError :: IOError -> IO (Either IOError BL.ByteString)
        handleFileError err = return (Left err)

-- Função para salvar a estante no arquivo JSON
salvarEstante :: Estante -> IO ()
salvarEstante estante = BL.writeFile estanteFile (encodePretty estante)

-- Função para adicionar um livro à estante
adicionaLivro :: String -> String -> Estante -> IO ()
adicionaLivro nomeLivro tipoEstante estante = do
    maybeLivros <- getListaLivros
    let livro = maybe (error "Livro não encontrado.") id $ getLivro nomeLivro $ fromJust maybeLivros

    -- Verifica se o livro já está na estante
    let livroJaCadastrado = any (\livro' -> nome livro' == nomeLivro) (getCampoEstante tipoEstante estante)

    when (not livroJaCadastrado) $ do
        -- Adiciona o livro ao campo apropriado
        let estanteAtualizada = updateCampoEstante tipoEstante livro estante
        salvarEstante estanteAtualizada
        putStrLn $ "Livro adicionado à estante de " ++ tipoEstante ++ "."

    -- Mensagem de erro se o livro já estiver cadastrado
    when livroJaCadastrado $
        putStrLn "Este livro já está na sua estante."

-- Função para obter o campo apropriado da estante com base no tipo
getCampoEstante :: String -> Estante -> [Livro]
getCampoEstante "lidos" estante = lidos estante
getCampoEstante "lendo" estante = lendo estante
getCampoEstante "pretendo ler" estante = pretendo_ler estante
getCampoEstante "abandonados" estante = abandonados estante
getCampoEstante _ _ = error "Tipo de estante inválido."

-- Função para atualizar um campo da estante com um novo livro
updateCampoEstante :: String -> Livro -> Estante -> Estante
updateCampoEstante "lidos" livro estante = estante { lidos = livro : lidos estante }
updateCampoEstante "lendo" livro estante = estante { lendo = livro : lendo estante }
updateCampoEstante "pretendo ler" livro estante = estante { pretendo_ler = livro : pretendo_ler estante }
updateCampoEstante "abandonados" livro estante = estante { abandonados = livro : abandonados estante }
updateCampoEstante _ _ _ = error "Tipo de estante inválido."
=======
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
