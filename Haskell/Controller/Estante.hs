
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
