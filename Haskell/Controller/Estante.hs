{-# LANGUAGE DeriveGeneric #-}

module Controller.Estante where

import System.IO.Unsafe (unsafePerformIO)
import System.Directory
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Control.Exception (catch)
import Data.Aeson.Encode.Pretty (encodePretty)
import Controller.Livro
import Controller.Usuario

-- Definição do tipo de dados Estante
data Estante = Estante {
    lidos :: [Livro],
    lendo :: [Livro],
    pretendo_ler :: [Livro],
    abandonados :: [Livro]
} deriving (Show, Generic)

-- Instâncias para permitir a serialização/desserialização JSON
instance ToJSON Estante
instance FromJSON Estante

-- Tipo de dados para armazenar as estantes de todos os usuários
type Estantes = [(String, Estante)]  -- (idUsuario, Estante)

-- Caminho para o arquivo JSON das estantes
estantesFile :: FilePath
estantesFile = "Data/estantes.json"

-- Função para carregar as estantes de todos os usuários do arquivo JSON
getEstantes :: IO Estantes
getEstantes = do
    result <- tryReadFile estantesFile
    case result of
        Right bytes -> return (fromMaybe [] (decode bytes))
        Left err -> do
            putStrLn $ "Erro ao ler o arquivo de estantes: " ++ show err
            return []
    where
        tryReadFile :: FilePath -> IO (Either IOError BL.ByteString)
        tryReadFile path = catch (Right <$> BL.readFile path) handleFileError

        handleFileError :: IOError -> IO (Either IOError BL.ByteString)
        handleFileError err = return (Left err)

-- Função para salvar as estantes de todos os usuários no arquivo JSON
salvarEstantes :: Estantes -> IO ()
salvarEstantes estantes = BL.writeFile estantesFile (encodePretty estantes)

-- Função para adicionar um livro à estante de um usuário
adicionaLivro :: String -> String -> String -> Estantes -> IO ()
adicionaLivro nomeLivro tipoEstante idUsuario estantes = do
    maybeLivros <- getListaLivros
    let livro = maybe (error "Livro não encontrado.") id $ getLivro nomeLivro $ fromJust maybeLivros
    case lookup idUsuario estantes of
        Just estante -> do
            let estanteAtualizada = updateCampoEstante tipoEstante livro estante
            salvarEstantes $ atualizarEstantes idUsuario estanteAtualizada estantes
            putStrLn $ "Livro adicionado à estante de " ++ tipoEstante ++ " do usuário " ++ idUsuario ++ "."
        Nothing -> putStrLn "Usuário não encontrado."

-- Função para atualizar um campo da estante com um novo livro
updateCampoEstante :: String -> Livro -> Estante -> Estante
updateCampoEstante "lidos" livro estante = estante { lidos = livro : lidos estante }
updateCampoEstante "lendo" livro estante = estante { lendo = livro : lendo estante }
updateCampoEstante "pretendo ler" livro estante = estante { pretendo_ler = livro : pretendo_ler estante }
updateCampoEstante "abandonados" livro estante = estante { abandonados = livro : abandonados estante }
updateCampoEstante _ _ _ = error "Tipo de estante inválido."

-- Função para atualizar as estantes com a estante de um usuário
atualizarEstantes :: String -> Estante -> Estantes -> Estantes
atualizarEstantes idUsuario estante [] = [(idUsuario, estante)]
atualizarEstantes idUsuario estante ((id, est):resto)
    | id == idUsuario = (id, estante) : resto
    | otherwise = (id, est) : atualizarEstantes idUsuario estante resto

-- Função para obter a estante de um usuário pelo ID
getEstanteUsuario :: String -> Estantes -> Maybe Estante
getEstanteUsuario userId estantes = lookup userId estantes
updateCampoEstante _ _ _ = error "Tipo de estante inválido."
