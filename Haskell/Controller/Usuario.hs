{-# LANGUAGE DeriveGeneric #-}

module Controller.Usuario where
import Data.Aeson 
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (unwords)
import Data.Maybe
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import System.Directory
import System.IO.Unsafe
import Controller.Perfil
--import Controller.Estante


data Usuario = Usuario {
    idUsuario :: String,
    senha :: String,
    seguidores :: [String],
    seguindo :: [String]
} deriving (Show, Generic)

instance ToJSON Usuario 
instance FromJSON Usuario
instance Eq Usuario where 
    (Usuario nome1 senha1 seguidores1 seguindo1) == (Usuario nome2 senha2 seguidores2 seguindo2) = nome1 == nome2

{- Cria um usuário e chama o método de adicionar ele nos cadastrados-}
cadastraUsuario ::  String->  String-> IO ()
cadastraUsuario nome senha = do
    let usuario = Usuario nome senha [] []
    maybeJson <- recuperaUsuarios
    adicionaUsuario (fromJust maybeJson) usuario
    criarPerfil "" "" nome

loginUsuario :: String -> String -> Maybe Usuario
loginUsuario nome senha = do
    let usuario = pegaUsuario nome 
    case usuario of
        Nothing -> Nothing
        Just usuario -> valideSenha usuario senha

valideSenha :: Usuario -> String -> Maybe Usuario
valideSenha usuario senhaFornecida = if senha usuario == senhaFornecida then Just usuario else Nothing

{-Escreve na lista inteira de usuarios um usuario novo-}
adicionaUsuario :: [Usuario] -> Usuario -> IO()
adicionaUsuario usuarios usuario = do
    let novosUsuarios = encodePretty (usuario : usuarios)
    BS.writeFile "Data/temp.json" novosUsuarios
    removeFile "Data/usuarios.json"
    renameFile "Data/temp.json" "Data/usuarios.json"

{- Pega a lista inteira de usuários cadastrados -}
recuperaUsuarios :: IO (Maybe [Usuario])
recuperaUsuarios = do
    arquivo <- BS.readFile "Data/usuarios.json"
    return (decode arquivo)

{- Recupera usuarios cadastrados fora do contexto IO -}
recuperaUsuariosUnsafe :: [Usuario]
recuperaUsuariosUnsafe = do
    let arquivo = unsafePerformIO $ BS.readFile "Data/usuarios.json"
    let maybeJson = (decode arquivo) :: Maybe [Usuario]
    case maybeJson of
        Nothing -> []
        Just usuarios -> usuarios

pegaUsuario :: String -> Maybe Usuario
pegaUsuario nome = do
    let usuarios = recuperaUsuariosUnsafe
    procuraUsuario nome usuarios

procuraUsuario :: String -> [Usuario] -> Maybe Usuario
procuraUsuario nome [] = Nothing 
procuraUsuario nome (x:xs) = if idUsuario x == nome then Just x else procuraUsuario nome xs 

recuperaNomeDeUsuarios :: [Usuario] -> [String] -> [String]
recuperaNomeDeUsuarios [] nomes = nomes
recuperaNomeDeUsuarios (x:xs) nomes = recuperaNomeDeUsuarios xs (nomes ++ [idUsuario x]) 
    
seguirUsuario :: Usuario -> String -> IO()
seguirUsuario usuarioLogado idUsuarioASeguir = do
    atualizaSeguindo usuarioLogado idUsuarioASeguir
        
atualizaSeguindo :: Usuario -> String -> IO()
atualizaSeguindo usuario novoSeguindo= do
    {-busca nos dados o objeto com o usuario seguido-}
    let outro = procuraUsuarioUnsafe novoSeguindo recuperaUsuariosUnsafe

    {-Cria novos objetos atualizados-}
    let updateSeguindo = usuario {seguindo = seguindo usuario ++ [novoSeguindo]}
    let updateSeguidor = outro {seguidores = seguidores outro ++ [idUsuario usuario]}

    {-Remove os antigos-}
    let usuariosAtualizados = removeUsuarios recuperaUsuariosUnsafe [updateSeguindo, updateSeguidor]

    {-Adiciona os novos-}
    adicionaUsuario (usuariosAtualizados ++ [updateSeguindo]) updateSeguidor


removeElementosString :: [String] -> [String] -> [String]
removeElementosString listaTotal remover = [nome | nome <- listaTotal, not (nome `elem` remover)]

removeUsuarios :: [Usuario] -> [Usuario] -> [Usuario]
removeUsuarios usuariosTotais usuariosRemovidos = [usuarios | usuarios <- usuariosTotais, not (usuarios `elem` usuariosRemovidos)]

procuraUsuarioUnsafe :: String -> [Usuario] -> Usuario
procuraUsuarioUnsafe nome [] = Usuario "falha" "falha" [] []
procuraUsuarioUnsafe nome (x:xs) = if idUsuario x == nome then x else procuraUsuarioUnsafe nome xs

