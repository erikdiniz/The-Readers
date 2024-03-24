module Controller.Usuario where

data Usuario = Usuario {
    idUsuario :: String,
    senha :: String,
    seguidores :: [Usuario],
    seguindo :: [Usuario]
} deriving Show

cadastraUsuario ::  String->  String-> IO ()
cadastraUsuario nome senha = do
    let usuario = Usuario nome senha [] []
    putStrLn "cadastro realizado"
