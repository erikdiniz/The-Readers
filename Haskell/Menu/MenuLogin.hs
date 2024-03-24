module Menu.MenuLogin where

import System.Exit (exitSuccess)
import Controller.Usuario (cadastraUsuario)


menuLogin :: IO ()
menuLogin = do
     putStrLn "Escolher opção: [C] Cadastro de Usuario"
     opcao <- getLine 
     selecionaAcao opcao


selecionaAcao :: String -> IO ()
selecionaAcao "c" = do
    login <- getLine
    senha <- getLine
    cadastraUsuario login senha

