module Menu.MenuLogin where

import System.Exit (exitSuccess)
import Controller.Usuario (cadastraUsuario)


menuLogin :: IO ()
menuLogin = do
     putStrLn "Escolher opção: [C] Cadastro de Usuario"
     opcao <- getLine 
     selecionaAcao opcao


selecionaAcao :: String -> IO ()
selecionaAcao "C" = do
    putStrLn "Insira seu nome de login: "
    login <- getLine

    putStrLn "Insira sua senha: "
    senha <- getLine
    cadastraUsuario login senha
    putStrLn "Usuário Cadastrado!"