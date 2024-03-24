module Menu.MenuLogin where

import System.Exit (exitSuccess)
import Controller.Usuario (cadastraUsuario)


menuLogin :: IO ()
menuLogin = do
     putStrLn "Escolher opção: \n[C] Cadastro de Usuario \n[L] Realizar Login "
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
selecionaAcao "L" = do
     putStrLn "Nome de usuário: "
     login <- getLine

     putStrLn "Senha: "
     senha <- getLine
     putStrLn "Login realizado!"

selecionaAcao "" = do
     putStrLn "Opção Inválida"
     menuLogin

