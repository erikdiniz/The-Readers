module Menu.MenuLogin where

import Controller.Usuario 
import Menu.MenuLogado


menuLogin :: IO ()
menuLogin = do
     putStrLn "Escolher opção: \n[C] Cadastro de Usuario \n[L] Realizar Login\n[S] Sair do Sistema "
     opcao <- getLine 
     selecionaAcao opcao

login :: String -> String -> IO()
login nome senha = do 
     let usuario = loginUsuario nome senha
     case usuario of
          Just usuario -> exibeMenuLogado usuario
          Nothing -> putStrLn "Credenciais Inválidas"

selecionaAcao :: String -> IO ()
selecionaAcao "C" = do
    putStrLn "Insira seu nome de login: "
    login <- getLine

    putStrLn "Insira sua senha: "
    senha <- getLine
    cadastraUsuario login senha
    putStrLn "Usuário Cadastrado!"
    menuLogin

selecionaAcao "L" = do
     putStrLn "Nome de usuário: "
     nome <- getLine

     putStrLn "Senha: "
     senha <- getLine
     login nome senha 

selecionaAcao "S" = do
     putStrLn "Obrigado!"

selecionaAcao "" = do
     putStrLn "Opção Inválida"
     menuLogin

