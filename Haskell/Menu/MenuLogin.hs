module Menu.MenuLogin where

import Controller.Usuario
import Controller.Admin
import Menu.MenuLogado
import Prelude (putStrLn, getLine, IO, String, Int, read)
import Data.Maybe

menuLogin :: IO ()
menuLogin = do
     putStrLn "Escolher opção: \n [C] Cadastro de Usuario \n [L] Realizar Login \n [D] Dashboard Administrador \n [S] Sair do Sistema "
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

selecionaAcao "D" = do
     dashAdm

selecionaAcao "S" = do
     putStrLn "Obrigado!"

selecionaAcao "" = do
     putStrLn "Opção Inválida"
     menuLogin

dashAdm :: IO ()
dashAdm = do
     putStrLn "Escolher opção: \n [L] Realizar Login \n [V] Voltar ao menu de login "
     acao <- getLine
     selecionaAcaoAdm acao

loginAdmMenu :: String -> String -> IO()
loginAdmMenu nome senha = do
     let adm = loginAdm nome senha
     case adm of
          Just adm -> exibeDashAdm adm
          Nothing -> putStrLn "Credenciais Inválidas"

selecionaAcaoAdm :: String -> IO ()
selecionaAcaoAdm "L" = do
     putStrLn "Login: "
     nome <- getLine

     putStrLn "Senha: "
     senha <- getLine
     loginAdmMenu nome senha

selecionaAcaoAdm "V" = do
     menuLogin

selecionaAcaoAdm "" = do
     putStrLn "Opção Inválida"
     dashAdm