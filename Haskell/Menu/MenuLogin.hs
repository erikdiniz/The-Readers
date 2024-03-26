module Menu.MenuLogin where

import Controller.Usuario 
import Menu.MenuLogado
import Controller.Livro(cadastraLivro,deletaLivro)
import Prelude (putStrLn, getLine, IO, String, Int, read)
import Data.Maybe

menuLogin :: IO ()
menuLogin = do
     putStrLn "Escolher opção: \n[C] Cadastro de Usuario \n[L] Realizar Login\n[D] Cadastro de Livro\n[E] Excluir um livro\n[S] Sair do Sistema "
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

selecionaAcao "D" = do
     putStrLn "Nome do livro: "
     nome <- getLine

     putStrLn "Autor: "
     autor <- getLine

     putStrLn "Número de páginas: "
     input <- getLine
     let n_paginas = read input :: Int  

     putStrLn "Gênero: "
     genero <- getLine

     cadastraLivro nome autor n_paginas genero

selecionaAcao "E" = do
     putStrLn "Nome do livro a ser excluido: "
     nomeLivro <- getLine
     deletaLivro nomeLivro 

selecionaAcao "" = do
     putStrLn "Opção Inválida"
     menuLogin

