module Menu.MenuLogado where 
import Menu.MenuPerfil
import Controller.Usuario
import Controller.Livro


exibeMenuLogado :: Usuario -> IO()
exibeMenuLogado usuario = do
    putStrLn $ "-------------------------------------" ++ "\n"
    putStrLn $ "|       Bem vindo ao The Readers     |" ++ "\n"
    putStrLn $ "-------------------------------------" ++ "\n"
    putStrLn "\n [P] Meu Perfil\n [+] Cadastro de Livro\n [-] Excluir um livro\n [S] sair"
    opcao <- getLine
    selecionaAcaoLogin opcao

selecionaAcaoLogin :: String -> IO ()
selecionaAcaoLogin "S" = do
    putStrLn "Obrigado"

selecionaAcaoLogin "P" = do
     menuPerfil

selecionaAcaoLogin "+" = do
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

selecionaAcaoLogin "-" = do
    putStrLn "Nome do livro a ser excluido: "
    nomeLivro <- getLine
    deletaLivro nomeLivro

