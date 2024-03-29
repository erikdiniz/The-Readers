module Menu.MenuLogado where
import Controller.Usuario
import Controller.Livro
import Controller.Perfil
import Data.Typeable



exibeMenuLogado :: Usuario -> IO()
exibeMenuLogado usuario = do
    putStrLn $ "-------------------------------------" ++ "\n"
    putStrLn $ "|       Bem vindo ao The Readers     |" ++ "\n"
    putStrLn $ "-------------------------------------" ++ "\n"
    putStrLn "\n [P] Meu Perfil\n [A] Adicionar amigo\n [B] Buscar usuário\n [+] Cadastro de Livro\n [-] Excluir um livro\n [S] Sair"
    opcao <- getLine
    selecionaAcaoLogin usuario opcao

selecionaAcaoLogin :: Usuario -> String -> IO ()
selecionaAcaoLogin usuario "S" = do
    putStrLn "Obrigado"

selecionaAcaoLogin usuario "P" = do
     menuPerfil usuario

selecionaAcaoLogin usuario "+" = do
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

selecionaAcaoLogin usuario "-" = do
    putStrLn "Nome do livro a ser excluido: "
    nomeLivro <- getLine
    deletaLivro nomeLivro

selecionaAcaoLogin usuario "B" = do
    menuPerfilStalker usuario

selecionaAcaoLogin usuario "A" = do
    let nomesUsuarios = recuperaNomeDeUsuarios recuperaUsuariosUnsafe []
    let nomesFiltrados = (removeElementos nomesUsuarios ([idUsuario usuario] ++ seguindo usuario))
    putStrLn "Usuários: "
    putStrLn ""
    imprimeLista nomesFiltrados
    putStrLn "Seguir usuário: "
    seguir <- getLine
    tentaSeguir usuario nomesFiltrados seguir

tentaSeguir :: Usuario -> [[Char]] -> String -> IO()
tentaSeguir usuario validos seguir = do
    case (seguir `elem` validos) of
        True -> do
            atualizaSeguindo usuario seguir 
            putStrLn "Usuário seguido com sucesso!"
            exibeMenuLogado usuario
        False -> do 
            putStrLn "Usuário inválido"
            exibeMenuLogado usuario
    
    
imprimeLista :: [[Char]] -> IO()
imprimeLista [] = putStrLn ""
imprimeLista (x:xs) = do
     putStr "- " 
     putStrLn(x) 
     imprimeLista xs

removeElementos :: [String] -> [String] -> [String]
removeElementos listaTotal remover = [nome | nome <- listaTotal, not (nome `elem` remover)]

menuPerfil :: Usuario -> IO()
menuPerfil usuario = do
     putStrLn "\nEscolher opção: \n [V] Visão geral \n [E] Editar meu perfil\n [S] Voltar ao menu principal"
     opcao <- getLine
     selecionaOpcao usuario opcao


selecionaOpcao :: Usuario -> String -> IO ()

selecionaOpcao usuario "V" = do
    visaoGeral (idUsuario usuario) (seguidores usuario) (seguindo usuario)
    putStrLn $ "\n--------------------------------------" ++ "\n"
    menuPerfil usuario

selecionaOpcao usuario "E" = do
    putStrLn "Escolha seu nome: "
    nome <- getLine

    putStrLn "Escolha sua biografia: "
    biografia <- getLine

    criarPerfil nome biografia (idUsuario usuario)
    putStrLn "Perfil salvo com sucesso!"

    menuPerfil usuario

selecionaOpcao usuario "S" = do
    exibeMenuLogado usuario

selecionaOpcao usuario "" = do
     putStrLn "Opção Inválida"
     menuPerfil usuario

menuPerfilStalker :: Usuario -> IO()
menuPerfilStalker usuario = do
     putStrLn "Digite o login do perfil que você deseja visitar:"
     userVisitado <- getLine
     visaoStalker userVisitado
     menuPerfil usuario
