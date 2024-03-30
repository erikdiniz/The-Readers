module Menu.MenuLogado where 
import Menu.MenuPerfil
import Controller.Usuario
import Controller.Livro
import Controller.Leitura
import Data.Typeable
import Data.Maybe


exibeMenuLogado :: Usuario -> IO()
exibeMenuLogado usuario = do
    putStrLn $ "-------------------------------------" ++ "\n"
    putStrLn $ "|       Bem vindo ao The Readers     |" ++ "\n"
    putStrLn $ "-------------------------------------" ++ "\n"
    putStrLn "\n [P] Meu Perfil\n [U] Seguir usuário\n [L] Cadastrar Leitura\n [+] Cadastro de Livro\n [-] Excluir um livro\n [S] sair"
    opcao <- getLine
    selecionaAcaoLogin usuario opcao

selecionaAcaoLogin :: Usuario -> String -> IO ()
selecionaAcaoLogin usuario "S" = do
    putStrLn "Obrigado"

selecionaAcaoLogin usuario "P" = do
     menuPerfil

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

selecionaAcaoLogin usuario "U" = do
    let nomesUsuarios = recuperaNomeDeUsuarios recuperaUsuariosUnsafe []
    let nomesFiltrados = (removeElementos nomesUsuarios ([idUsuario usuario] ++ seguindo usuario))
    putStrLn "Usuários: "
    putStrLn ""
    imprimeLista nomesFiltrados
    putStrLn "Seguir usuário: "
    seguir <- getLine
    tentaSeguir usuario nomesFiltrados seguir

selecionaAcaoLogin usuario "L" = do
    putStrLn "Nome do livro: "
    nomeLivro <- getLine
    putStrLn "Data da leitura: "
    dataLeitura <- getLine
    putStrLn "Nota da leitura (1 - 5)"
    nota <- readLn :: IO Int
    maybeListaLivros <- getListaLivros
    let listaLivros = fromMaybe [] maybeListaLivros
    let maybeLivro = getLivro nomeLivro listaLivros
    tentaCadastrar usuario maybeLivro dataLeitura nota 

tentaCadastrar :: Usuario -> Maybe Livro -> String -> Int -> IO()
tentaCadastrar usuario maybeLivro dataLeitura nota = do
    case maybeLivro of 
        Just livro -> do
            if nota >= 1 && nota <= 5 then do
                cadastrarLeitura (idUsuario usuario) livro dataLeitura nota
                putStrLn "Livro cadastrado!"
                exibeMenuLogado usuario
            else do 
                putStrLn "Nota invalida"
                exibeMenuLogado usuario
        Nothing -> do
            putStrLn "Livro não encontrado no sistema"
            exibeMenuLogado usuario
    
 


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