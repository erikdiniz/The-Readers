module Menu.MenuLogado where
import Controller.Usuario
import Controller.Livro
import Controller.Perfil
import Controller.Leitura
import Controller.Avaliacao
import Controller.Admin
import Controller.Estatisticas
import Menu.MenuEstatisticas
import Data.Maybe


exibeMenuLogado :: Usuario -> IO()
exibeMenuLogado usuario = do
    putStrLn $ "-------------------------------------" ++ "\n"
    putStrLn $ "|       Bem vindo ao The Readers     |" ++ "\n"
    putStrLn $ "-------------------------------------" ++ "\n"

    putStrLn "\n [P] Meu Perfil\n [F] Feed\n [U] Seguir usuário\n [B] Buscar usuário\n [L] Cadastrar Leitura\n [R] Criar Resenha\n [+] Cadastro de Livro\n [-] Excluir um livro\n [M] Minhas Estantes\n [E] Estatísticas\n [S] Sair"


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

selecionaAcaoLogin usuario "F" = do
    maybeAvaliacoes <- recuperaAvaliacoesSafe
    let avaliacoes = fromJust maybeAvaliacoes
    seguindo <- recuperaSeguidores usuario
    let avaliacoesSeguindo = recuperaAvaliacoesPorNome (seguindo) avaliacoes
    simuladorFeed usuario avaliacoesSeguindo
    exibeMenuLogado usuario

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

selecionaAcaoLogin usuario "E" = do
    putStrLn $ "-------------------------------------" ++ "\n"
    putStrLn $ "|            Estatísticas           |" ++ "\n"
    putStrLn $ "-------------------------------------" ++ "\n"
    menuEstatisticas usuario
    exibeMenuLogado usuario

selecionaAcaoLogin usuario "R" = do
    putStrLn "Qual leitura você deseja resenhar:"
    todasLeituras <- recuperaLeituraSafe
    case todasLeituras of
        Just leituras -> do 
            let lista_leituras = recuperaLeituraDoUsuarioSafe (idUsuario usuario) leituras 
            let nomes_das_leituras = concatenaLidos lista_leituras []

            imprimeLista nomes_das_leituras

            nomeLeitura <- getLine

            let maybeLeitura = procuraLeitura (idUsuario usuario) nomeLeitura (recuperaLeituraDoUsuarioSafe (idUsuario usuario) leituras)

            putStrLn "Escreva sua resenha: "
            resenha <- getLine
            
            tentaResenhar usuario maybeLeitura resenha
        Nothing -> putStrLn "Falha no JSON"
    


tentaResenhar :: Usuario -> Maybe Leitura -> String -> IO()
tentaResenhar usuario maybeLeitura resenha = 
    case maybeLeitura of
        Just leitura -> do
            fazAvaliacao (idUsuario usuario) leitura resenha
            putStrLn "Resenha cadastrada!"
            exibeMenuLogado usuario
        Nothing -> do
            putStrLn "Título não lido"
            exibeMenuLogado usuario



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

menuPerfil :: Usuario -> IO()
menuPerfil usuario = do
     putStrLn "\nEscolher opção: \n [V] Visão geral \n [E] Editar meu perfil\n [M] Minhas Resenhas\n [S] Voltar ao menu principal"
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

selecionaOpcao usuario "M" = do
    carregaAvaliacoes usuario

selecionaOpcao usuario "" = do
     putStrLn "Opção Inválida"
     menuPerfil usuario



{-Carrega as Resenhas do Usuário Logado-}
carregaAvaliacoes :: Usuario -> IO()
carregaAvaliacoes usuario = do
    avaliacoes <- recuperaAvaliacoesSafe
    case avaliacoes of
        Just avaliacoes -> do
            let minhasAvaliacoes = recuperaAvaliacoesUsuario [usuario] avaliacoes
            putStrLn "----------- RESENHAS ----------"
            imprimeAvaliacoes minhasAvaliacoes
            menuPerfil usuario
        Nothing -> do
            putStrLn "Nenhuma avaliação"
            menuPerfil usuario

{-Itera imprimindo cada avaliação-}
imprimeAvaliacoes :: [Avaliacao] -> IO()
imprimeAvaliacoes [] = putStrLn "------------- FIM -------------"
imprimeAvaliacoes (x:xs) = do 
    let string = mostraAvaliacao x 
    putStrLn "-------------------------------"
    putStrLn string
    imprimeAvaliacoes xs

menuEstante :: Usuario -> IO()
menuEstante usuario = do
    putStrLn "\nEscolher opção: \n [+] Adicionar Livro \n [L] Lidos\n [E] Lendo\n [P] Pretendo Ler\n [A] Abandonados\n [S] Voltar para menu principal"
    opcao <- getLine
    selecionaOpcao usuario opcao    


menuPerfilStalker :: Usuario -> IO()
menuPerfilStalker usuario = do
     putStrLn "Digite o login do perfil que você deseja visitar:"
     userVisitado <- getLine
     visaoStalker userVisitado
     menuPerfil usuario

exibeDashAdm :: Admin -> IO()
exibeDashAdm adm = do
    putStrLn $ "-------------------------------------" ++ "\n"
    putStrLn $ "|     Dashboard de Administrador     |" ++ "\n"
    putStrLn $ "-------------------------------------" ++ "\n"

    putStrLn "\n [1] Estatísticas Gerais\n [2] Listar de usuários cadastrados\n [3] Listar livros cadastrados\n [+] Cadastrar novo adm\n [S] Sair"

    escolha <- getLine
    escolhaAdm adm escolha

escolhaAdm :: Admin -> String -> IO ()
escolhaAdm adm "1" = do
    exibeEstatisticasAdmin
    exibeDashAdm adm

escolhaAdm adm "2" = do
    listaUsers adm
    exibeDashAdm adm

escolhaAdm adm "3" = do
    listaLivros adm
    exibeDashAdm adm

escolhaAdm adm "+" = do
    putStrLn "Novo login administrador: "
    login <- getLine

    putStrLn "Escolha uma senha: "
    senha <- getLine
    cadastraAdm login senha
    putStrLn "Adm Cadastrado!"
    exibeDashAdm adm

escolhaAdm adm "S" = do
    putStrLn "Até a próxima!"

escolhaAdm adm "" = do
     putStrLn "Opção Inválida"
     exibeDashAdm adm

