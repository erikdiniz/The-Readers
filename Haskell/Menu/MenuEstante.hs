module Menu.MenuEstante where

import Controller.Estante
import Controller.Usuario

-- Função para lidar com o submenu de estantes
submenuEstante :: Usuario -> Estantes -> IO ()
submenuEstante usuario estantes = do
    putStrLn $ "\nEscolher opção:\n[C] Cadastrar livro nas estantes\n[L] Lendo\n[I] Lidos\n[P] Pretendo ler\n[A] Abandonados\n[S] Voltar ao menu principal"
    x <- getLine
    selecionaAcaoEstante usuario x estantes

selecionaAcaoEstante :: Usuario -> String -> Estantes -> IO ()
selecionaAcaoEstante usuario "C" estantes = do
    putStrLn "Nome do livro:"
    nomeLivro <- getLine
    putStrLn "Qual estante você quer cadastrar?"
    tipoEstante <- getLine
    adicionaLivro nomeLivro tipoEstante (idUsuario usuario) estantes
    putStrLn $ "Livro adicionado à estante de " ++ tipoEstante ++ " do usuário " ++ idUsuario usuario ++ "."
    submenuEstante usuario estantes

selecionaAcaoEstante usuario opcao estantes
    | opcao `elem` ["L", "I", "P", "A"] = exibirLivrosNaEstante usuario estantes opcao
    | opcao == "S" = putStrLn "Voltando ao menu principal"
    | otherwise = putStrLn "Opção inválida."
    where
        exibirLivrosNaEstante :: Usuario -> Estantes -> String -> IO ()
        exibirLivrosNaEstante usuario estantes tipo = do
            case getEstanteUsuario (idUsuario usuario) estantes of
                Just e -> do
                    putStrLn $ "Livros na estante '" ++ tipo' ++ "':"
                    print $ case tipo of
                        "L" -> lendo e
                        "I" -> lidos e
                        "P" -> pretendo_ler e
                        "A" -> abandonados e
                Nothing -> putStrLn "Estante não encontrada."
            submenuEstante usuario estantes
            where
                tipo' = case tipo of
                    "L" -> "Lendo"
                    "I" -> "Lidos"
                    "P" -> "Pretendo ler"
                    "A" -> "Abandonados"

selecionaAcaoEstante usario "S" = putStrLn ""
