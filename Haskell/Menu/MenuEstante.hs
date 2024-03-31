module Menu.MenuEstante where

import Controller.Estante
import Controller.Usuario

submenuEstante :: Usuario -> IO ()
submenuEstante usuario = do
    putStrLn $ "\nEscolher opção:\n[C] Cadastrar livro nas estantes\n[L] Lendo\n[I] Lidos \n[A] Abandonados \n[S] Voltar ao menu principal"

    x <- getLine
    selecionaAcaoEstante usuario x 

selecionaAcaoEstante :: Usuario -> String -> IO ()
selecionaAcaoEstante usuario "C" =  do
    putStrLn "Nome do livro: "
    nomeLivro <- getLine

    putStrLn "Qual estante você quer cadastrar?"
    tipoEstante <- getLine

    let cadastraLivro = adicionaLivro nomeLivro tipoEstante
    submenuEstante usuario

selecionaAcaoEstante usuario "L" = do
    let estante = getEstanteUnsafe
    let lendo = getCampoEstante "lendo" estante
    print(lendo)
    submenuEstante usuario

selecionaAcaoEstante usuario "I" = do
    let estante = getEstanteUnsafe
    let lidos = getCampoEstante "lidos" estante
    print(lidos)
    submenuEstante usuario

selecionaAcaoEstante usuario "P" = do
    let estante = getEstanteUnsafe
    let pretendidos = getCampoEstante "pretendo ler" estante
    print(pretendidos)
    submenuEstante usuario

selecionaAcaoEstante usuario "A" = do
    let estante = getEstanteUnsafe
    let abandonados = getCampoEstante "abandonados" estante
    print(abandonados)
    submenuEstante usuario

selecionaAcaoEstante usario "S" = putStrLn ""
