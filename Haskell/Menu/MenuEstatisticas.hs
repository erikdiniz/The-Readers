module Menu.MenuEstatisticas where

import Controller.Estatisticas
import Controller.Usuario
import Controller.Leitura

menuEstatisticas :: Usuario -> [Leitura] -> IO ()
menuEstatisticas usuario leituras = do
    putStrLn $ "\nEscolher opção:\n[V] Visão Geral\n[A] Autores\n[G] Gêneros \n[Y] Lidos por ano \n[S] Voltar ao menu"

    x <- getLine
    selecionaAcao usuario x leituras

selecionaAcao :: Usuario -> String -> [Leitura] -> IO ()
selecionaAcao usuario "V" leituras =  do
    estatisticasGerais usuario leituras
    menuEstatisticas usuario leituras

selecionaAcao usuario "A" leituras = do
    exibeAutoresQuantidadeLivrosUsuario usuario leituras
    menuEstatisticas usuario leituras

selecionaAcao usuario "G" leituras = do
    exibeLivrosPorGenero usuario leituras
    menuEstatisticas usuario leituras

selecionaAcao usuario "Y" leituras = do
    exibeLivrosPorAno usuario leituras
    menuEstatisticas usuario leituras

selecionaAcao usario "S" leituras = putStrLn ""