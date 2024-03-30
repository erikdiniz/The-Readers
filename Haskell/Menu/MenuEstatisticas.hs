module Menu.MenuEstatisticas where

import Controller.Estatisticas
import Controller.Usuario

menuEstatisticas :: Usuario -> IO ()
menuEstatisticas usuario = do
    putStrLn $ "\nEscolher opção:\n[V] Visão Geral\n[A] Autores\n[G] Gêneros\n[S] Voltar ao menu"

    x <- getLine
    selecionaAcao usuario x 

selecionaAcao :: Usuario -> String -> IO ()
selecionaAcao usuario "V" =  do
    estatisticasGerais usuario
    menuEstatisticas usuario
selecionaAcao usario "S" = putStrLn $ ""