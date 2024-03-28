module Menu.MenuEstatisticas where

import Controller.Estatisticas

menuEstatisticas :: IO ()
menuEstatisticas = do
    putStrLn "[E] Estatísticas do Perfil"
    opcao <- getLine
    selecionaAcao opcao

subMenuEstatisticas :: IO ()
subMenuEstatisticas = do
    putStrLn $ ".----------------------------------------------------------." ++ "\n"
          ++ "|                     Estatísticas                         |" ++ "\n"
          ++ "|                                                          |" ++ "\n"
          ++ "|                  Selecione uma opção:                    |" ++ "\n"
          ++ "|                                                          |" ++ "\n"
          ++ "|               [V] Visão Geral                            |" ++ "\n"
          ++ "|               [A] Autores                                |" ++ "\n"
          ++ "|               [G] Gêneros                                |" ++ "\n"
          ++ "|               [N] Avaliações                             |" ++ "\n"
          ++ "|               [S] Voltar para o menu                     |" ++ "\n"
          ++ ".----------------------------------------------------------." ++ "\n"
    x <- getLine
    selecionaAcao x

selecionaAcao :: String -> IO ()
selecionaAcao "e" = subMenuEstatisticas
selecionaAcao "v" =  do
    estatisticasGerais
    subMenuEstatisticas
selecionaAcao "s" = menuEstatisticas