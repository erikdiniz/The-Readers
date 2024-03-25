module Menu.MenuPerfil where
import Controller.Perfil


menuPerfil :: IO ()
menuPerfil = do
     putStrLn "Escolher opção: \n[V] Visão geral \n[E] Editar perfil\n[S] Voltar ao menu principal"
     opcao <- getLine
     selecionaOpcao opcao


selecionaOpcao :: String -> IO ()
selecionaOpcao "V" = do
      putStrLn "calma que jaja chega!"

selecionaOpcao "E" = do
      putStrLn "calma que jaja chega!"

selecionaOpcao "S" = do
     putStrLn "calma que jaja chega!"

selecionaOpcao "" = do
     putStrLn "Opção Inválida"
     menuPerfil

