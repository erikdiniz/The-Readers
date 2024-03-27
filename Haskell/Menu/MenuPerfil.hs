module Menu.MenuPerfil where
import Controller.Perfil


menuPerfil :: IO ()
menuPerfil = do
     putStrLn "\nEscolher opção: \n[V] Visão geral \n[E] Editar perfil\n[S] Voltar ao menu principal"
     opcao <- getLine
     selecionaOpcao opcao


selecionaOpcao :: String -> IO ()

selecionaOpcao "V" = do
    visaoGeral
    putStrLn $ "\n--------------------------------------" ++ "\n"
    menuPerfil

selecionaOpcao "E" = do
    putStrLn "Escolha seu nome: "
    nome <- getLine

    putStrLn "Escolha sua biografia: "
    biografia <- getLine

    criarPerfil nome biografia
    putStrLn "Perfil salvo com sucesso!"

    menuPerfil

selecionaOpcao "S" = do
     putStrLn "calma que jaja chega!"

selecionaOpcao "" = do
     putStrLn "Opção Inválida"
     menuPerfil

