module Menu.MenuPerfil where
import Controller.Perfil
import Controller.Usuario

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
     putStrLn "calma que jaja chega!"

selecionaOpcao usuario "" = do
     putStrLn "Opção Inválida"
     menuPerfil usuario

menuPerfilStalker :: Usuario -> IO()
menuPerfilStalker usuario = do
     putStrLn "Digite o login do perfil que você deseja visitar:"
     userVisitado <- getLine
     visaoStalker userVisitado
     menuPerfil usuario
