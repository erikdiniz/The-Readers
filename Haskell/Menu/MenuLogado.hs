module Menu.MenuLogado where 


import Controller.Usuario
{- Guarda as funcionalidades de quando um usuário está logado -}
exibeMenuLogado :: Usuario -> IO()
exibeMenuLogado usuario = do
    putStrLn "Bem vindo ao The Readers \n [S] sair "
    opcao <- getLine
    selecionaAcaoLogin opcao

selecionaAcaoLogin :: String -> IO ()
selecionaAcaoLogin "S" = do
    putStrLn "Obrigado"
