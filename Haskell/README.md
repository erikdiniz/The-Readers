## Requisitos
Para rodar o nosso projeto, além do Haskell e do ghc, é necessário a instalação das bibliotecas aeson e aeson-pretty utilizadas na manipulação dos arquivos JSON. Elas e os módulos necessários estão presentes no arquivo Haskell.cabal e você pode obter elas executando os seguintes comandos em seu terminal:

     cabal update 
     cabal install --only-dependencies

## Compilação
Para compilar e executar nosso projeto, execute os seguintes comandos:

     cabal build
     cabal run