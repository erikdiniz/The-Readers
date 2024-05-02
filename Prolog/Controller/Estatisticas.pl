:- module(estatisticas, [menuEstatisticas/1]).
:- use_module("../Menu/MenuLogado.pl").
:- use_module("../Controller/Leitura.pl").
:- use_module("../Util/util.pl").
:- use_module(library(http/json)).

% Exibe o Menu Estatísticas do usuário
menuEstatisticas(Usuario):-
    nl,
    writeln("[V] Visao Geral"),
    writeln("[A] Autores"),
    writeln("[G] Generos"),
    writeln("[L] Lidos por ano"),
    writeln("[S] Voltar ao menu"),
    read_line_to_string(user_input, Opcao),
    selecionaEstatisticas(Opcao, Usuario).

selecionaEstatisticas(Opcao, Usuario):- (
        Opcao == "V" -> visaoGeralUser(Usuario), menuEstatisticas(Usuario);
        Opcao == "A" -> autoresUser(Usuario), menuEstatisticas(Usuario);
        Opcao == "G" -> generosUser(Usuario), menuEstatisticas(Usuario);
        Opcao == "L" -> lidosAnoUser(Usuario), menuEstatisticas(Usuario);
        Opcao == "S" -> menuLogado(Usuario)
    ).

% Retorna o total de páginas de uma lista de livros (leituras)
totalPaginas([], 0).
totalPaginas([X|XS], TotalPaginas):-
    totalPaginas(XS, TotalPaginas2),
    TotalPaginas is X.num_paginas + TotalPaginas2.

% Conta ocorrências de um elemento em uma lista
contaOcorrencias(_, [], 0).
contaOcorrencias(Elemento, [X|XS], Total):- (
        Elemento == X -> contaOcorrencias(Elemento, XS, Total2),
        Total is 1 + Total2;
        contaOcorrencias(Elemento, XS, Total)
    ).

% Retorna o gênero mais lido de uma lista de leituras.
generoMaisLido([], "Nenhum gênero lido.").
generoMaisLido([Leitura], Leitura.genero_lido).
generoMaisLido([Leitura1, Leitura2|Resto], Genero):-
    contaOcorrencias(Leitura1.genero_lido, Qntd1),
    contaOcorrencias(Leitura2.genero_lido, Qntd2),
    (
        Qntd1 >= Qntd2 -> Leitura1.genero_lido, generoMaisLido([Leitura2|Resto], Genero)
    ).

% Retorna o autor mais lido de uma lista de leituras.
autorMaisLido([], "Nenhum autor lido.").
autorMaisLido([Leitura], Leitura.autor_lido).
autorMaisLido([Leitura1, Leitura2|Resto], AutorMaisLido):-
    contaOcorrencias(Leitura1.autor_lido, Qntd1),
    contaOcorrencias(Leitura2.autor_lido, Qntd2),
    (
        Qntd1 >= Qntd2 -> autorMaisLido([Leitura1|Resto], AutorMaisLido)
    ;
        autorMaisLido([Leitura2|Resto], AutorMaisLido)
    ).

% Retorna o livro com melhor avaliação
melhorAvaliado([], "Nenhum livro encontrado.").
melhorAvaliado([Leitura], Leitura).
melhorAvaliado([Leitura1, Leitura2|Resto], MelhorLivro):-
    (
        Leitura1.nota >= Leitura2.nota -> melhorAvaliado([Leitura1|Resto], MelhorLivro);
        melhorAvaliado([Leitura2|Resto], MelhorAvaliado)
    ).

% Exibe as estatísticas gerais de um usuário
visaoGeralUser(Usuario):-
    recuperaLeiturasUsuario(Usuario, Leituras),
    length(Leituras, TotalLeituras),
    totalPaginas(Leituras, TotalPaginas),
    generoMaisLido(Leituras, Genero),
    autorMaisLido(Leituras, Autor),
    melhorAvaliado(Leituras, MelhorAvaliado), nl,
    writeln("--------------------------------------"),
    writeln("|        Estatisticas                |"),
    writeln("--------------------------------------"), nl
    format("Total de livros lidos: ~w", [TotalLeituras]), nl,
    format("Total de páginas lidas: ~w", [TotalPaginas]), nl,
    format("Genero mais lido: ~w", [Genero]), nl,
    format("Autor mais lido: ~w", [Autor]), nl,
    format("Livro com maior nota: ~w - ~w", [MelhorAvaliado.titulo_lido, MelhorAvaliado.nota]), nl.