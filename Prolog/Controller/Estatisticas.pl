:- module(estatisticas, [menuEstatisticas/1, menuEstatisticasAdmin/1]).
:- use_module("../Menu/MenuLogado.pl").
:- use_module("../Controller/Leitura.pl").
:- use_module("../Util/util.pl").
:- use_module(library(http/json)).

% Exibe o Menu Estatísticas do usuário
menuEstatisticas(Usuario):-
    nl,
    writeln("--------------------------------------"),
    writeln("|            ESTATISTICAS            |"),
    writeln("--------------------------------------"), nl,
    writeln("[V] Visao Geral"),
    writeln("[A] Autores"),
    writeln("[G] Generos"),
    writeln("[L] Lidos por ano"),
    writeln("[S] Voltar ao menu"),
    read_line_to_string(user_input, Opcao),
    selecionaEstatisticasUser(Opcao, Usuario).

selecionaEstatisticasUser(Opcao, Usuario):- (
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

% Predicado para contar ocorrências de um elemento em uma lista
contaOcorrencias(_, [], 0).
contaOcorrencias(Elemento, [Elemento|Resto], Total) :-
    contaOcorrencias(Elemento, Resto, TotalResto),
    Total is TotalResto + 1.
contaOcorrencias(Elemento, [_|Resto], Total) :-
    contaOcorrencias(Elemento, Resto, Total).

% Predicado para encontrar o gênero mais lido de uma lista de leituras
generoMaisLido([], "Nenhum livro encontrado.").
generoMaisLido([Leitura], Leitura.genero_lido).
generoMaisLido([Leitura1, Leitura2|Resto], GeneroMaisLido) :-
    generoMaisLido(Resto, GeneroResto),
    contaOcorrencias(Leitura1.genero_lido, [Leitura1, Leitura2|Resto], Qntd1),
    contaOcorrencias(Leitura2.genero_lido, [Leitura1, Leitura2|Resto], Qntd2),
    (
        Qntd1 >= Qntd2 -> GeneroMaisLido = Leitura1.genero_lido
    ;
        GeneroMaisLido = GeneroResto
    ).

% Predicado para encontrar o autor mais lido de uma lista de leituras
autorMaisLido([], "Nenhum livro encontrado.").
autorMaisLido([Leitura], Leitura.autor_lido).
autorMaisLido([Leitura1, Leitura2|Resto], AutorMaisLido) :-
    autorMaisLido(Resto, AutorResto),
    contaOcorrencias(Leitura1.autor_lido, [Leitura1, Leitura2|Resto], Qntd1),
    contaOcorrencias(Leitura2.autor_lido, [Leitura1, Leitura2|Resto], Qntd2),
    (
        Qntd1 >= Qntd2 -> AutorMaisLido = Leitura1.autor_lido
    ;
        AutorMaisLido = AutorResto
    ).

% Retorna o livro com melhor avaliação
melhorAvaliado([], _{titulo_lido: "Nenhum livro encontrado.", nota: 0}).
melhorAvaliado([Leitura], Leitura).
melhorAvaliado([Leitura1, Leitura2|Resto], MelhorLivro):-
    melhorAvaliado([Leitura2|Resto], MelhorLivroRestante),
    (
        Leitura1.nota >= MelhorLivroRestante.nota -> MelhorLivro = Leitura1;
        MelhorLivro = MelhorLivroRestante
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
    writeln("|            VISAO GERAL             |"),
    writeln("--------------------------------------"), nl,
    format("Total de livros lidos: ~w", [TotalLeituras]), nl,
    format("Total de paginas lidas: ~w", [TotalPaginas]), nl,
    format("Genero mais lido: ~w", [Genero]), nl,
    format("Autor mais lido: ~w", [Autor]), nl,
    (
        MelhorAvaliado.nota == 0 -> writeln("Livro com maior nota: Nenhum livro encontrado."), nl;
        format("Livro com maior nota: ~w - ~w", [MelhorAvaliado.titulo_lido, MelhorAvaliado.nota]), nl
    ).

% Exibe a quantidade de livros por autor, gênero, ano ou qualquer outra propriedade de um livro
imprimeNumLivros([]):-
    writeln("Nenhum livro encontrado."), nl.
imprimeNumLivros([Elemento]):-
    format("~w - 1 livro(s)", [Elemento]),nl.
imprimeNumLivros([Elemento1, Elemento2|Resto]):-
    contaOcorrencias(Elemento1, [Elemento1, Elemento2|Resto], Qntd),
    format("~w - ~w livro(s)", [Elemento1, Qntd]), nl,
    imprimeNumLivros([Elemento2|Resto]).

% Exibe as estatísticas por autor das leituras de um usuário
autoresUser(Usuario):-
    recuperaAutoresLidos(Usuario, Autores), nl,
    writeln("--------------------------------------"),
    writeln("|              AUTORES               |"),
    writeln("--------------------------------------"), nl,
    imprimeNumLivros(Autores).

% Exibe as estatísticas por gênero das leituras de um usuário
generosUser(Usuario):-
    recuperaGenerosLidos(Usuario, Generos), nl,
    writeln("--------------------------------------"),
    writeln("|              GENEROS               |"),
    writeln("--------------------------------------"), nl,
    imprimeNumLivros(Generos).

% Predicado para extrair o ano de uma data no formato "dd/mm/aaaa"
extraiAno([], []).
extraiAno([Data|Resto], [Ano|AnosRestantes]) :-
    split_string(Data, "/", "", [_, _, AnoString]),
    atom_number(AnoString, Ano),
    extraiAno(Resto, AnosRestantes).

% Exibe as estatísticas por ano das leituras de um usuário
lidosAnoUser(Usuario):-
    recuperaDatasLidos(Usuario, Datas),
    extraiAno(Datas, Anos),
    writeln("--------------------------------------"),
    writeln("|            LIDOS POR ANO           |"),
    writeln("--------------------------------------"), nl,
    imprimeNumLivros(Anos).

% Exibe o Menu Estatísticas do adm
menuEstatisticasAdmin(Admin):-
    nl,
    writeln("--------------------------------------"),
    writeln("|        ESTATISTICAS GERAIS         |"),
    writeln("--------------------------------------"), nl,
    writeln("[1] Generos dos livros cadastrados"),
    writeln("[2] Autores dos livros cadastrados"),
    writeln("[3] Livros com melhor avaliaçao"),
    writeln("[4] Numero de leituras"),
    writeln("[S] Voltar ao menu"),
    read_line_to_string(user_input, Opcao),
    selecionaEstatisticasAdmin(Opcao, Admin).

selecionaEstatisticasAdmin(Opcao, Admin):- (
        Opcao == "1" -> generosCadastrados, menuEstatisticasAdmin(Admin);
        Opcao == "2" -> autoresCadastrados, menuEstatisticmsAdmin(Admin);
        Opcao == "3" -> melhoresLivros, menuEstatisticasAdmin(Admin);
        Opcao == "4" -> totalLeituras, menuEstatisticasAdmin(Admin);
        Opcao == "5" -> totalLivros, menuEstatisticasAdmin(Admin);
        Opcao == "S" -> menuLogadoAdm(Admin)
    ).