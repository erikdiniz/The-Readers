:- module(estatisticas, [menuEstatisticas/1, menuEstatisticasAdmin/1]).
:- use_module("../Menu/MenuLogado.pl").
:- use_module("../Controller/Leitura.pl").
:- use_module("../Controller/Livro.pl").
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
% Recebe uma lista com os elemntos duplicados e uma lista com os elementos sem duplicata
imprimeNumLivros(_, []):-
    writeln("Nenhum livro encontrado."), nl.
imprimeNumLivros(ElementoDup, [Elemento]):-
    format("~w - 1 livro(s)", [Elemento]), nl.
imprimeNumLivros(ElementosDup, [Elemento|Resto]):-
    contaOcorrencias(Elemento, ElementosDup, Qntd),
    format("~w - ~w livro(s)", [Elemento, Qntd]), nl,
    imprimeNumLivros(ElementosDup, Resto).

% Exibe as estatísticas por autor das leituras de um usuário
autoresUser(Usuario):-
    recuperaAutoresLidos(Usuario, AutoresDup),
    list_to_set(AutoresDup, Autores),
    nl,
    writeln("--------------------------------------"),
    writeln("|              AUTORES               |"),
    writeln("--------------------------------------"), nl,
    imprimeNumLivros(AutoresDup, Autores).

% Exibe as estatísticas por gênero das leituras de um usuário
generosUser(Usuario):-
    recuperaGenerosLidos(Usuario, GenerosDup),
    list_to_set(GenerosDup, Generos),
    nl,
    writeln("--------------------------------------"),
    writeln("|              GENEROS               |"),
    writeln("--------------------------------------"), nl,
    imprimeNumLivros(GenerosDup, Generos).

% Predicado para extrair o ano de uma data no formato "dd/mm/aaaa"
extraiAno([], []).
extraiAno([Data|Resto], [Ano|AnosRestantes]) :-
    split_string(Data, "/", "", [_, _, AnoString]),
    atom_number(AnoString, Ano),
    extraiAno(Resto, AnosRestantes).

% Exibe as estatísticas por ano das leituras de um usuário
lidosAnoUser(Usuario):-
    recuperaDatasLidos(Usuario, Datas),
    extraiAno(Datas, AnosDup),
    list_to_set(AnosDup, Anos),
    nl,
    writeln("--------------------------------------"),
    writeln("|            LIDOS POR ANO           |"),
    writeln("--------------------------------------"), nl,
    imprimeNumLivros(AnosDup, Anos).

% Exibe o Menu Estatísticas do adm
menuEstatisticasAdmin(Admin):-
    nl,
    writeln("--------------------------------------"),
    writeln("|        ESTATISTICAS GERAIS         |"),
    writeln("--------------------------------------"), nl,
    writeln("[1] Generos dos livros cadastrados"),
    writeln("[2] Autores dos livros cadastrados"),
    writeln("[3] Livros com melhor avaliacao"),
    writeln("[4] Numero de livros e leituras"),
    writeln("[S] Voltar ao menu"),
    read_line_to_string(user_input, Opcao),
    selecionaEstatisticasAdmin(Opcao, Admin).

selecionaEstatisticasAdmin(Opcao, Admin):- (
        Opcao == "1" -> generosCadastrados, menuEstatisticasAdmin(Admin);
        Opcao == "2" -> autoresCadastrados, menuEstatisticasAdmin(Admin);
        Opcao == "3" -> melhoresLivros, menuEstatisticasAdmin(Admin);
        Opcao == "4" -> totalLivrosLeituras, menuEstatisticasAdmin(Admin);
        Opcao == "S" -> menuLogadoAdm(Admin)
    ).

% Exibe os gêneros cadastrados no sistema e a quantidade de livros de cada um
generosCadastrados:-
    lerJSON("../Data/livros.json", Livros),
    listaGeneros(Livros, GenerosDup),
    list_to_set(GenerosDup, Generos),
    length(Generos, TotalGeneros), nl,
    writeln("--------------------------------------"),
    writeln("|        GENEROS CADASTRADOS         |"),
    writeln("--------------------------------------"), nl,
    (
        TotalGeneros == 0 -> writeln("Nenhum livro encontrado.");
        imprimeNumLivros(GenerosDup, Generos), nl, nl,
        format("~w genero(s) cadastrados.", [TotalGeneros]), nl
    ).

% Exibe os autores cadastrados no sistema e a quantidade de livros de cada um
autoresCadastrados:-
    lerJSON("../Data/livros.json", Livros),
    listaAutores(Livros, AutoresDup),
    list_to_set(AutoresDup, Autores),
    length(Autores, TotalAutores), nl,
    writeln("--------------------------------------"),
    writeln("|        AUTORES CADASTRADOS         |"),
    writeln("--------------------------------------"), nl,
    (
        TotalAutores == 0 -> writeln("Nenhum livro encontrado.");
        imprimeNumLivros(AutoresDup, Autores), nl, nl,
        format("~w autores(s) cadastrados.", [TotalAutores]), nl
    ).

% Retorna uma lista com os livros que possuem nota 5
livrosNota5([], []).
livrosNota5([Leitura|Resto], [Leitura|LeituraResto]):-
    Leitura.nota == 5,
    livrosNota5(Resto, LeituraResto).
livrosNota5([_|Resto], LeituraResto):-
    livrosNota5(Resto, LeituraResto).

% Imprime os titulos dos livros e suas notas
imprimeLivrosNotas([]).
imprimeLivrosNotas([Livro]):-
    format("~w - Nota ~w", [Livro.titulo_lido, Livro.nota]).
imprimeLivrosNotas([Livro|Resto]):-
    format("~w - Nota ~w", [Livro.titulo_lido, Livro.nota]),
    imprimeLivrosNotas(Resto).

% Exibe os livros avaliados com nota 5
melhoresLivros:-
    lerJSON("../Data/leituras.json", Leituras),
    livrosNota5(Leituras, LivrosNota5Dup),
    list_to_set(LivrosNota5Dup, LivrosNota5),
    length(LivrosNota5, TotalLivros), nl,
    writeln("--------------------------------------"),
    writeln("|          MELHORES LIVROS           |"),
    writeln("--------------------------------------"), nl,
    (
        TotalLivros == 0 -> writeln("Nenhum livro encontrado.");
        imprimeLivrosNotas(LivrosNota5), nl, nl,
        format("~w livros(s) com nota 5.", [TotalLivros]), nl
    ).

% Exibe a quantidade de livros e leituras cadastradas no sistema 
totalLivrosLeituras:-
    lerJSON("../Data/leituras.json", Leituras),
    lerJSON("../Data/livros.json", Livros),
    length(Leituras, TotalLeituras),
    length(Livros, TotalLivros), nl,
    writeln("--------------------------------------"),
    writeln("|          LIVROS E LEITURAS         |"),
    writeln("--------------------------------------"), nl,
    format("~w livro(s) cadastrado(s).", [TotalLivros]), nl,
    format("~w leitura(s) realizadas.", [TotalLeituras]), nl.