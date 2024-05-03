:- module(leitura, [cadastrarLeitura/1, recuperaTitulosLidos/2, recuperaGenerosLidos/2, recuperaLeiturasUsuario/2, recuperaAutoresLidos/2, recuperaDatasLidos/2]).
:- use_module("../Controller/Livro.pl").
:- use_module("../Util/util.pl").
:- use_module("../Menu/MenuLogado.pl").


% Interação com usuário para cadastrar leitura
cadastrarLeitura(Usuario):-
    tty_clear,
    lista_livros(Titulos),
    writeln("Títulos disponíveis: "),
    imprimeListaString(Titulos),
    writeln("Cadastrar leitura de: "),
    read_line_to_string(user_input, Titulo),
    (member(Titulo,Titulos) -> coletaAvaliacao(Usuario,Titulo);
                               tty_clear, writeln("Título inválido"), nl, menuLogado(Usuario)). 
% Interação com usuário para cadastrar leitura
coletaAvaliacao(Usuario, Titulo):-
    recupera_livro(Titulo, Livro),
    writeln("Data da leitura: "),
    read_line_to_string(user_input, Data),
    writeln("Nota da leitura"),
    read_line_to_string(user_input, SNota),
    atom_number(SNota, Nota),
    (validaNota(Nota) -> 
    criaLeitura(Usuario, Livro, Data, Nota),
    nl, tty_clear,
    writeln("Leitura Cadastrada!"),
    nl,
    menuLogado(Usuario);
    tty_clear, nl, writeln("Nota inválida"), nl, menuLogado(Usuario)
    ).

validaNota(Nota):- Nota =< 5, Nota >=1.

%Cria leitura
criaLeitura(Usuario, Livro, Data, Nota):-
    Leitura = _{id_usuario: Usuario.nome, autor_lido: Livro.autor, data: Data, genero_lido: Livro.genero, num_paginas: Livro.n_paginas, nota: Nota, titulo_lido: Livro.nome},
    adicionaLeitura(Leitura).
%Escreve leitura no JSON
adicionaLeitura(Leitura):-
    lerJSON('../Data/leituras.json', Leituras),
    append([Leitura], Leituras, LeiturasAtualizadas),
    escreveJSON('../Data/leituras.json', LeiturasAtualizadas).

% Pega as leituras de um usuário
recuperaLeiturasUsuario(Usuario, LeiturasUsuario):-
    lerJSON('../Data/leituras.json', Leituras),
    recuperaLeiturasUsuario(Usuario.nome, Leituras, [], LeiturasUsuario).

recuperaLeiturasUsuario(Nome, [X|XS], Acc, Resultado):-
    (X.id_usuario == Nome -> append([X], Acc, NewAcc), recuperaLeiturasUsuario(Nome, XS, NewAcc, Resultado);
                             recuperaLeiturasUsuario(Nome, XS, Acc, Resultado)).
recuperaLeiturasUsuario(_,[],Resultado,Resultado):-!.

% Pega os titulos lidos por um usuário
recuperaTitulosLidos(Usuario, TitulosLidos):-
    lerJSON('../Data/leituras.json', Leituras),
    recuperaLeiturasUsuario(Usuario.nome, Leituras, [], LeiturasUsuario),
    pegaTitulos(LeiturasUsuario,[], TitulosLidos).

% Pega os titulos de uma lista de leituras
pegaTitulos([X|XS], Acc, Resultado):-
    append([X.titulo_lido], Acc, NewAcc),
    pegaTitulos(XS, NewAcc, Resultado).
pegaTitulos([], Resultado, Resultado):-!.

% Pega os gêneros lidos por um usuário
recuperaGenerosLidos(Usuario, GenerosLidos) :-
    lerJSON('../Data/leituras.json', Leituras),
    recuperaLeiturasUsuario(Usuario.nome, Leituras, [], LeiturasUsuario),
    pegaGeneros(LeiturasUsuario, GenerosLidos).

% Pega os gêneros de uma lista de leituras
pegaGeneros([Leitura|LeiturasRestantes], [Leitura.genero_lido|GenerosRestantes]) :-
    pegaGeneros(LeiturasRestantes, GenerosRestantes).
pegaGeneros([], []).

% Pega os autores lidos por um usuário
recuperaAutoresLidos(Usuario, AutoresLidos):-
    lerJSON('../Data/leituras.json', Leituras),
    recuperaLeiturasUsuario(Usuario.nome, Leituras, [], LeiturasUsuario),
    pegaAutores(LeiturasUsuario,[], AutoresLidos).

% Pega os autores de uma lista de leituras
pegaAutores([X|XS], Acc, Resultado):-
    append([X.autor_lido], Acc, NewAcc),
    pegaAutores(XS, NewAcc, Resultado).
pegaAutores([], Resultado, Resultado):-!.

% Pega as datas dos livros lidos por um usuário
recuperaDatasLidos(Usuario, DatasLidos):-
    lerJSON('../Data/leituras.json', Leituras),
    recuperaLeiturasUsuario(Usuario.nome, Leituras, [], LeiturasUsuario),
    pegaDatas(LeiturasUsuario,[], DatasLidos).

% Pega as datas de uma lista de leituras
pegaDatas([X|XS], Acc, Resultado):-
    append([X.data], Acc, NewAcc),
    pegaDatas(XS, NewAcc, Resultado).
pegaDatas([], Resultado, Resultado):-!.