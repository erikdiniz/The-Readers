:- module(leitura, [cadastrarLeitura/1]).
:- use_module("../Controller/Livro.pl").
:- use_module("../Util/util.pl").
:- use_module("../Menu/MenuLogado.pl").


% Interação com usuário para cadastrar leitura
cadastrarLeitura(Usuario):-
    lista_livros(Titulos),
    writeln("Títulos disponíveis: "),
    imprimeListaString(Titulos),
    writeln("Cadastrar leitura de: "),
    read_line_to_string(user_input, Titulo),
    (member(Titulo,Titulos) -> coletaAvaliacao(Usuario,Titulo);
                               nl, writeln("Título inválido"), nl, menuLogado(Usuario)). 
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
    nl,
    writeln("Leitura Cadastrada!"),
    nl,
    menuLogado(Usuario);
    nl, writeln("Nota inválida"), nl, menuLogado(Usuario)
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
    (X.id_usuario == Nome -> append([X], Acc, NewAcc), recuperaLeituras(Nome, XS, NewAcc, Resultado);
                             recuperaLeituras(Nome, XS, Acc, Resultado)).
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

