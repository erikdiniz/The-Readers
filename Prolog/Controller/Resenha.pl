:- module(resenha,[criaResenha/3, adicionarComentario/2, minhasResenhas/1, cadastrarResenha/1]).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module("../Util/util.pl").
:- use_module("../Controller/Livro.pl").
:- use_module("../Controller/Leitura.pl").
:- use_module("../Controller/Usuario.pl").
:- use_module("../Menu/MenuLogado.pl").

%Interação com o usuário para cadastrar a resenha
cadastrarResenha(Usuario):-
    tty_clear,
    recuperaTitulosLidos(Usuario, Titulos),
    imprimeListaString(Titulos),
    writeln("Qual livro deseja avaliar: "),
    read_line_to_string(user_input, Titulo),
    (member(Titulo, Titulos) -> recupera_livro(Titulo, Livro),
    writeln("O que você achou da leitura: "),
    read_line_to_string(user_input, Resenha),
    criaResenha(Usuario, Livro, Resenha),
    nl, tty_clear,
    writeln("Resenha Cadastrada!"),
    nl,
    menuLogado(Usuario);
    tty_clear,
    writeln("Livro não cadastrado"),
    nl,
    menuLogado(Usuario)
    ).
    
    


% Cria uma resenha para um livro lido
criaResenha(Usuario, Livro, ResenhaC):-
    Resenha = _{usuarioId: Usuario.nome, titulo: Livro.nome, autor: Livro.autor, resenha: ResenhaC, curtidas: 0, comentarios: []},
    adicionaResenha(Resenha).

% Escreve a resenha no Json
adicionaResenha(Resenha):-
    lerJSON('../Data/resenhas.json', Resenhas),
    append([Resenha], Resenhas, ResenhasAtulizadas),
    escreveJSON('../Data/resenhas.json', ResenhasAtulizadas).



% Predicado para carregar as resenhas do arquivo JSON
carregar_resenhas(Resenhas) :-
    lerJSON('../Data/resenhas.json', Resenhas).

%ToString Resenha
visaoResenha(Resenha) :-
    writeln("=== Resenha ==="),
    format("Título do livro: ~s~n", [Resenha.titulo]),
    format("Autor do livro: ~s~n", [Resenha.autor]),
    format("Resenha: ~s~n", [Resenha.resenha]),
    format("Curtidas: ~d~n", [Resenha.curtidas]),
    writeln("Comentários:"),
    imprimeListaString(Resenha.comentarios).

% Predicado para imprimir uma lista de strings
imprimeListaString([]).
imprimeListaString([X|Xs]) :-
    writeln(X),
    imprimeListaString(Xs).

%Recupera todas as Resenhas
recuperaResenhas(Usuario):-
    lerJSON('../Data/resenhas.json', Resenhas),
    recupera_resenhas(ResenhasJSON, [], Resenhas).


% Predicado auxiliar para recuperar as resenhas
recupera_resenhas([ResenhaJSON|OutrasResenhasJSON], Acc, Resenhas) :-
    append([ResenhaJSON], Acc, NewAcc),
    recupera_resenhas(OutrasResenhasJSON, NewAcc, Resenhas).
recupera_resenhas([], Resenhas, Resenhas) :- !.

% Predicado para adicionar uma curtida a uma resenha
adicionaCurtidaResenha(TituloLivro, Usuario) :-
    lerJSON('../Data/resenhas.json', Resenhas),
    (buscarResenha(TituloLivro, ResenhasJSON, Resenha) ->
        adiciona_curtida_resenha(Resenha, Usuario, ResenhaAtualizada),
        atualiza_resenha_json(ResenhasJSON, ResenhaAtualizada);
        writeln('Resenha não encontrada.')
    ).

% Predicado para buscar uma resenha pelo título do livro
buscarResenha(_, [], _) :- fail. % Falha se não encontrar a resenha
buscarResenha(TituloLivro, [ResenhaJSON|OutrasResenhasJSON], Resenha) :-
    ResenhaJSON.titulo = TituloLivro,
    Resenha = ResenhaJSON,
    !. % Para a busca após encontrar a resenha

% Predicado para adicionar uma curtida à resenha
adiciona_curtida_resenha(Resenha, Usuario, ResenhaAtualizada) :-
    memberchk(Usuario, Resenha.curtidas),
    writeln('Você já curtiu esta resenha!'),
    ResenhaAtualizada = Resenha; % Não atualiza a resenha se o usuário já curtiu
    append([Usuario], Resenha.curtidas, NovaCurtidas),
    ResenhaAtualizada = Resenha.put(curtidas, NovaCurtidas).

% Predicado para atualizar o arquivo JSON com as resenhas atualizadas
atualiza_resenha_json(ResenhasJSON, ResenhaAtualizada) :-
    retractall(resenha(_, _, _, _, _, _)), % Remove todas as resenhas existentes (evita duplicatas)
    assertz(resenha(ResenhaAtualizada)), % Adiciona a resenha atualizada
    open('../Data/resenhas.json', write, Stream),
    json_write(Stream, ResenhasJSON),
    close(Stream).

adicionarComentario(Usuario, Resenha):-
    writeln("Insira seu comentário: "),
    read_line_to_string(user_input, Comentario),
    Componente = _{comentario: Comentario, usuario: Usuario.nome},
    append(Resenha.comentarios, [Componente], NovoComentario),
    atualizaResenha(Resenha, NovoComentario).

atualizaResenha(Resenha, NovosComentarios):-
    ResenhaAtualizada = Resenha^comentarios := NovosComentarios,
    removeResenha(Resenha), % Remove a resenha antiga do arquivo
    adicionaResenha(ResenhaAtualizada). % Adiciona a resenha atualizada

removeResenha(Resenha):-
    lerJSON('../Data/resenhas.json', Resenhas),
    remove_element(Resenha, Resenhas, ResenhasSemRemocao),
    escreveJSON('../Data/resenhas.json', ResenhasSemRemocao).

minhasResenhas(Usuario):-
    recuperaResenhasUsuario(Usuario, ResenhasUsuario),
    (ResenhasUsuario == [] -> writeln("Você ainda não possui resenhas");
    imprimeResenhas(ResenhasUsuario)
    ).
recuperaResenhasUsuario(Usuario, ResenhasUsuario):-
    lerJSON('../Data/resenhas.json', Resenhas),
    procuraResenhasPorUsuario(Usuario, Resenhas, [],ResenhasUsuario).

procuraResenhasPorUsuario(_, [],Resultado, Resultado):- !.
procuraResenhasPorUsuario(Usuario, [Resenha|ResenhasRestantes], Acc, Resultado):-
    (Usuario.nome == Resenha.usuarioId -> append([Resenha], Acc, NewAcc), procuraResenhasPorUsuario(Usuario, ResenhasRestantes, NewAcc, Resultado);
                                            procuraResenhasPorUsuario(Usuario, ResenhasRestantes, Acc, Resultado)).

recuperaLeiturasUsuario(Nome, [X|XS], Acc, Resultado):-
    (X.id_usuario == Nome -> append([X], Acc, NewAcc), recuperaLeiturasUsuario(Nome, XS, NewAcc, Resultado);
                             recuperaLeiturasUsuario(Nome, XS, Acc, Resultado)).
recuperaLeiturasUsuario(_,[],Resultado,Resultado):-!.

imprimeResenhas([]):-
    writeln("").
imprimeResenhas([Resenha|ResenhasRestantes]):-
    writeln("---------------------------------"),
    writeln(Resenha.titulo),
    writeln(Resenha.autor),
    writeln("Resenha:"),
    writeln(Resenha.resenha),
    writeln("---------------------------------"),
    writeln("Curtidas:"),
    number_string(Resenha.curtidas, SCurtidas),
    writeln(SCurtidas),
    writeln("Comentários:"),
    imprimeComentarios(Resenha.comentarios),
    writeln("---------------------------------"),
    imprimeResenhas(ResenhasRestantes).

imprimeComentarios([]):-
    writeln("").

imprimeComentarios([Comentario|ComentariosRestantes]):-
    writeln("Usuário: " ),
    writeln(Comentario.usuario),
    writeln("Comentário: "),
    writeln(Comentario.comentario),
    imprimeComentarios(ComentariosRestantes).

    

    