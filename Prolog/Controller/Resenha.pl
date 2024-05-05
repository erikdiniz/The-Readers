:- module(resenha,[criaResenha/3, minhasResenhas/1, cadastrarResenha/1, feed/1]).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module("../Util/util.pl").
:- use_module("../Controller/Livro.pl").
:- use_module("../Controller/Leitura.pl").
:- use_module("../Controller/Usuario.pl").
:- use_module("../Menu/MenuLogado.pl").

feed(Usuario):-
    SeguindoNomes = Usuario.seguindo,
    pegaResenhaDosSeguindo(SeguindoNomes, [], Resultado),
    iniciarFeed(Usuario, Resultado).

iniciarFeed(Usuario, []):- clear, writeln("Fim do Feed."), menuLogado(Usuario).
iniciarFeed(Usuario, [Atual|Resto]):-
    clear,
    imprimeResenhasFeed(Atual),
    imprimeOpcoesFeed(),
    nl,
    read_line_to_string(user_input, Opcao),
    (Opcao == "L" -> adicionaCurtidaResenha(Atual, Att), iniciarFeed(Usuario, [Att|Resto]);
    Opcao == "S" -> clear, menuLogado(Usuario);
    Opcao == "P" -> iniciarFeed(Usuario, Resto);
    Opcao == "C" -> adicionaComentario(Usuario, Atual, Att), iniciarFeed(Usuario, [Att|Resto])).

imprimeOpcoesFeed():-
    format("[P] Próximo post | [L] Curtir | [C] Comentar | [S] Sair", []).
imprimeResenhasFeed(Resenha):-
    writeln("---------------------------------"),
    format('Resenha de ~w~n', [Resenha.usuarioId]),
    format('~w escrito por ~w~n',[Resenha.titulo, Resenha.autor]),
    writeln("---------------------------------"),
    format('Resenha: ~w~n', [Resenha.resenha]),
    writeln("---------------------------------"),
    number_string(Resenha.curtidas, SCurtidas),
    format('~w curtidas ~n', [SCurtidas]),
    writeln("Comentários:"),
    imprimeComentarios(Resenha.comentarios),
    writeln("---------------------------------").
 
pegaResenhaDosSeguindo([Id|Resto], Acc, Resultado):-
    recuperaUsuario(Id, UsuarioId),
    recuperaResenhasUsuario(UsuarioId, Resenhas),
    append(Resenhas, Acc, NewAcc),
    pegaResenhaDosSeguindo(Resto, NewAcc, Resultado).
pegaResenhaDosSeguindo([], Resultado, Resultado).

%Interação com o usuário para cadastrar a resenha
cadastrarResenha(Usuario):-
    clear,
    recuperaTitulosLidos(Usuario, Titulos),
    imprimeListaString(Titulos),
    writeln("Qual livro deseja avaliar: "),
    read_line_to_string(user_input, Titulo),
    (member(Titulo, Titulos) -> recupera_livro(Titulo, Livro),
    writeln("O que você achou da leitura: "),
    read_line_to_string(user_input, Resenha),
    criaResenha(Usuario, Livro, Resenha),
    nl, clear,
    writeln("Resenha Cadastrada!"),
    nl,
    menuLogado(Usuario);
    clear,
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
adicionaCurtidaResenha(Resenha, ResenhaAtt) :-
    removeResenha(Resenha),
    CurtidasAtt is Resenha.curtidas + 1,
    put_dict(curtidas, Resenha, CurtidasAtt, ResenhaAtt),
    lerJSON('../Data/resenhas.json', Resenhas),
    append([ResenhaAtt], Resenhas, ResenhasAtt),
    escreveJSON('../Data/resenhas.json', ResenhasAtt).

% Predicado para atualizar o arquivo JSON com as resenhas atualizadas
atualiza_resenha_json(ResenhasJSON, ResenhaAtualizada) :-
    retractall(resenha(_, _, _, _, _, _)), % Remove todas as resenhas existentes (evita duplicatas)
    assertz(resenha(ResenhaAtualizada)), % Adiciona a resenha atualizada
    open('../Data/resenhas.json', write, Stream),
    json_write(Stream, ResenhasJSON),
    close(Stream).

adicionaComentario(Usuario, Resenha, ResenhaAtt):-
    nl,
    writeln("Insira seu comentário: "),
    read_line_to_string(user_input, Comentario),
    Componente = _{comentario: Comentario, usuario: Usuario.nome},
    removeResenha(Resenha),
    append(Resenha.comentarios, [Componente], ComentarioAtt),
    put_dict(comentarios, Resenha, ComentarioAtt, ResenhaAtt),
    lerJSON('../Data/resenhas.json', Resenhas),
    append([ResenhaAtt], Resenhas, ResenhasAtt),
    escreveJSON('../Data/resenhas.json', ResenhasAtt).


removeResenha(Resenha):-
    lerJSON('../Data/resenhas.json', Resenhas),
    remove_element(Resenha, Resenhas, [],ResenhasSemRemocao),
    escreveJSON('../Data/resenhas.json', ResenhasSemRemocao).

remove_element(Resenha, [Head|Tail], Acc,Resultado):-
    ((Resenha.resenha == Head.resenha, Resenha.usuarioId == Head.usuarioId) -> remove_element(Resenha, Tail, Acc, Resultado);
                                                          append([Head], Acc, NewAcc), remove_element(Resenha, Tail, NewAcc, Resultado)).
remove_element(_, [], Resultado,Resultado):-!.

minhasResenhas(Usuario):-
    recuperaResenhasUsuario(Usuario, ResenhasUsuario),
    (ResenhasUsuario == [] -> writeln("Você ainda não possui resenhas");
    nl,
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
    format('Resenha de ~w~n', [Resenha.usuarioId]),
    format('~w escrito por ~w~n',[Resenha.titulo, Resenha.autor]),
    writeln("---------------------------------"),
    format('Resenha: ~w~n', [Resenha.resenha]),
    writeln("---------------------------------"),
    number_string(Resenha.curtidas, SCurtidas),
    format('~w curtidas ~n', [SCurtidas]),
    writeln("Comentários:"),
    imprimeComentarios(Resenha.comentarios),
    writeln("---------------------------------"),
    nl,
    imprimeResenhas(ResenhasRestantes).

imprimeComentarios([]):-!.
imprimeComentarios([Comentario|ComentariosRestantes]):-
    format("~w: ~w~n", [Comentario.usuario, Comentario.comentario]),
    imprimeComentarios(ComentariosRestantes).

    

    