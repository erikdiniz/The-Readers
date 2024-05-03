:- module(resenha,[criaResenha/3, adicionarComentario/2, minhasResenhas/1]).
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
    recupera_livro(Titulo, Livro),
    writeln("O que você achou da leitura: "),
    read_line_to_string(user_input, Resenha),
    criaResenha(Usuario, Livro, Resenha),
    nl, tty_clear,
    writeln("Resenha Cadastrada!"),
    nl,
    menuLogado(Usuario).
    

% Cria uma resenha para um livro lido
criaResenha(Usuario, Livro, Resenha):-
    Resenha = _{usuarioId: Usuario.nome, titulo: Livro.nome, autor: Livro.autor, resenha: Resenha, curtidas: 0, comentarios: []},
    adicionaResenha(Resenha).

% Escreve a resenha no Json
adicionaResenha(Resenha):-
    lerJSON('../Data/resenhas.json', Resenhas),
    append([Resenha], Resenhas, ResenhasAtulizadas),
    escreveJSON('../Data/resenhas.json', ResenhasAtulizadas).

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
    imprimeResenhas(ResenhasUsuario).

recuperaResenhasUsuario(Usuario, ResenhasUsuario):-
    lerJSON('../Data/resenhas.json', Resenhas),
    procuraResenhasPorUsuario(Usuario, Resenhas, ResenhasUsuario).

procuraResenhasPorUsuario(_, [], []):- !.
procuraResenhasPorUsuario(Usuario, [Resenha|ResenhasRestantes], ResenhasUsuario):-
    (Resenha.usuarioId == Usuario.nome) ->
        append([Resenha], ResenhasUsuario, ResenhasUsuarioTemp),
        procuraResenhasPorUsuario(Usuario, ResenhasRestantes, ResenhasUsuarioTemp), !;
    procuraResenhasPorUsuario(Usuario, ResenhasRestantes, ResenhasUsuario).

imprimeResenhas([]):-
    writeln("Você ainda não possui resenhas!").
imprimeResenhas([Resenha|ResenhasRestantes]):-
    writeln("---------------------------------"),
    writeln(Resenha.titulo),
    writeln(Resenha.autor),
    writeln("Resenha:"),
    writeln(Resenha.resenha),
    writeln("---------------------------------"),
    writeln("Curtidas:"),
    writeln(integer_to_string(Resenha.curtidas)),
    writeln("Comentários:"),
    imprimeComentarios(Resenha.comentarios),
    writeln("---------------------------------"),
    imprimeResenhas(ResenhasRestantes).

imprimeComentarios([]):-
    writeln("Sem comentários.").

imprimeComentarios([Comentario|ComentariosRestantes]):-
    writeln("Usuário: " ),
    writeln(Comentario.usuario),
    writeln("Comentário: "),
    writeln(Comentario.comentario),
    imprimeComentarios(ComentariosRestantes).

