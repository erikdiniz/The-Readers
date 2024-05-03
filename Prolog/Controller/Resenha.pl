:- module(resenha,[cadastrarResenha/1, adicionarComentario/2, minhasResenhas/1]).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module("../Util/util.pl").
:- use_module("../Controller/Livro.pl").
:- use_module("../Controller/Leitura.pl").
:- use_module("../Controller/Usuario.pl").
:- use_module("../Menu/MenuLogado.pl").

%Interação com o usuário para cadastrar a resenha
cadastrarResenha(NomeUsuario):-
    writeln("Qual livro deseja avaliar? "),
    read_line_to_string(user_input, TituloLivro),
    recupera_livro(TituloLivro, Livro),
    writeln("O que você achou da leitura? "),
    read_line_to_string(user_input, Texto),
    criaResenha(NomeUsuario, Texto, TituloLivro),
    writeln("Resenha salva!").


% Cria uma resenha para um livro lido
criaResenha(NomeUsuario, Texto, TituloLivro):-
    Perfil = _{usuarioId: NomeUsuario, texto: Texto, livro: TituloLivro},
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

minhasResenhas(NomeUsuario):-
    recuperaResenhasUsuario(NomeUsuario, ResenhasUsuario),
    imprimeResenhas(ResenhasUsuario).

recuperaResenhasUsuario(NomeUsuario, ResenhasUsuario):-
    lerJSON('../Data/resenhas.json', Resenhas),
    procuraResenhasPorUsuario(NomeUsuario, Resenhas, ResenhasUsuario).

procuraResenhasPorUsuario(_, [], []):- !.
procuraResenhasPorUsuario(NomeUsuario, [Resenha|ResenhasRestantes], ResenhasUsuario):-
    (Resenha.usuarioId == NomeUsuario) ->
        append([Resenha], ResenhasUsuario, ResenhasUsuarioTemp),
        procuraResenhasPorUsuario(Usuario, ResenhasRestantes, ResenhasUsuarioTemp), !;
    procuraResenhasPorUsuario(NomeUsuario, ResenhasRestantes, ResenhasUsuario).

imprimeResenhas([]):-
    writeln("Você ainda não possui resenhas!").
imprimeResenhas([Resenha|ResenhasRestantes]):-
    writeln("---------------------------------"),
    writeln(Resenha.titulo),
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

