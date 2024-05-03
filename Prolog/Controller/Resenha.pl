:- module(resenha,[]).
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

%Escreve a resenha no Json
adicionaResenha(Resenha):-
    lerJSON('../Data/resenhas.json', Resenhas),
    append([Resenha], Resenhas, ResenhasAtulizadas),
    escreveJSON('../Data/resenhas.json', ResenhasAtulizadas).

    