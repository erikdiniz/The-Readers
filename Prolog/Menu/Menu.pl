:- use_module(library(http/json)).
:- use_module("../Util/util.pl").
:- use_module("../Controller/Usuario.pl").

menu:-
    writeln("Bem vindo"), nl,
    writeln("Escolher opção:"),
    imprimeOpcoes(Opcao),
    selecionaAcao(Opcao).

imprimeOpcoes(Opcao):-
    writeln("[C] Cadastro de Usuario"),
    read(Opcao).

selecionaAcao('C'):- cadastraUsuario, menu, !.
selecionaAcao(_):- writeln("Ação inválida"), menu, !.

cadastraUsuario:-
    writeln("Nome de login: "),
    read(Nome),
    writeln("Insira sua senha: "),
    read(Senha),
    criaUsuario(Nome, Senha),
    writeln("Cadastro Realizado!").

procuraUsuario:-
    read(N),
    atom_string(N, Nome),
    recuperaUsuario(Nome, Usuario),
    writeln(Usuario).

