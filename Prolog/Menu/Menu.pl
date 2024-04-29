:- use_module(library(http/json)).
:- use_module("../Util/util.pl").
:- use_module("../Controller/Usuario.pl").
:- use_module("../Menu/MenuLogado.pl").

menu:-
    writeln("Bem vindo"), nl,
    writeln("Escolher opção:"),
    imprimeOpcoes(Opcao),
    selecionaAcao(Opcao).

imprimeOpcoes(Opcao):-
    writeln("[C] Cadastro de Usuario"),
    writeln("[L] Login de Usuario"),
    writeln("[S] Sair"),
    read_line_to_string(user_input,Opcao).

selecionaAcao(Opcao):- (Opcao == "C" -> cadastraUsuario, menu, !;
                        Opcao == "L" -> loginUsuario,!;
                        Opcao == "S" -> nl, writeln("Obrigado por acessar o The Readers"), halt;
                        writeln("Ação inválida"), menu, !).

loginUsuario:-
    writeln("Nome de login: "),
    read_line_to_string(user_input,Nome),
    recuperaUsuario(Nome, Usuario),
    (Usuario == [] -> nl, writeln("Login Inválido"), nl, menu; verificaSenha(Usuario)).

%colocar chamada pra menu
verificaSenha(Usuario):-
    writeln("Insira sua senha: "),
    read_line_to_string(user_input,Senha),
    (Senha == Usuario.senha -> nl, writeln("Você está logado"), menuLogado(Usuario); nl, writeln("Senha inválida"), menu).
   
cadastraUsuario:-
    writeln("Nome de login: "),
    read_line_to_string(user_input,Nome),
    writeln("Insira sua senha: "),
    read_line_to_string(user_input,Senha),
    criaUsuario(Nome, Senha),
    writeln("Cadastro Realizado!").

procuraUsuario:-
    read_line_to_string(user_input,Nome),
    recuperaUsuario(Nome, Usuario),
    writeln(Usuario).

