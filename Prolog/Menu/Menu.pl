:- use_module(library(http/json)).
:- use_module("../Util/util.pl").
:- use_module("../Controller/Usuario.pl").
:- use_module("../Controller/Admin.pl").
:- use_module("../Controller/Livro.pl").
:- use_module("../Menu/MenuLogado.pl").

menu:-
    writeln("Bem vindo"), nl,
    writeln("Escolher opção:"),
    imprimeOpcoes(Opcao),
    selecionaAcao(Opcao).

imprimeOpcoes(Opcao):-
    writeln("[C] Cadastro de Usuario"),
    writeln("[L] Login de Usuario"),
    writeln("[D] Dashboard Administrador"),
    writeln("[S] Sair"),
    nl,
    read_line_to_string(user_input,Opcao).

selecionaAcao(Opcao):- (Opcao == "C" -> cadastraUsuario, menu, !;
                        Opcao == "L" -> loginUsuario,!;
                        Opcao == "D" -> menuAdm,!;
                        Opcao == "S" -> nl, writeln("Obrigado por acessar o The Readers"), nl, halt;
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
    listaUsuarios(NomesUtilizados),
    (member(Nome, NomesUtilizados) -> nl ,writeln("Esse nome de usuário já está sendo utilizado"),nl, menu;
                                    writeln("Insira sua senha: "),
                                    read_line_to_string(user_input,Senha),
                                    criaUsuario(Nome, Senha),
                                    writeln("Cadastro Realizado!")
                                    ).
    

procuraUsuario:-
    read_line_to_string(user_input,Nome),
    recuperaUsuario(Nome, Usuario),
    writeln(Usuario).


menuAdm:-
    writeln("Bem vindo"), nl,
    writeln("Escolher opção:"),
    imprimeOpcoesAdm(Opcao),
    selecionaAcaoAdm(Opcao).

imprimeOpcoesAdm(Opcao):-
    writeln("[L] Realizar login"),
    writeln("[V] Voltar ao menu de login"),
    writeln("[S] Sair"),
    nl,
    read_line_to_string(user_input,Opcao).

selecionaAcaoAdm(Opcao):- (Opcao == "L" -> loginAdm,!;
                        Opcao == "V" -> menu, !;
                        Opcao == "S" -> nl, writeln("Obrigado por acessar o The Readers"), nl, halt;
                        writeln("Ação inválida"), menu, !).

loginAdm:-
    writeln("Insira seu ID: "),
    read_line_to_string(user_input,Id),
    recuperaAdm(Id, Admin),
    (Admin == [] -> nl, writeln("Login Inválido"), nl, menuAdm; verificaSenhaAdm(Admin)).

%colocar chamada pra menu
verificaSenhaAdm(Admin):-
    writeln("Insira sua senha: "),
    read_line_to_string(user_input,Senha),
    (Senha == Admin.senha -> nl, writeln("Você está logado"), menuLogadoAdm(Admin); nl, writeln("Senha inválida"), menuAdm).

menuLogadoAdm(Admin):-
    writeln("Dashboard de Administrador"), nl,
    writeln("Escolher opção:"),
    dashAdm(Opcao, Admin),
    selecionaAdm(Opcao).

dashAdm(Opcao, Admin):-
    writeln("[1] Cadastro novo Adm"),
    writeln("[2] Estatísticas Gerais"),
    writeln("[3] Lista de usuários cadastrados"),
    writeln("[-] Excluir livro"),
    writeln("[S] Sair"),
    nl,
    read_line_to_string(user_input,Opcao).

selecionaAdm(Opcao):- (Opcao == "1" -> cadastraAdm, menuLogadoAdm(Admin), !;
                        Opcao == "2" -> menuLogadoAdm(Admin),!;
                        Opcao == "3" -> imprimeUsuarios, menuLogadoAdm(Admin),!;
                        Opcao == "-" -> removeLivro, menuLogadoAdm(Admin), !;
                        Opcao == "S" -> nl, writeln("Obrigado por acessar o The Readers"), nl, halt;
                        writeln("Ação inválida"), menu, !).


cadastraAdm:-
    writeln("Novo Id Administrador: "),
    read_line_to_string(user_input,Id),
    writeln("Senha: "),
    read_line_to_string(user_input,Senha),
    cadastraAdmin(Id, Senha),
    writeln("Novo adm cadastrado com sucesso!").

removeLivro:-
    writeln("Nome do livro a ser excluido: "),
    read_line_to_string(user_input, Nome),
    removeLivro(Nome),
    writeln("").