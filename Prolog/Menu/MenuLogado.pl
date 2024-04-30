:- module(menulogado, [menuLogado/1]).
:- use_module("../Controller/Usuario.pl").
:- use_module("../Controller/Leitura.pl").
:- use_module("../Controller/Estante.pl").

menuLogado(Usuario):-
    nl,
    writeln("Bem vindo ao The Readers"),
    imprimeOpcoes(Opcao),
    selecionaAcao(Opcao, Usuario).

seguirUsuario(Usuario):- tty_clear, seguir(Usuario, UsuarioAtt), menuLogado(UsuarioAtt).

imprimeOpcoes(Opcao):-
    nl,
    writeln("[U] Seguir Usuário"),
    writeln("[M] Minhas Estantes"),
    writeln("[L] Cadastrar Leitura"),
    writeln("[S] Sair"),
    read_line_to_string(user_input, Opcao).    

selecionaAcao(Opcao, Usuario):- (
                        Opcao == "S" -> nl, writeln("Obrigado por acessar o The Readers"), halt;
                        Opcao == "U" -> seguirUsuario(Usuario);
                        Opcao == "L" -> cadastrarLeitura(Usuario);
                        Opcao == "M" -> menuEstante(Usuario);
                        writeln("Ação inválida"), menu, !).
