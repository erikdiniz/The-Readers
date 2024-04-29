:- module(menulogado, [menuLogado/1]).
:- use_module("../Controller/Usuario.pl").

menuLogado(Usuario):-
    writeln("Bem vindo ao The Readers"),
    imprimeOpcoes(Opcao),
    selecionaAcao(Opcao, Usuario).

seguirUsuario(Usuario):- seguir(Usuario, UsuarioAtt), menuLogado(UsuarioAtt).

imprimeOpcoes(Opcao):-
    writeln("[U] Seguir Usuário"),
    writeln("[S] Sair"),
    read_line_to_string(user_input, Opcao).    

selecionaAcao(Opcao, Usuario):- (
                        Opcao == "S" -> nl, writeln("Obrigado por acessar o The Readers"), halt;
                        Opcao == "U" -> seguirUsuario(Usuario);
                        writeln("Ação inválida"), menu, !).
