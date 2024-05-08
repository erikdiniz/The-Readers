:- module(estante, [menuEstante/1]).

:- use_module("../Menu/MenuLogado.pl").
:- use_module("../Controller/Leitura.pl").
:- use_module("../Util/util.pl").

menuEstante(Usuario):-
    imprimeOpcoes(Opcao),
    selecionaAcao(Opcao, Usuario).

imprimeOpcoes(Opcao):-
    writeln("Estantes"),
    nl,
    writeln("[L] Livros lidos"),
    writeln("[V] Voltar"),
    read_line_to_string(user_input, Opcao). 

selecionaAcao(Opcao, Usuario):- (
                        Opcao == "V" -> menuLogado(Usuario);
                        Opcao == "L" -> imprimeLidos(Usuario);
                        writeln("Ação inválida"), menuEstante(Usuario), !).

imprimeLidos(Usuario):-
    writeln("Leituras do usuário:"),
    nl,
    recuperaTitulosLidos(Usuario, Titulos),
    imprimeListaString(Titulos),
    nl,
    writeln("[V] Voltar"),
    read_line_to_string(user_input, _),
    menuLogado(Usuario).
