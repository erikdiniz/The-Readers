:- module(estante, [menuEstante/1]).

:- use_module("../Menu/MenuLogado.pl").
:- use_module("../Controller/Leitura.pl").
:- use_module("../Util/util.pl").

menuEstante(Usuario):-
    imprimeOpcoes(Opcao),
    selecionaAcao(Opcao, Usuario).

imprimeOpcoes(Opcao):-
    clear,
    writeln("Estantes"),
    nl,
    writeln("[L] Livros lidos"),
    writeln("[V] Voltar"),
    read_line_to_string(user_input, Opcao). 

selecionaAcao(Opcao, Usuario):- (
                        Opcao == "V" -> clear, menuLogado(Usuario);
                        Opcao == "L" -> imprimeLidos(Usuario);
                        writeln("Ação inválida"), menuEstante(Usuario), !).

imprimeLidos(Usuario):-
    clear,
    writeln("Leituras do usuário:"),
    nl,
    recuperaTitulosLidos(Usuario, Titulos),
    imprimeListaString(Titulos),
    nl,
    writeln("[V] Voltar"),
    read_line_to_string(user_input, _),
    clear,
    menuLogado(Usuario).
