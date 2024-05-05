:- module(menulogado, [menuLogado/1, menuPerfil/1, stalkear/1, editaPerfil/2 ]).
:- use_module("../Controller/Usuario.pl").
:- use_module("../Controller/Leitura.pl").
:- use_module("../Controller/Estante.pl").
:- use_module("../Controller/Perfil.pl").
:- use_module("../Controller/Estatisticas.pl").
:- use_module("../Controller/Resenha.pl").

menuLogado(Usuario):-
    nl,
    writeln("--------------------------------------"),
    writeln("|       Bem vindo ao The Readers     |"),
    writeln("--------------------------------------"),
    imprimeOpcoes(Opcao),
    selecionaAcao(Opcao, Usuario).

seguirUsuario(Usuario):- tty_clear, seguir(Usuario, UsuarioAtt), menuLogado(UsuarioAtt).

imprimeOpcoes(Opcao):-
    nl,
    nl, writeln("------------ M E N U : ------------"), nl,
    writeln("[F] Ir para o Feed"),
    writeln("[P] Meu Perfil"),
    writeln("[U] Seguir Usuário"),
    writeln("[M] Minhas Estantes"),
    writeln("[+] Cadastro de Livro"),
    writeln("[L] Cadastrar Leitura"),
    writeln("[R] Cadastrar Resenha"),
    writeln("[E] Estatisticas"),
    writeln("[S] Sair"),
    read_line_to_string(user_input, Opcao).

selecionaAcao(Opcao, Usuario):- (
                        Opcao == "S" -> nl, writeln("Obrigado por acessar o The Readers"), halt;
                        Opcao == "U" -> seguirUsuario(Usuario);
                        Opcao == "L" -> cadastrarLeitura(Usuario);
                        Opcao == "+" -> cadastraLivro, menuLogado(Usuario), !;
                        Opcao == "M" -> menuEstante(Usuario);
                        Opcao == "E" -> menuEstatisticas(Usuario);
                        Opcao == "R" -> cadastrarResenha(Usuario);
                        Opcao == "P" -> menuPerfil(Usuario);
                        Opcao == "F" -> feed(Usuario);
                        writeln("Ação inválida"), menu, !).


cadastraLivro:-
    writeln("Nome do livro: "),
    read_line_to_string(user_input, Nome),
    writeln("Autor: "),
    read_line_to_string(user_input, Autor),
    writeln("Número de páginas: "),
    read_line_to_string(user_input, Paginas_String),
    atom_number(Paginas_String, N_Paginas),
    writeln("Gênero: "),
    read_line_to_string(user_input, Genero),
    criaLivro(Nome, Autor, N_Paginas, Genero),
    writeln("").


menuPerfil(Usuario):-
    nl,
    writeln("Escolher opção:"),
    imprimeOpcoesPerfil(Opcao),
    selecionaAcaoPerfil(Opcao, Usuario).

imprimeOpcoesPerfil(Opcao):-
    nl,
    writeln("[V] Visão geral"),
    writeln("[E] Editar meu perfil"),
    writeln("[O] Visitar outro perfil"),
    writeln("[R] Minhas Resenhas"),
    writeln("[S] Voltar ao menu principal"),
    read_line_to_string(user_input, Opcao).

selecionaAcaoPerfil(Opcao, Usuario):- (
                        Opcao == "S" -> menuLogado(Usuario);
                        Opcao == "V" -> nl,
                                    writeln("--------------------------------------"),
                                    writeln("|        MEU PERFIL THE READER        |"),
                                    writeln("--------------------------------------"),
                                    visaoGeral(Usuario.nome), menuPerfil(Usuario), !;
                        Opcao == "E" -> editaPerfil(Usuario, Usuario.nome);
                        Opcao == "O" -> stalkear(Usuario);
                        Opcao == "R" -> minhasResenhas(Usuario), menuPerfil(Usuario), !;
                        writeln("Ação inválida"), menu, !).

editaPerfil(Usuario, NomeUsuario):- (
    nl,
    writeln("Escolha seu nome: "),
    read_line_to_string(user_input,NomePerfil),
    writeln("Escolha sua biografia: "),
    read_line_to_string(user_input,Biografia),
    criaPerfil(NomePerfil, Biografia, NomeUsuario),
    writeln("Perfil salvo com sucesso!"), menuPerfil(Usuario), !).


stalkear(Usuario):- (
    nl,
    writeln("Insira o ID do perfil à ser visitado: "),
    read_line_to_string(user_input, PerfilVisitado),
    writeln("--------------------------------------"),
    writeln("|        CONHEÇA ESSE READER :)      |"),
    writeln("--------------------------------------"),
    visaoStalker(PerfilVisitado), menuPerfil(Usuario), !).