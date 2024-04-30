:- module(menulogado, [menuLogado/1, menuPerfil/1]).
:- use_module("../Controller/Usuario.pl").
:- use_module("../Controller/Leitura.pl").
:- use_module("../Controller/Estante.pl").
:- use_module("../Controller/Perfil.pl").

menuLogado(Usuario):-
    nl,
    writeln("Bem vindo ao The Readers"),
    imprimeOpcoes(Opcao),
    selecionaAcao(Opcao, Usuario).

seguirUsuario(Usuario):- seguir(Usuario, UsuarioAtt), menuLogado(UsuarioAtt).

imprimeOpcoes(Opcao):-
    nl,
    writeln("[P] Menu Perfil"),
    writeln("[B] Buscar Usuário"),
    writeln("[U] Seguir Usuário"),
    writeln("[M] Minhas Estantes"),
    writeln("[+] Cadastro de Livro"),
    writeln("[-] Remover um Livro"),
    writeln("[L] Cadastrar Leitura"),
    writeln("[S] Sair"),
    read_line_to_string(user_input, Opcao).    

selecionaAcao(Opcao, Usuario):- (
                        Opcao == "S" -> nl, writeln("Obrigado por acessar o The Readers"), halt;
                        Opcao == "U" -> seguirUsuario(Usuario);
                        Opcao == "L" -> cadastrarLeitura(Usuario);
                        Opcao == "+" -> cadastraLivro, menu, !;
                        Opcao == "-" -> removeLivro, menu, !;
                        Opcao == "M" -> menuEstante(Usuario);
                        Opcao == "P" -> menuPerfil(Usuario);
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

removeLivro:-
    writeln("Nome do livro a ser excluido: "),
    read_line_to_string(user_input, Nome),
    removeLivro(Nome),
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
    writeln("[M] Minhas Resenhas"),
    writeln("[S] Voltar ao menu principal"),
    read_line_to_string(user_input, Opcao).

selecionaAcaoPerfil(Opcao, Usuario):- (
                        Opcao == "S" -> menuLogado(Usuario);
                        Opcao == "V" -> visaoGeral(Usuario);
                        Opcao == "E" -> editaPerfil(Usuario);
                        Opcao == "M" -> menuEstante(Usuario);
                        writeln("Ação inválida"), menu, !).

editaPerfil(Usuario):- (
    nl,
    writeln("Escolha seu nome: "),
    read_line_to_string(user_input,NomePerfil),
    writeln("Escolha sua biografia: "),
    read_line_to_string(user_input,Biografia),
    criaPerfil(NomePerfil, Biografia, Usuario),
    writeln("Perfil salvo com sucesso!"), menuPerfil(Usuario), !).
