:- module(usuario,[criaUsuario/2, recuperaUsuario/2, procuraUsuario/3, imprimeUsuarios/0, imprimeLista/1, removeUsuario/1, seguir/2, listaUsuarios/1]).
:- use_module(library(http/json)).
:- use_module(library(lists)).

% Cria um novo usuário
criaUsuario(Nome, Senha):-
    Usuario = _{nome: Nome, seguidores: [], seguindo: [], senha: Senha},
    adicionaUsuario(Usuario).

adicionaUsuario(Usuario):-
    lerJSON('../Data/usuarios.json', Usuarios),
    append([Usuario], Usuarios, UsuariosAtualizados),
    escreveJSON('../Data/usuarios.json', UsuariosAtualizados).

%Retorna o dicionário do usuário a partir de seu Nome
recuperaUsuario(Nome, Usuario):-
    atom_string(Nome, StrNome),
    lerJSON('../Data/usuarios.json', IUsuarios),
    procuraUsuario(StrNome, IUsuarios, Usuario).

% Procura recursivamente por um usuário a partir do Nome
procuraUsuario(_, [], []):- !. 
procuraUsuario(Nome, [X|__], X):- X.nome == Nome, !.
procuraUsuario(Nome, [_|XS], Usuario):- procuraUsuario(Nome, XS, Usuario).


%Remove um usuario do JSON pelo nome dele
removeUsuario(Usuario):-
     lerJSON('../Data/usuarios.json', Usuarios),
     remover_por_nome(Usuarios, Usuario.nome, Att),
     escreveJSON('../Data/usuarios.json', Att).

remover_por_nome(Lista, Nome, Resultado) :-
    exclude(tem_nome(Nome), Lista, Resultado).

tem_nome(Nome, Usuario):- Nome == Usuario.nome.

seguir(Usuario, UsuarioAtt):-
    writeln("Usuarios Disponíveis: "),
    opcoesSeguir(Usuario, Disponiveis),
    imprimeListaString(Disponiveis),
    writeln("Digite o nome de usuário: "),
    read_line_to_string(user_input, Opcao),
    (member(Opcao, Disponiveis) -> nl, writeln("Usuario seguido com sucesso!"), adicionarSeguidor(Usuario, Opcao, UsuarioAtt), menuLogado(UsuarioAtt);
                                   nl, writeln("Opção Inválida!"), menuLogado(Usuario)). 

opcoesSeguir(Usuario, Disponiveis):-
    listaUsuarios(NomesUsuarios),
    append([Usuario.nome], Usuario.seguindo, NaoDisponiveis),
    notMember(NomesUsuarios, NaoDisponiveis, [], Disponiveis).

adicionarSeguidor(Usuario, Nome, UsuarioAtt):-
    recuperaUsuario(Nome, Seguido),
    append(Usuario.seguindo, [Seguido.nome], SeguindoAtt),
    put_dict(seguindo, Usuario, SeguindoAtt, UsuarioAtt),
    append(Seguido.seguidores, [Usuario.nome], SeguidoresAtt),
    put_dict(seguidores, Seguido, SeguidoresAtt, SeguidoAtt),
    removeUsuario(Usuario),
    removeUsuario(Seguido),
    adicionaUsuario(UsuarioAtt),
    adicionaUsuario(SeguidoAtt).



% Imprime o nome de todos os usuários
imprimeUsuarios:-
    lerJSON('../Data/usuarios.json', IUsuarios),
    imprimeLista(IUsuarios).


% Imprime uma lista com nomes de usuários
imprimeLista([X|XS]):- writeln(X.nome), imprimeLista(XS).
imprimeLista([]):- writeln("fim").


listaUsuarios(NomesUsuarios):-
    lerJSON('../Data/usuarios.json', Usuarios),
    montaNomes(Usuarios, [], NomesUsuarios).

montaNomes([X|XS], Acc, Result):- 
    append([X.nome], Acc, NewAcc), 
    montaNomes(XS, NewAcc, Result).

montaNomes([], Result, Result):-!.