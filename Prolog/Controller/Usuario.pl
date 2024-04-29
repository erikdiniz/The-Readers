:- module(usuario,[criaUsuario/2, recuperaUsuario/2, procuraUsuario/3, imprimeUsuarios/0, imprimeLista/1, removeUsuario/1]).
:- use_module(library(http/json)).
:- use_module(library(lists)).

% Cria um novo usuário
criaUsuario(Nome, Senha):-
    Usuario = _{nome: Nome, seguidores: [], seguindo: [], senha: Senha},
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

removeUsuario(Usuario):-
     lerJSON('../Data/usuarios.json', Usuarios), 
     remover_por_nome(Usuarios, Usuario.nome, Att),
     escreveJSON('../Data/usuarios.json', Att).

remover_por_nome(Lista, Nome, Resultado) :-
    exclude(tem_nome(Nome), Lista, Resultado).

tem_nome(Nome, dict(nome:Nome, _)).

% Imprime o nome de todos os usuários
imprimeUsuarios:-
    lerJSON('../Data/usuarios.json', IUsuarios),
    imprimeLista(IUsuarios).

% Imprime uma lista com nomes de usuários
imprimeLista([X|XS]):- writeln(X.nome), imprimeLista(XS).
imprimeLista([]):- writeln("fim").
