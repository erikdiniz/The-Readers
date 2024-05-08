:- module(perfil,[criaPerfil/3, salvarPerfil/1, visaoGeral/1, visaoStalker/1, recuperaPerfil/2, procuraPerfilPorId/3]).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module("../Util/util.pl").
:- use_module("../Controller/Usuario.pl").

% Cria um novo perfil
criaPerfil(NomePerfil, Biografia, NomeUsuario):-
    Perfil = _{nomePerfil: NomePerfil, biografia: Biografia, id: NomeUsuario},
    salvarPerfil(Perfil).

% Salva perfil no arquivo json
salvarPerfil(Perfil):-
    lerJSON('../Data/perfis.json', Perfis),
    append([Perfil], Perfis, PerfisAtualizados),
    escreveJSON('../Data/perfis.json', PerfisAtualizados).

visaoGeral(NomeUsuario):-
    recuperaPerfil(NomeUsuario, Perfil),
    writeln("MEU NOME... "),
    writeln(Perfil.nomePerfil),
    writeln("SOBRE MIM... "),
    writeln(Perfil.biografia).

visaoStalker(PerfilVisitado):-
    recuperaPerfil(PerfilVisitado, Perfil),
    writeln("NOME: "),
    writeln(Perfil.nomePerfil),
    writeln("SOBRE: "),
    writeln(Perfil.biografia).

% Recupera perfil pelo id
recuperaPerfil(Id, Perfil):-
    atom_string(Id, StrId),
    lerJSON('../Data/perfis.json', IPerfis),
    procuraPerfilPorId(StrId, IPerfis, Perfil).

% Procura recursivamente por um perfil a partir do id
procuraPerfilPorId(_, [], []):- !.
procuraPerfilPorId(Id, [Perfil|__], Perfil):- Perfil.id == Id, !.
procuraPerfilPorId(Id, [_|XS], Perfil):- procuraPerfilPorId(Id, XS, Perfil).


