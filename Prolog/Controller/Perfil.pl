:- module(perfil,[criaPerfil/3, salvarPerfil/1, recuperaPerfil/2, visaoGeral/1]).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module("../Util/util.pl").
:- use_module("../Controller/Usuario.pl").

% Cria um novo perfil
criaPerfil(NomePerfil, Biografia, Usuario):-
    Perfil = _{nomePerfil: NomePerfil, biografia: Biografia, id: Usuario},
    salvarPerfil(Perfil).

% Salva perfil no arquivo json
salvarPerfil(Perfil):-
    lerJSON('../Data/perfis.json', Perfis),
    append([Perfil], Perfis, PerfisAtualizados),
    escreveJSON('../Data/perfis.json', PerfisAtualizados).

recuperaPerfil(NomeUsuario, Perfil):-
    lerJSON("../Data/perfis.json", Conteudo),
    parsearJSON(Conteudo, Perfis),
    procuraPerfilPorNomeUsuario(NomeUsuario, Perfis, Perfil).

% Converte o conteúdo JSON em uma lista de perfis Prolog
parsearJSON([], []):- !.
parsearJSON([Head|Tail], [Perfil|Perfis]):-
    parsearObjeto(Head, Perfil),
    parsearJSON(Tail, Perfis).
parsearObjeto([nomePerfil:"Nome", biografia:"Biografia", id:"ID"], perfil(Nome, Biografia, ID)):- !.
parsearObjeto([nomePerfil:"Nome", biografia:"Biografia", id:{nome:"Nome", seguidores: Seguidores, seguindo: Seguindo, senha:"Senha"}], perfil(Nome, Biografia, {nome: "Nome", seguidores: Seguidores, seguindo: Seguindo, senha: "Senha"})):- !.
parsearObjeto([_|Tail], []):-
    parsearJSON(Tail, []).

% Procura o perfil com o nome de usuário especificado
procuraPerfilPorNomeUsuario(NomeUsuario, [Perfil|Perfis], Resultado):-
    perfil(Nome, _, _),
    Nome = NomeUsuario,
    Resultado = Perfil,
    !.
procuraPerfilPorNomeUsuario(NomeUsuario, [_|Perfis], Resultado):-
    procuraPerfilPorNomeUsuario(NomeUsuario, Perfis, Resultado).
procuraPerfilPorNomeUsuario(NomeUsuario, [], fail).


visaoGeral(Usuario):-
    recuperaPerfil(nome(Usuario), Perfil),
    cabecalhoPerfil,
    exibeNome(Perfil),
    exibeBiografia(Perfil).
 %   exibeSeguidores(perfilSeguidores(Perfil)),
 %   exibeSeguindo(perfilSeguindo(Perfil)).

cabecalhoPerfil:-
    writeln("--------------------------------------"),
    writeln("|        MEU PERFIL THE READER        |"),
    writeln("--------------------------------------").
exibeNome(Perfil):-
    writeln("MEU NOME: " ++ nomePerfil(Perfil)),
    writeln().
exibeBiografia(Perfil):-
    writeln("SOBRE MIM... " ++ biografia(Perfil)),
    writeln().
exibeSeguidores(Seguidores):-
    writeln("SOU SEGUIDO POR " ++ show(length(Seguidores))),
    writeln().
exibeSeguindo(Seguindo):-
    writeln("ACOMPANHO " ++ show(length(Seguindo))),
    writeln().



% recupera perfis
% visao stalker
% procura perfil





