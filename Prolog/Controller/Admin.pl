:- module(admin, [cadastraAdmin/2, recuperaAdm/2, procuraAdm/3, removeAdm/1]).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module("../Util/util.pl").

% Cadastra outro Adm na plataforma e salva no arquivo JSON
cadastraAdmin(Id, Senha) :-
    Admin = _{idAdm: Id, senha: Senha},
    salvaAdmin(Admin).

salvaAdmin(Admin) :-
    lerJSON('../Data/admins.json', Admins),
    append([Admin], Admins, AdminsAtualizados),
    escreveJSON('../Data/admins.json', AdminsAtualizados).

% Recupera o Adm no arquivo JSON a partir de seu ID
recuperaAdm(Id, Admin) :-
    atom_string(Id, StrId),
    lerJSON('../Data/admins.json', Admins),
    procuraAdm(StrId, Admins, Admin).

procuraAdm(Id, [Admin | _], Admin) :-
    Admin.idAdm == Id, !.
procuraAdm(Id, [_ | Rest], Admin) :-
    procuraAdm(Id, Rest, Admin).
procuraAdm(_, [], fail).

% Apaga o Adm do arquivo JSON a partir de seu ID
removeAdm(Id) :-
    lerJSON('../Data/admins.json', Admins),
    filter(tem_id(Id), Admins, AdminsRestantes),
    escreveJSON('../Data/admins.json', AdminsRestantes).

tem_id(Id, Admin) :-
    Admin.idAdm == Id.
