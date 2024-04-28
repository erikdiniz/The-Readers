:- use_module(library(http/json)).
:- use_module("../Util/util.pl").
menu:-
    writeln("Nome de login: "),
    read(Nome),
    writeln("Insira sua senha: "),
    read(Senha),
    criaUsuario(Nome, Senha),
    write("Cadastro Realizado!").

criaUsuario(Nome, Senha):-
    Usuario = [Nome, [], [], Senha],
    lerJSON('../Data/usuarios.json', Usuarios),
    append(Usuarios, [Usuario], UsuariosAtualizados),
    escreveJSON('../Data/usuarios.json', UsuariosAtualizados).
