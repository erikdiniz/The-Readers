:- module(util,[lerJSON/2, escreveJSON/2]).
:- use_module(library(http/json)).

% Regra genérica para ler um arquivo json e retornar ele em Lista
lerJSON(Path, Lista):-
    open(Path, read, Stream),
    json_read(Stream, Lista),
    close(Stream).

% Regra genérica para escrever Objetos em um json
escreveJSON(Path, Objetos):-
    open(Path, write, Stream),
    json_write(Stream, Objetos),
    close(Stream).