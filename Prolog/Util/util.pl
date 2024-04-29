:- module(util,[lerJSON/2, escreveJSON/2, notMember/4, imprimeListaString/1]).
:- use_module(library(http/json)).

% Regra genérica para ler um arquivo json e retornar ele em Lista
lerJSON(Path, Lista):-
    open(Path, read, Stream, [encoding(utf8)]),
    json_read_dict(Stream, Lista),
    close(Stream).

% Regra genérica para escrever Objetos em um json
escreveJSON(Path, Objetos):-
    open(Path, write, Stream, [encoding(utf8)]),
    json_write(Stream, Objetos),
    close(Stream).

% Filtra uma Lista1 tirando todos os elementos que estejam na Lista 2
% Na primeira chamada, passe uma lista vazia em Acc
notMember([X|XS], Lista2, Acc, Result):-
    (member(X,Lista2) -> notMember(XS, Lista2, Acc, Result); 
    append([X], Acc, NewAcc), notMember(XS, Lista2, NewAcc, Result)).
notMember(_,_,Result, Result):-!.

imprimeListaString([X|XS]):-write("- "), writeln(X), imprimeListaString(XS).
imprimeListaString([]):-!.