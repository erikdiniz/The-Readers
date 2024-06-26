:- module(livros, [criaLivro/4, livroJaExiste/3, getLivro/3, removeLivro/1, removeLivroPorNome/3, lista_livros/1, recupera_livro/2, imprimeListaLivros/0, listaGeneros/2, listaAutores/2]).
:- use_module(library(http/json)).
:- use_module("../Util/util.pl").

% Cria um novo Livro
criaLivro(Nome, Autor, N_paginas, Genero) :-
    lerJSON('../Data/livros.json', Livros),
    livroJaExiste(Nome, Livros, Resultado),
    Resultado = true -> 
        writeln("Livro já cadastrado no sistema.");
    Livro = _{nome: Nome, autor: Autor, n_paginas: N_paginas, genero: Genero},
    lerJSON('../Data/livros.json', Livros),
    append([Livro], Livros, LivrosAtualizados),
    escreveJSON('../Data/livros.json', LivrosAtualizados),
    writeln("Cadastro realizado com sucesso!").

% Busca um livro a partir de seu Nome.
getLivro(_ ,[],[]) :- !.
getLivro(NomeProcurado, [H|_], H) :- H.nome == NomeProcurado, !. 
getLivro(NomeProcurado, [_|T], Livro) :- getLivro(NomeProcurado, T, Livro).

% Remove um livro a partir de seu Nome.
removeLivro(Nome):-
    lerJSON('../Data/livros.json', Livros),
    livroJaExiste(Nome, Livros, Resultado),
    Resultado = true -> 
        removeLivroPorNome(Livros, Nome, ListaAtualizada),
        escreveJSON('../Data/livros.json', ListaAtualizada),
        writeln("Livro excluido com sucesso.");
    writeln("Livro não existe no sistema.").
removeLivroPorNome(Lista, Nome, Resultado):-
    exclude(tem_nome(Nome), Lista, Resultado).

% Verifica se um livro já existe no sistema.
livroJaExiste(_,[],false) :- !.
livroJaExiste(NomeProcurado, [H|_], true) :- H.nome == NomeProcurado, !.
livroJaExiste(NomeProcurado, [_|T], Resultado) :- livroJaExiste(NomeProcurado, T, Resultado).

% Lista os Livros do sistema
lista_livros(Titulos):-
    lerJSON('../Data/livros.json', Livros),
    lista_livros(Livros, [], Titulos).
lista_livros([X|XS], Acc, Resultado):-
    append([X.nome], Acc, NewAcc),
    lista_livros(XS, NewAcc, Resultado).
lista_livros([], Resultado, Resultado):-!.

% Imprime o nome de todos os livros cadastrados no sistema.
imprimeListaLivros() :-
    lista_livros(Titulos),
    imprimeLista(Titulos).
imprimeLista([H|T]) :- writeln(H), imprimeLista(T).
imprimeLista([]) :- writeln("").

% Recupera um livro pelo nome
recupera_livro(Titulo, Livro):-
    lerJSON('../Data/livros.json', Livros),
    getLivro(Titulo, Livros, Livro).

% Método auxiliar usado para verificar se Livro tem chave.
tem_nome(Nome, Livro):- Nome == Livro.nome .

% Retorna os gêneros de uma lista de livros (permite repetição)
listaGeneros([], []).
listaGeneros([Livro], [Livro.genero]).
listaGeneros([Livro1|Resto], [Livro1.genero|RestoGeneros]):-
    listaGeneros(Resto, RestoGeneros).

% Retorna os autores de uma lista de livros (permite repetição)
listaAutores([], []).
listaAutores([Livro], [Livro.autor]).
listaAutores([Livro1|Resto], [Livro1.autor|RestoAutores]):-
    listaAutores(Resto, RestoAutores).
