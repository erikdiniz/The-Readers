:- module(livros, [criaLivro/4, livroJaExiste/3, getLivro/3, removeLivro/1, removeLivroPorNome/3]).
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

% Remove um livro a partir de seu Nome
removeLivro(Nome):-
    lerJSON('../Data/livros.json', Livros),
    livroJaExiste(Nome, Livros, Resultado),
    Resultado = true -> 
        removeLivroPorNome(Livros, Nome, ListaAtualizada),
        escreveJSON('../Data/livros.json', ListaAtualizada),
        writeln("Livro excluido com sucesso.");
    writeln("Livro não existe no sistema.").

% Método auxiliar usado para remover um livro
removeLivroPorNome(Lista, Nome, Resultado):-
    exclude(tem_nome(Nome), Lista, Resultado).

% Verifica se um livro já existe no sistema.
livroJaExiste(_,[],false) :- !.
livroJaExiste(NomeProcurado, [H|_], true) :- H.nome == NomeProcurado, !.
livroJaExiste(NomeProcurado, [_|T], Resultado) :- livroJaExiste(NomeProcurado, T, Resultado).

tem_nome(Nome, Livro):- Nome == Livro.nome.