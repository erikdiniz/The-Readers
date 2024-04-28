:- use_module(librart(http/json)).

escreveUsuario(IdUsuario, Senha) :-
    open('Data/usuarios.json', read, json),
    json_read_dict(json, usuarios_json),
    converte_json_usuarios(usuarios_json, usuarios),
    cria_usuario(IdUsuario, [], [], Senha),

converte_json_usuarios([],[]).
converte_json_usuarios([X, XS],[Usuario, ProximosUsuarios]):-
    swritef(X, '{
        "idUsuario":"%w",
        "seguidores":%w,
        "seguindo":%w,
        "senha":"%w""
    }',
    [IdUsuario, Seguidores, Seguindo, Senha]),
    Usuario = [IdUsuario, Seguidores, Seguindo, Senha],
    converte_json_usuarios(XS, ProximosUsuarios).

cria_usuario(IdUsuario, Seguidores, Seguindo, Senha):-
    