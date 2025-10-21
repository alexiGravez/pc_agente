% Interfaz gráfica usando XPCE para guardar datos del usuario: nombre, tipo de equipo, tipo de hardware y usos

:- use_module(library(pce)).
:- use_module(library(theme/dark)).

% Predicado principal para lanzar la interfaz
start :-
    new(Dialog, dialog('Datos del Usuario')),
    send(Dialog, append, new(NombreField, text_item('Nombre'))),
    send(Dialog, append, new(TipoField, text_item('Tipo de Equipo (ej. gaming, oficina)'))),
    send(Dialog, append, new(HardwareField, text_item('Tipo de Hardware (escritorio / portatil)'))),
    send(Dialog, append, new(UsosField, text_item('Usos (ej. trabajo, juegos)'))),
    send(Dialog, append, button('Guardar', message(@prolog, guardar_datos, NombreField, TipoField, HardwareField, UsosField, Dialog))),
    send(Dialog, append, button('Mostrar Datos', message(@prolog, mostrar_datos))),
    send(Dialog, append, button('Limpiar Datos', message(@prolog, limpiar_datos))),
    send(Dialog, open).

% Predicado para guardar los datos en usuario.pl
guardar_datos(NombreField, TipoField, HardwareField, UsosField, Dialog) :-
    get(NombreField, selection, Nombre),
    get(TipoField, selection, Tipo),
    get(HardwareField, selection, Hardware),
    get(UsosField, selection, Usos),
    tell('usuario.pl'),
    write('datos_usuario('), writeq(Nombre), write(', '), writeq(Tipo), write(', '), writeq(Hardware), write(', '), writeq(Usos), write(').'), nl,
    told,
    send(Dialog, report, status, 'Datos guardados exitosamente en usuario.pl.'),
    send(NombreField, clear),
    send(TipoField, clear),
    send(HardwareField, clear),
    send(UsosField, clear).

% Predicado para mostrar datos guardados desde usuario.pl
mostrar_datos :-
    (   exists_file('usuario.pl')
    ->  consult('usuario.pl'),
        (   datos_usuario(Nombre, Tipo, Hardware, Usos)
        ->  format('Nombre: ~w~nTipo de Equipo: ~w~nTipo de Hardware: ~w~nUsos: ~w~n', [Nombre, Tipo, Hardware, Usos])
        ;   write('No hay datos guardados.')
        )
    ;   write('Archivo usuario.pl no encontrado.')
    ).

% Predicado para limpiar datos (eliminar archivo usuario.pl)
limpiar_datos :-
    (   exists_file('usuario.pl')
    ->  delete_file('usuario.pl'),
        write('Datos eliminados (archivo usuario.pl borrado).')
    ;   write('Archivo usuario.pl no encontrado.')
    ).

% Para ejecutar la interfaz, llamar a start.
