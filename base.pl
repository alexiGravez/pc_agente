:- use_module(library(pce)).
:- use_module(library(pce_util)).

% Base de conocimiento del sistema experto
:- dynamic usuario/4.

% Archivo para guardar los datos
:- dynamic archivo_datos/1.
archivo_datos('base_datos_hardware.pl').

% Predicado principal
inicio :-
    cargar_datos_desde_archivo,  % Cargar datos existentes al iniciar
    new(Ventana, dialog('Sistema Experto - Recomendación de Hardware')),
    send(Ventana, size, size(500, 400)),
    send(Ventana, append, label(titulo, 'Sistema Experto de Hardware')),
    send(Ventana, append, label(subtitulo, 'Complete la siguiente información:')),

    % Campo nombre
    send(Ventana, append, label(nombre_label, 'Nombre:')),
    send(Ventana, append, new(Nombre, text_item(nombre, ''))),

    % Campo tipo de equipo
    send(Ventana, append, label(tipo_label, 'Tipo de Equipo:')),
    send(Ventana, append, new(Tipo, menu(tipo, marked))),
    send_list(Tipo, append, ['Laptop', 'Desktop', 'Workstation', 'Servidor']),

    % Campo usos principales
    send(Ventana, append, label(usos_label, 'Usos Principales:')),
    send(Ventana, append, new(Usos, menu(usos, marked))),
    send_list(Usos, append, [
        'Desarrollo Software', 
        'Diseño Gráfico', 
        'Video Edición', 
        'Gaming',
        'Oficina/Productividad',
        'Ciencia de Datos',
        'Arquitectura/3D',
        'Servidores'
    ]),

    % Campo presupuesto
    send(Ventana, append, label(presupuesto_label, 'Presupuesto:')),
    send(Ventana, append, new(Presupuesto, menu(presupuesto, marked))),
    send_list(Presupuesto, append, [
        'Económico (<$1000)', 
        'Medio ($1000-$2500)', 
        'Alto ($2500-$5000)', 
        'Profesional (>$5000)'
    ]),

    % Botones
    send(Ventana, append, button(guardar, 
         message(@prolog, guardar_datos, Nombre, Tipo, Usos, Presupuesto))),
    send(Ventana, append, button(consultar, 
         message(@prolog, mostrar_recomendaciones))),
    send(Ventana, append, button(limpiar, 
         message(@prolog, limpiar_base))),
    send(Ventana, append, button(exportar, 
         message(@prolog, exportar_datos))),
    send(Ventana, append, button(salir, 
         message(@prolog, salir_y_guardar))),

    send(Ventana, open).

% Cargar datos desde archivo al iniciar
cargar_datos_desde_archivo :-
    archivo_datos(Archivo),
    (   exists_file(Archivo)
    ->  consult(Archivo),
        format('Datos cargados desde ~w~n', [Archivo])
    ;   format('Archivo ~w no encontrado. Se creará uno nuevo.~n', [Archivo])
    ).

% Guardar datos en memoria y en archivo
guardar_datos(NombreObj, TipoObj, UsosObj, PresupuestoObj) :-
    get(NombreObj, selection, Nombre),
    get(TipoObj, selection, Tipo),
    get(UsosObj, selection, Usos),
    get(PresupuestoObj, selection, Presupuesto),
    
    (   Nombre == ''
    ->  mensaje_error('Error: El nombre no puede estar vacío'),
        fail
    ;   true
    ),
    
    % Verificar si el usuario ya existe
    (   usuario(Nombre, _, _, _)
    ->  mensaje_error('Error: Ya existe un usuario con ese nombre'),
        fail
    ;   true
    ),
    
    % Generar recomendación automáticamente
    generar_recomendacion(Tipo, Usos, Presupuesto, Recomendacion),
    
    % Guardar en memoria
    assertz(usuario(Nombre, Tipo, Usos, Recomendacion)),
    
    % Guardar en archivo inmediatamente
    guardar_todos_datos,
    
    format(atom(Mensaje), 'Datos guardados exitosamente!~n~nNombre: ~w~nTipo: ~w~nUsos: ~w~nRecomendación: ~w', 
           [Nombre, Tipo, Usos, Recomendacion]),
    mensaje_exito(Mensaje).

% Guardar todos los datos en archivo
guardar_todos_datos :-
    archivo_datos(Archivo),
    tell(Archivo),
    write('% Base de datos de recomendaciones de hardware\n'),
    write('% Generado automáticamente por el sistema experto\n\n'),
    listing(usuario/4),
    told,
    format('Datos guardados en ~w~n', [Archivo]).

% Exportar datos (función adicional)
exportar_datos :-
    guardar_todos_datos,
    mensaje_exito('Todos los datos han sido exportados al archivo').

% Salir y guardar
salir_y_guardar :-
    guardar_todos_datos,
    mensaje_exito('Datos guardados. Saliendo del sistema...'),
    halt.

% Reglas de recomendación (las mismas que antes)
generar_recomendacion('Laptop', 'Desarrollo Software', 'Económico (<$1000)', 
    'Laptop con Intel i5/Ryzen 5, 16GB RAM, SSD 512GB, buena batería').

generar_recomendacion('Laptop', 'Desarrollo Software', 'Medio ($1000-$2500)',
    'Laptop con Intel i7/Ryzen 7, 32GB RAM, SSD 1TB, pantalla 15-16"').

generar_recomendacion('Laptop', 'Desarrollo Software', 'Alto ($2500-$5000)',
    'Laptop gaming/workstation: Intel i9/Ryzen 9, 64GB RAM, SSD 2TB, GPU dedicada').

generar_recomendacion('Laptop', 'Diseño Gráfico', 'Medio ($1000-$2500)',
    'Laptop con pantalla color preciso, GPU dedicada RTX 4060, 32GB RAM, SSD 1TB').

generar_recomendacion('Laptop', 'Diseño Gráfico', 'Alto ($2500-$5000)',
    'MacBook Pro 16" o workstation móvil con GPU profesional, 64GB RAM, pantalla 4K').

generar_recomendacion('Desktop', 'Video Edición', 'Medio ($1000-$2500)',
    'CPU Ryzen 7/Intel i7, GPU RTX 4070, 64GB RAM, SSD NVMe 1TB + HDD 4TB').

generar_recomendacion('Desktop', 'Video Edición', 'Alto ($2500-$5000)',
    'CPU Ryzen 9/Intel i9, GPU RTX 4090, 128GB RAM, SSD NVMe 2TB RAID').

generar_recomendacion('Desktop', 'Gaming', 'Económico (<$1000)',
    'CPU Ryzen 5/Intel i5, GPU RTX 4060, 16GB DDR5, SSD 1TB').

generar_recomendacion('Desktop', 'Gaming', 'Medio ($1000-$2500)',
    'CPU Ryzen 7/Intel i7, GPU RTX 4070 Ti, 32GB DDR5, SSD NVMe 2TB').

generar_recomendacion('Desktop', 'Gaming', 'Alto ($2500-$5000)',
    'CPU Ryzen 9/Intel i9, GPU RTX 4090, 64GB DDR5, SSD NVMe 4TB').

generar_recomendacion('Workstation', 'Ciencia de Datos', 'Alto ($2500-$5000)',
    'AMD Ryzen Threadripper, GPU RTX 4090, 128GB RAM, SSD NVMe 4TB').

generar_recomendacion('Workstation', 'Ciencia de Datos', 'Profesional (>$5000)',
    'Doble CPU EPYC/Xeon, múltiples GPUs, 256GB+ RAM ECC').

generar_recomendacion('Workstation', 'Arquitectura/3D', 'Profesional (>$5000)',
    'Workstation con GPU NVIDIA RTX A6000, Threadripper PRO, 128GB RAM ECC').

generar_recomendacion('Servidor', 'Servidores', 'Medio ($1000-$2500)',
    'Servidor rack con ECC RAM 64GB, discos SAS, RAID').

generar_recomendacion('Servidor', 'Servidores', 'Alto ($2500-$5000)',
    'Cluster servidores con virtualización, almacenamiento SAN, 128GB RAM').

% Recomendación por defecto
generar_recomendacion(Tipo, Usos, Presupuesto, Recomendacion) :-
    format(atom(Recomendacion), 
           'Configuración estándar para ~w en rango ~w para ~w', 
           [Tipo, Presupuesto, Usos]).

% Mostrar todas las recomendaciones guardadas
mostrar_recomendaciones :-
    new(Ventana, dialog('Recomendaciones Guardadas')),
    send(Ventana, size, size(700, 500)),
    
    (   findall([Nombre, Tipo, Usos, Recomendacion], 
                usuario(Nombre, Tipo, Usos, Recomendacion), 
                Usuarios),
        Usuarios \= []
    ->  send(Ventana, append, new(Texto, text_item('Buscar por nombre:', ''))),
        send(Ventana, append, button(buscar, 
             message(@prolog, buscar_por_nombre, Texto))),
        send(Ventana, append, button(exportar_ventana, 
             message(@prolog, exportar_datos))),
        send(Ventana, append, new(_, label(separator, '---'))),
        mostrar_usuarios(Usuarios, Ventana)
    ;   send(Ventana, append, 
             label(no_data, 'No hay datos guardados aún'))
    ),
    
    send(Ventana, append, button(cerrar, message(Ventana, destroy))),
    send(Ventana, open).

% Mostrar lista de usuarios
mostrar_usuarios([], _).
mostrar_usuarios([Usuario|Resto], Ventana) :-
    Usuario = [Nombre, Tipo, Usos, Recomendacion],
    format(atom(Texto1), 'Nombre: ~w', [Nombre]),
    format(atom(Texto2), 'Tipo: ~w', [Tipo]),
    format(atom(Texto3), 'Usos: ~w', [Usos]),
    format(atom(Texto4), 'Recomendación: ~w', [Recomendacion]),
    
    send(Ventana, append, label(nombre, Texto1)),
    send(Ventana, append, label(tipo, Texto2)),
    send(Ventana, append, label(usos, Texto3)),
    send(Ventana, append, label(recomendacion, Texto4)),
    send(Ventana, append, new(_, label(separator, '---'))),
    mostrar_usuarios(Resto, Ventana).

% Buscar por nombre
buscar_por_nombre(TextoObj) :-
    get(TextoObj, selection, Nombre),
    (   usuario(Nombre, Tipo, Usos, Recomendacion)
    ->  format(atom(Mensaje), 
               'Resultado de búsqueda:~n~nNombre: ~w~nTipo: ~w~nUsos: ~w~nRecomendación: ~w', 
               [Nombre, Tipo, Usos, Recomendacion]),
        mensaje_exito(Mensaje)
    ;   mensaje_error('No se encontró usuario con ese nombre')
    ).

% Limpiar base de datos (con confirmación)
limpiar_base :-
    new(D, dialog('Confirmar')),
    send(D, append, label(pregunta, '¿Está seguro de eliminar TODOS los datos?')),
    send(D, append, button(si, 
         message(@prolog, confirmar_limpiar, D))),
    send(D, append, button(no, message(D, destroy))),
    send(D, open).

confirmar_limpiar(Dialogo) :-
    send(Dialogo, destroy),
    retractall(usuario(_, _, _, _)),
    guardar_todos_datos,
    mensaje_exito('Todos los datos han sido eliminados!').

% Utilidades para mensajes
mensaje_exito(Mensaje) :-
    new(D, dialog('Éxito')),
    send(D, append, label(info, Mensaje)),
    send(D, append, button(ok, message(D, destroy))),
    send(D, open).

mensaje_error(Mensaje) :-
    new(D, dialog('Error')),
    send(D, append, label(error, Mensaje)),
    send(D, append, button(ok, message(D, destroy))),
    send(D, open).

% Cargar ejemplos para prueba
cargar_ejemplos :-
    assertz(usuario('Juan Perez', 'Laptop', 'Desarrollo Software', 
            'Laptop con Intel i7/Ryzen 7, 32GB RAM, SSD 1TB, pantalla 15-16"')),
    assertz(usuario('Maria Garcia', 'Desktop', 'Video Edición', 
            'CPU Ryzen 9/Intel i9, GPU RTX 4090, 128GB RAM, SSD NVMe 2TB')),
    assertz(usuario('Carlos Lopez', 'Workstation', 'Ciencia de Datos', 
            'AMD Ryzen Threadripper, GPU RTX 4090, 128GB RAM DDR5, SSD NVMe 4TB')),
    guardar_todos_datos,
    mensaje_exito('Ejemplos cargados y guardados exitosamente!').

% Comando para iniciar
:- inicio.