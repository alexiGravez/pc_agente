/* 
   SISTEMA EXPERTO PARA SELECCIÓN DE HARDWARE DE TRABAJO
   Base de conocimiento y motor de inferencia para recomendar
   configuraciones óptimas de hardware según necesidades específicas
*/

% HECHOS: Base de conocimiento de componentes disponibles

% CPUs disponibles (nombre, núcleos, frecuencia GHz, precio, eficiencia)
cpu(amd_ryzen_9_7950x, 16, 4.5, 650, alta).
cpu(intel_core_i9_14900k, 24, 5.8, 600, media).
cpu(amd_ryzen_7_7700x, 8, 4.5, 350, alta).
cpu(intel_core_i5_13600k, 14, 5.1, 320, media).
cpu(amd_ryzen_5_7600x, 6, 4.7, 250, alta).

% GPUs disponibles (nombre, VRAM GB, ancho_banda GB/s, precio, tipo_rendimiento)
gpu(nvidia_rtx_4090, 24, 1008, 1800, extremo).
gpu(nvidia_rtx_4080, 16, 736, 1200, alto).
gpu(amd_rx_7900_xtx, 24, 960, 1000, alto).
gpu(nvidia_rtx_4070, 12, 504, 650, medio).
gpu(amd_rx_7700_xt, 12, 432, 450, medio).

% Memoria RAM (tipo, frecuencia MHz, precio_por_32GB)
ram(ddr5, 6000, 120).
ram(ddr5, 5200, 100).
ram(ddr4, 3600, 80).
ram(ddr4, 3200, 70).

% Almacenamiento (tipo, capacidad GB, velocidad MB/s, precio)
almacenamiento(nvme_gen4, 2000, 7000, 120).
almacenamiento(nvme_gen3, 1000, 3500, 70).
almacenamiento(sata_ssd, 1000, 550, 50).
almacenamiento(hdd, 2000, 180, 60).

% REGLAS: Lógica de inferencia para recomendaciones

% Perfiles de usuario y sus requisitos
perfil_requisitos(diseno_grafico, 
    requisitos(cpu_alto, gpu_alto, ram_alta, almacenamiento_rapido)).
    
perfil_requisitos(programacion, 
    requisitos(cpu_medio, gpu_bajo, ram_media, almacenamiento_rapido)).
    
perfil_requisitos(analisis_datos, 
    requisitos(cpu_alto, gpu_medio, ram_alta, almacenamiento_masivo)).
    
perfil_requisitos(ofimatica, 
    requisitos(cpu_bajo, gpu_bajo, ram_modesta, almacenamiento_modesto)).

% Clasificación de componentes por nivel
es_cpu_alto(CPU) :- cpu(CPU, Nucleos, Frec, _, _), Nucleos >= 12, Frec >= 4.5.
es_cpu_medio(CPU) :- cpu(CPU, Nucleos, Frec, _, _), Nucleos >= 6, Nucleos < 12, Frec >= 3.5.
es_cpu_bajo(CPU) :- cpu(CPU, Nucleos, _, _, _), Nucleos < 6.

es_gpu_alto(GPU) :- gpu(GPU, _, _, _, alto).
es_gpu_alto(GPU) :- gpu(GPU, _, _, _, extremo).
es_gpu_medio(GPU) :- gpu(GPU, _, _, _, medio).
es_gpu_bajo(GPU) :- gpu(GPU, VRAM, _, Precio, _), VRAM < 8, Precio < 400.

es_ram_alta(Tipo, Frec) :- ram(Tipo, Frec, _), Frec >= 5200.
es_ram_media(Tipo, Frec) :- ram(Tipo, Frec, _), Frec >= 3200, Frec < 5200.
es_ram_modesta(Tipo, Frec) :- ram(Tipo, Frec, _), Frec < 3200.

% Reglas principales de recomendación
recomendar_configuracion(Perfil, Presupuesto, config(CPU, GPU, RAM, Almacenamiento, PrecioTotal)) :-
    perfil_requisitos(Perfil, Requisitos),
    encontrar_cpu(Requisitos, CPU, PrecioCPU),
    encontrar_gpu(Requisitos, GPU, PrecioGPU),
    encontrar_ram(Requisitos, RAM, PrecioRAM),
    encontrar_almacenamiento(Requisitos, Almacenamiento, PrecioAlmacenamiento),
    PrecioTotal is PrecioCPU + PrecioGPU + PrecioRAM + PrecioAlmacenamiento,
    PrecioTotal =< Presupuesto,
    validar_compatibilidad(CPU, RAM).

% Reglas para encontrar componentes específicos
encontrar_cpu(requisitos(cpu_alto, _, _, _), CPU, Precio) :- 
    es_cpu_alto(CPU), cpu(CPU, _, _, Precio, _).
encontrar_cpu(requisitos(cpu_medio, _, _, _), CPU, Precio) :- 
    es_cpu_medio(CPU), cpu(CPU, _, _, Precio, _).
encontrar_cpu(requisitos(cpu_bajo, _, _, _), CPU, Precio) :- 
    es_cpu_bajo(CPU), cpu(CPU, _, _, Precio, _).

encontrar_gpu(requisitos(_, gpu_alto, _, _), GPU, Precio) :- 
    es_gpu_alto(GPU), gpu(GPU, _, _, Precio, _).
encontrar_gpu(requisitos(_, gpu_medio, _, _), GPU, Precio) :- 
    es_gpu_medio(GPU), gpu(GPU, _, _, Precio, _).
encontrar_gpu(requisitos(_, gpu_bajo, _, _), GPU, Precio) :- 
    es_gpu_bajo(GPU), gpu(GPU, _, _, Precio, _).

encontrar_ram(requisitos(_, _, ram_alta, _), ram(Tipo, Frec), Precio) :- 
    es_ram_alta(Tipo, Frec), ram(Tipo, Frec, Precio).
encontrar_ram(requisitos(_, _, ram_media, _), ram(Tipo, Frec), Precio) :- 
    es_ram_media(Tipo, Frec), ram(Tipo, Frec, Precio).
encontrar_ram(requisitos(_, _, ram_modesta, _), ram(Tipo, Frec), Precio) :- 
    es_ram_modesta(Tipo, Frec), ram(Tipo, Frec, Precio).

encontrar_almacenamiento(requisitos(_, _, _, almacenamiento_rapido), Almacenamiento, Precio) :-
    almacenamiento(nvme_gen4, _, _, Precio), Almacenamiento = nvme_gen4.
encontrar_almacenamiento(requisitos(_, _, _, almacenamiento_masivo), Almacenamiento, Precio) :-
    almacenamiento(hdd, _, _, Precio), Almacenamiento = hdd.
encontrar_almacenamiento(requisitos(_, _, _, almacenamiento_modesto), Almacenamiento, Precio) :-
    almacenamiento(sata_ssd, _, _, Precio), Almacenamiento = sata_ssd.

% Reglas de compatibilidad
validar_compatibilidad(CPU, ram(ddr5, _)) :-
    cpu(CPU, _, _, _, _), 
    (atom_contenido(CPU, 'ryzen_7') ; atom_contenido(CPU, 'ryzen_9') ; atom_contenido(CPU, 'i9')).

validar_compatibilidad(CPU, ram(ddr4, _)) :-
    cpu(CPU, _, _, _, _).

atom_contenido(Atom, Sub) :-
    atom_codes(Atom, CodesAtom),
    atom_codes(Sub, CodesSub),
    sublist(CodesSub, CodesAtom).

sublist(S, L) :-
    append(_, T, L),
    append(S, _, T).

% Reglas de optimización por presupuesto
mejor_configuracion(Perfil, Presupuesto, Config) :-
    findall(config(CPU, GPU, RAM, Alm, Precio), 
            recomendar_configuracion(Perfil, Presupuesto, config(CPU, GPU, RAM, Alm, Precio)), 
            Configs),
    sort_configs_by_price(Configs, SortedConfigs),
    member(Config, SortedConfigs).

sort_configs_by_price(Configs, Sorted) :-
    predsort(compare_configs, Configs, Sorted).

compare_configs(>, config(_,_,_,_,P1), config(_,_,_,_,P2)) :- P1 > P2.
compare_configs(<, config(_,_,_,_,P1), config(_,_,_,_,P2)) :- P1 < P2.

% Consultas de ejemplo
% ?- recomendar_configuracion(diseno_grafico, 3000, Config).
% ?- mejor_configuracion(programacion, 1500, Config).
% ?- encontrar_cpu(requisitos(cpu_alto,_,_,_), CPU, Precio).