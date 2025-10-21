% Base de conocimientos para componentes de PC y ensamble óptimo
% Hechos: componentes con atributos (tipo, modelo, precio, rendimiento, compatibilidad)

% CPUs
cpu('Intel Core i5-12600K', 300, 95, 'LGA1700').
cpu('AMD Ryzen 5 5600X', 250, 90, 'AM4').
cpu('Intel Core i7-12700K', 400, 100, 'LGA1700').
cpu('AMD Ryzen 7 5800X', 350, 98, 'AM4').

% Motherboards
motherboard('ASUS ROG Strix Z690-E', 400, 'LGA1700').
motherboard('MSI B550-A PRO', 150, 'AM4').
motherboard('Gigabyte Z690 AORUS Elite', 350, 'LGA1700').
motherboard('ASRock B450M PRO4', 80, 'AM4').

% RAM
ram('Corsair Vengeance LPX 16GB DDR4-3200', 80, 85, 'DDR4').
ram('G.Skill Ripjaws V 32GB DDR4-3600', 150, 90, 'DDR4').
ram('Kingston HyperX Fury 8GB DDR4-2666', 50, 75, 'DDR4').

% GPUs
gpu('NVIDIA RTX 3060', 400, 90).
gpu('AMD RX 6700 XT', 350, 88).
gpu('NVIDIA RTX 3070', 500, 95).
gpu('AMD RX 6800', 450, 92).

% Storage
storage('Samsung 970 EVO 1TB NVMe SSD', 120, 95).
storage('WD Blue 2TB HDD', 60, 70).
storage('Crucial MX500 500GB SATA SSD', 70, 85).

% PSU
psu('Corsair RM750x', 130, 90).
psu('EVGA 600 W1', 50, 75).
psu('Seasonic Focus GX-650', 100, 92).

% Case
case('Fractal Design Meshify C', 100).
case('Cooler Master MasterBox NR600', 90).
case('NZXT H510', 80).

% Reglas de compatibilidad
compatible_cpu_motherboard(CPU, Motherboard) :-
    cpu(CPU, _, _, Socket),
    motherboard(Motherboard, _, Socket).

compatible_ram_motherboard(RAM, Motherboard) :-
    ram(RAM, _, _, Type),
    motherboard(Motherboard, _, _),  % Asumiendo que todas las motherboards modernas soportan DDR4
    Type = 'DDR4'.

% Regla para ensamblar una PC completa
ensamble(CPU, Motherboard, RAM, GPU, Storage, PSU, Case, TotalPrecio, RendimientoTotal) :-
    compatible_cpu_motherboard(CPU, Motherboard),
    compatible_ram_motherboard(RAM, Motherboard),
    cpu(CPU, PrecioCPU, RendCPU, _),
    motherboard(Motherboard, PrecioMB, _),
    ram(RAM, PrecioRAM, RendRAM, _),
    gpu(GPU, PrecioGPU, RendGPU),
    storage(Storage, PrecioStorage, RendStorage),
    psu(PSU, PrecioPSU, RendPSU),
    case(Case, PrecioCase),
    TotalPrecio is PrecioCPU + PrecioMB + PrecioRAM + PrecioGPU + PrecioStorage + PrecioPSU + PrecioCase,
    RendimientoTotal is (RendCPU + RendRAM + RendGPU + RendStorage + RendPSU) / 5.

% Regla para encontrar el mejor ensamble dentro de un presupuesto
mejor_ensamble(PresupuestoMax, MejorEnsamble) :-
    findall((TotalPrecio, RendimientoTotal, [CPU, Motherboard, RAM, GPU, Storage, PSU, Case]),
            (ensamble(CPU, Motherboard, RAM, GPU, Storage, PSU, Case, TotalPrecio, RendimientoTotal),
             TotalPrecio =< PresupuestoMax),
            Ensambles),
    sort(1, @>=, Ensambles, Sorted),  % Ordenar por precio descendente (menor precio primero)
    Sorted = [MejorEnsamble | _].

% Consulta de ejemplo: mejor_ensamble(1000, Ensamble).
