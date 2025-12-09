% ======================================================
%  CEREBRO DE PANTRYWATCH (FINAL INTEGRADO)
% ======================================================
:- use_module(library(odbc)).

% 1. DECLARACIÓN DE DINÁMICOS (Memoria RAM)
:- dynamic producto/4.        % ID, Nombre, PesoUnit, _
:- dynamic stock_minimo/2.    % ID, StockMin
:- dynamic condicion_ideal/3. % ID, TempMax, HumMax
:- dynamic estante_actual/3.  % Estante, ID, PesoActual
:- dynamic ambiente_actual/3. % Zona, Temp, Humedad
:- dynamic zona_estante/2.    % Estante, Zona

% ------------------------------------------------------
% 2. CONEXIÓN Y CARGA (ETL)
% ------------------------------------------------------

iniciar_sistema :-
    write('🔌 Prolog: Intentando conectar ODBC...'), nl,
    % Conectamos a Postgres
    catch(
        odbc_connect('PantryDB', _Connection, [alias(pantry_conn), open(once)]),
        error(existence_error(_, _), _), % Ignorar si ya está conectado
        (write('⚠️ Ya estaba conectado o driver ocupado.'), nl)
    ),
    sincronizar_datos.

sincronizar_datos :-
   write('📥 Descargando datos de la Base de datos...'), nl,
    
    % Limpieza
    retractall(producto(_,_,_,_)),
    retractall(stock_minimo(_,_)),
    retractall(condicion_ideal(_,_,_)),
    retractall(estante_actual(_,_,_)),

    % Query SQL
    odbc_query(pantry_conn,
        'SELECT 
            id_producto, 
            nombre, 
            peso_unitario, 
            stock_minimo, 
            peso_actual, 
            temp_max, 
            humedad_max 
        FROM productos', 
        
        % 2. Ahora sí, 7 columnas SQL = 7 variables Prolog
        row(ID, Nombre, PesoUnit, StockMin, PesoActual, TMax, HMax)
    ),
    
    % Guardar en RAM
    assertz(producto(ID, Nombre, PesoUnit, 0)), 
    assertz(stock_minimo(ID, StockMin)),
    assertz(estante_actual(e1, ID, PesoActual)), % Asignamos e1 por defecto
    assertz(condicion_ideal(ID, TMax, HMax)),
    
    fail. % Backtracking forzado

sincronizar_datos :- 
    write('✅ Datos sincronizados.'), nl.

cerrar_conexion :- 
    catch(odbc_disconnect(pantry_conn), _, true),
    write('🔌 Desconectado.').

% ------------------------------------------------------
% 3. INTERFAZ DE SENSORES (NUEVO)
% ------------------------------------------------------

% Llamado por Node.js: actualizar_sensor(cocina, 32, 60).
actualizar_sensor(Zona, NuevaTemp, NuevaHum) :-
    retractall(ambiente_actual(Zona, _, _)),
    assertz(ambiente_actual(Zona, NuevaTemp, NuevaHum)),
    format('📡 Sensor ~w actualizado: T=~w H=~w~n', [Zona, NuevaTemp, NuevaHum]).

% Helper para detectar peligros (usado por Node)
detectar_peligro_calor(Nombre, TempActual) :-
    riesgo_ambiental_temp(ID), % Usamos tu regla original
    producto(ID, Nombre, _, _),
    % Recuperamos la temperatura actual para mostrarla en el mensaje
    estante_actual(Estante, ID, _),
    zona_estante(Estante, Zona),
    ambiente_actual(Zona, TempActual, _).

% ------------------------------------------------------
% 4. TUS REGLAS DE NEGOCIO (Lógica Pura)
% ------------------------------------------------------

% Unidades restantes
unidades_restantes(ProductoID, Unidades) :-
    estante_actual(_, ProductoID, PesoActual),
    producto(ProductoID, _, PesoUnitario, _),
    PesoUnitario > 0, !,
    Unidades is floor(PesoActual / PesoUnitario).

unidades_restantes(ProductoID, 0) :-
    producto(ProductoID, _, _, _),
    \+ estante_actual(_, ProductoID, _).

% Necesita reponer
necesita_reponer(ProductoID) :-
    estante_actual(_, ProductoID, PesoActual),
    stock_minimo(ProductoID, PesoMinimo),
    PesoActual =< PesoMinimo.

necesita_reponer(ProductoID) :-
    stock_minimo(ProductoID, _),
    \+ estante_actual(_, ProductoID, _).

% Riesgos Ambientales (Leen de ambiente_actual)
riesgo_ambiental_temp(ProductoID) :-
    estante_actual(EstanteID, ProductoID, _),
    zona_estante(EstanteID, ZonaID),
    ambiente_actual(ZonaID, TempActual, _),
    condicion_ideal(ProductoID, TempMax, _),
    TempActual > TempMax.  

riesgo_ambiental_hum(ProductoID) :-
    estante_actual(EstanteID, ProductoID, _),
    zona_estante(EstanteID, ZonaID),
    ambiente_actual(ZonaID, _, HumedadActual),
    condicion_ideal(ProductoID, _, HumedadMax),
    HumedadActual > HumedadMax. 

alerta_ambiental(ProductoID) :-
    riesgo_ambiental_temp(ProductoID) ;
    riesgo_ambiental_hum(ProductoID).

% Generador de alertas
generar_alerta(ProductoID, 'Stock bajo') :-
    necesita_reponer(ProductoID).

generar_alerta(ProductoID, 'Condiciones ambientales inadecuadas') :-
    \+ necesita_reponer(ProductoID), 
    once(alerta_ambiental(ProductoID)). 

% Consultas Helper
reporte_completo(Nombre, Mensaje) :-
    producto(ID, Nombre, _, _),
    generar_alerta(ID, Mensaje).

info_producto(Nombre, Unidades) :-
    consultar_unidades(Nombre, Unidades).

consultar_unidades(NombreProducto, Unidades) :-
    producto(ProductoID, NombreProducto, _, _),
    unidades_restantes(ProductoID, Unidades).