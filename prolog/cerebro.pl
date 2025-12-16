% ======================================================
%  CEREBRO DE PANTRYWATCH
% ======================================================
:- use_module(library(odbc)).

% 1. DECLARACIÓN DE DINÁMICOS
:- dynamic producto/4.        % ID, Nombre, _Ignorado, _Ignorado
:- dynamic stock_minimo/2.    % ID, StockMin
:- dynamic stock_fisico/2.    % ID, StockReal
:- dynamic condicion_ideal/3. % ID, TempMax, HumMax
:- dynamic estante_actual/3.  % Estante, ID, _Ignorado
:- dynamic ambiente_actual/3. % Zona, Temp, Humedad
:- dynamic zona_estante/2.    % Estante, Zona
:- dynamic capacidad_estante/2. % ID_Zona, CapacidadMax

% ------------------------------------------------------
% 2. CONEXIÓN Y CARGA
% ------------------------------------------------------

iniciar_sistema :-
    write('🔌 Prolog: Conectando...'), nl,
    catch(
        odbc_connect('PantryDB', _Connection, [alias(pantry_conn), open(once)]),
        _, 
        (write('⚠️ Ya conectado.'), nl)
    ),
    sincronizar_datos.

sincronizar_datos :-
   write('📥 Sincronizando Base de Conocimiento...'), nl,
   
   % 1. Limpieza de memoria (Esto hace que funcione el Hot Reload)
   retractall(producto(_,_,_,_)),
   retractall(stock_minimo(_,_)),
   retractall(stock_fisico(_,_)), 
   retractall(estante_actual(_,_,_)),
   retractall(condicion_ideal(_,_,_)),
   retractall(zona_estante(_,_)), 
   retractall(capacidad_estante(_,_)),

   % 2. Configuración Lógica
   assertz(zona_estante(e1, cocina)),

   % 3. Carga Estante (Por Nombre)
   catch(
       (
           odbc_query(pantry_conn, 'SELECT peso_maximo FROM contenedores WHERE nombre = ''Estante 1''', row(Cap)), 
           assertz(capacidad_estante(e1, Cap))
       ),
       _, 
       (write('⚠️ Usando default estante.'), assertz(capacidad_estante(e1, 5000)))
   ),

   % 4. Carga Productos (Casteando ID a String para evitar errores)
   catch(
       (
           odbc_query(pantry_conn,
               'SELECT CAST(id_producto AS VARCHAR), nombre, stock_minimo, COALESCE(stock_actual, 0), temp_max, humedad_max, ubicacion FROM productos', 
               row(ID_Str, Nombre, StockMin, StockReal, TMax, HMax, UbicacionRaw)
           ),
           atom_string(ID, ID_Str),
           (atom(UbicacionRaw) -> Ubicacion = UbicacionRaw ; atom_string(Ubicacion, UbicacionRaw)),

           assertz(producto(ID, Nombre, 0, 0)),
           assertz(stock_minimo(ID, StockMin)),
           assertz(stock_fisico(ID, StockReal)),
           assertz(estante_actual(Ubicacion, ID, 0)),
           assertz(condicion_ideal(ID, TMax, HMax)),
           fail
       ),
       _, true
   ),
   write('✅ IA: Conocimiento actualizado.'), nl.

cerrar_conexion :- 
    catch(odbc_disconnect(pantry_conn), _, true),
    write('🔌 Desconectado.').

% ------------------------------------------------------
% 3. REGLAS DE DIAGNÓSTICO (SIMPLIFICADAS)
% ------------------------------------------------------

% A. DETECTAR AGOTADO (Solo lógica de Stock)

% Caso 1: Stock Real es 0 (Agotado total)
detectar_agotado(ID) :- 
    catch(stock_fisico(ID, 0), _, fail).

% Caso 2: Stock Real es menor al Mínimo (Stock Bajo)
detectar_agotado(ID) :-
    stock_fisico(ID, StockReal),
    stock_minimo(ID, StockMin),
    StockReal < StockMin.

% Caso 3: Respaldo (Si el producto existe en BD pero no está asignado a estante)
detectar_agotado(ID) :- 
    stock_minimo(ID, _), \+ estante_actual(_, ID, _).


% B. DIAGNÓSTICO CLIMÁTICO (Las 8 Reglas)
diagnostico_climatico(ID, 'PELIGRO: Calor Excesivo') :-
    datos_ambiente(ID, T, _), condicion_ideal(ID, TMax, _), T > TMax.

diagnostico_climatico(ID, 'PELIGRO: Humedad Alta (Riesgo Moho)') :-
    datos_ambiente(ID, _, H), condicion_ideal(ID, _, HMax), H > HMax.

diagnostico_climatico(ID, 'CRITICO: Ambiente Sofocante (Calor+Humedad)') :-
    datos_ambiente(ID, T, H), condicion_ideal(ID, TMax, HMax), T > TMax, H > HMax.

diagnostico_climatico(ID, 'ADVERTENCIA: Temperatura subiendo') :-
    datos_ambiente(ID, T, _), condicion_ideal(ID, TMax, _),
    Lim is TMax - 3, T > Lim, T =< TMax.

diagnostico_climatico(ID, 'RIESGO: Chocolate derritiendose') :-
    producto(ID, Nombre, _, _), sub_atom(Nombre, _, _, _, 'Chocolate'),
    datos_ambiente(ID, T, _), T > 24.

% Helper de ambiente
datos_ambiente(ID, T, H) :-
    estante_actual(E, ID, _), zona_estante(E, Zona), ambiente_actual(Zona, T, H).


% ------------------------------------------------------
% 4. REGLA MAESTRA DE ESTADOS (PRIORIDADES)
% ------------------------------------------------------

% PRIORIDAD 0: COLAPSO ESTRUCTURAL
analizar_estado_zona(Zona, _, _, PesoTotalSensor, 'COLAPSO') :-
    zona_estante(Estante, Zona),          
    capacidad_estante(Estante, CapacidadMax), 
    PesoTotalSensor > CapacidadMax, !.

% PRIORIDAD 1: AGOTADO
analizar_estado_zona(Zona, _, _, _, 'AGOTADO') :-
    zona_estante(Estante, Zona),
    (estante_actual(Estante, ID, _) ; stock_minimo(ID, _)), 
    detectar_agotado(ID), !. 

% PRIORIDAD 2: PELIGRO AMBIENTAL
analizar_estado_zona(Zona, Temp, Hum, _, 'PELIGRO') :-
    retractall(ambiente_actual(Zona, _, _)),
    assertz(ambiente_actual(Zona, Temp, Hum)),
    zona_estante(Estante, Zona),
    estante_actual(Estante, ID, _),
    diagnostico_climatico(ID, _), !.

% PRIORIDAD 3: OPTIMO
analizar_estado_zona(Zona, Temp, Hum, _, 'OPTIMO') :-
    retractall(ambiente_actual(Zona, _, _)),
    assertz(ambiente_actual(Zona, Temp, Hum)).

% ------------------------------------------------------
% 5. INTERFAZ NODE.JS
% ------------------------------------------------------
obtener_detalle_alerta(ID, Mensaje) :- diagnostico_climatico(ID, Mensaje).