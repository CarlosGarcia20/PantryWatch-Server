% ======================================================
%  CEREBRO DE PANTRYWATCH
% ======================================================
:- use_module(library(odbc)).

% 1. DECLARACIÓN DE DINÁMICOS
:- dynamic producto/4.        % ID, Nombre, PesoUnit, _
:- dynamic stock_minimo/2.    % ID, StockMin
:- dynamic stock_fisico/2.    % ID, StockReal
:- dynamic condicion_ideal/3. % ID, TempMax, HumMax
:- dynamic estante_actual/3.  % Estante, ID, PesoActual
:- dynamic ambiente_actual/3. % Zona, Temp, Humedad
:- dynamic zona_estante/2.    % Estante, Zona

% Configuraciones Físicas
capacidad_maxima_estante(e1, 5000). 

% ------------------------------------------------------
% 2. CONEXIÓN Y CARGA (ETL)
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
   write('📥 Descargando datos...'), nl,
   
   % Limpieza
   retractall(producto(_,_,_,_)),
   retractall(stock_minimo(_,_)),
   retractall(stock_fisico(_,_)), 
   retractall(estante_actual(_,_,_)),
   retractall(condicion_ideal(_,_,_)),
   retractall(zona_estante(_,_)), 

   % ZONAS (Ajusta según tus necesidades)
   assertz(zona_estante(e1, cocina)),
   assertz(zona_estante(e2, alacena)),

   % BLOQUE PROTEGIDO: SQL EXACTO A TU TABLA
   catch(
       (
           odbc_query(pantry_conn,
               'SELECT 
                   id_producto, 
                   nombre, 
                   peso_unitario, 
                   stock_minimo, 
                   peso_actual, 
                   COALESCE(stock_actual, 0), /* Protegemos nulos */
                   temp_max, 
                   humedad_max,
                   ubicacion
               FROM productos', 
               
               % 9 Variables para 9 Columnas
               row(ID, Nombre, PesoUnit, StockMin, PesoActual, StockReal, TMax, HMax, Ubicacion)
           ),
           
           assertz(producto(ID, Nombre, PesoUnit, 0)), 
           assertz(stock_minimo(ID, StockMin)),
           assertz(stock_fisico(ID, StockReal)),     
           assertz(estante_actual(Ubicacion, ID, PesoActual)), % Usa la ubicación real
           assertz(condicion_ideal(ID, TMax, HMax)),
           fail
       ),
       Error,
       (write('❌ ERROR SQL (Ignorable): '), write(Error), nl)
   ).

sincronizar_datos :- 
    write('✅ Datos sincronizados.'), nl.

cerrar_conexion :- 
    catch(odbc_disconnect(pantry_conn), _, true),
    write('🔌 Desconectado.').

% ------------------------------------------------------
% 3. LÓGICA BLINDADA (Aquí estaba el error)
% ------------------------------------------------------

% Helper: Detectar Agotado

% Caso A: Peso menor a 20g
detectar_agotado(ID) :- 
    estante_actual(_, ID, Peso), Peso < 20.

% Caso B: Stock Real es 0 (CON CHALECO ANTIBALAS)
% El catch(..., _, fail) significa: "Si esto da error, finge que es falso y continúa"
detectar_agotado(ID) :- 
    catch(stock_fisico(ID, 0), _, fail).

% Caso C: No está en estante
detectar_agotado(ID) :- 
    stock_minimo(ID, _), \+ estante_actual(_, ID, _).


% --- REGLA MAESTRA ---

% 1. AGOTADO
analizar_estado_zona(Zona, _, _, 'AGOTADO') :-
    zona_estante(Estante, Zona),
    (estante_actual(Estante, ID, _) ; stock_minimo(ID, _)), 
    detectar_agotado(ID), !. 

% 2. PELIGRO
analizar_estado_zona(Zona, Temp, Hum, 'PELIGRO') :-
    retractall(ambiente_actual(Zona, _, _)),
    assertz(ambiente_actual(Zona, Temp, Hum)),
    zona_estante(Estante, Zona),
    estante_actual(Estante, ID, _),
    alerta_ambiental(ID), !.

% 3. OPTIMO
analizar_estado_zona(Zona, Temp, Hum, 'OPTIMO') :-
    retractall(ambiente_actual(Zona, _, _)),
    assertz(ambiente_actual(Zona, Temp, Hum)).

% ------------------------------------------------------
% 4. SOPORTE
% ------------------------------------------------------

riesgo_temperatura(ProductoID) :-
    estante_actual(E, ProductoID, _), zona_estante(E, Zona),
    ambiente_actual(Zona, TActual, _), condicion_ideal(ProductoID, TMax, _),
    TActual > TMax.

riesgo_humedad(ProductoID) :-
    estante_actual(E, ProductoID, _), zona_estante(E, Zona),
    ambiente_actual(Zona, _, HActual), condicion_ideal(ProductoID, _, HMax),
    HActual > HMax.

alerta_ambiental(ID) :- riesgo_temperatura(ID) ; riesgo_humedad(ID).

unidades_restantes(ID, U) :- 
    estante_actual(_, ID, P), producto(ID, _, PU, _), PU > 0, U is floor(P / PU).

generar_alerta(Nombre, 'REVISAR') :- 
    producto(ID, Nombre, _, _), (detectar_agotado(ID) ; alerta_ambiental(ID)).