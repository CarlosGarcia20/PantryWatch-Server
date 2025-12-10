% ======================================================
%  CEREBRO DE PANTRYWATCH (FINAL INTEGRADO v2.0)
% ======================================================
:- use_module(library(odbc)).

% 1. DECLARACIÓN DE DINÁMICOS (Memoria RAM)
:- dynamic producto/4.        % ID, Nombre, PesoUnit, _
:- dynamic stock_minimo/2.    % ID, StockMin
:- dynamic condicion_ideal/3. % ID, TempMax, HumMax
:- dynamic estante_actual/3.  % Estante, ID, PesoActual
:- dynamic ambiente_actual/3. % Zona, Temp, Humedad
:- dynamic zona_estante/2.    % Estante, Zona

% Configuraciones Físicas (Hechos estáticos)
capacidad_maxima_estante(e1, 5000). % El estante 1 aguanta 5kg (5000g)

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
    retractall(zona_estante(_,_)), % Limpiamos zonas también

    % Configuración inicial de zonas (Asumimos e1 = cocina por defecto)
    assertz(zona_estante(e1, cocina)),

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
        row(ID, Nombre, PesoUnit, StockMin, PesoActual, TMax, HMax)
    ),
    
    % Guardar en RAM
    assertz(producto(ID, Nombre, PesoUnit, 0)), 
    assertz(stock_minimo(ID, StockMin)),
    assertz(estante_actual(e1, ID, PesoActual)), 
    assertz(condicion_ideal(ID, TMax, HMax)),
    
    fail. % Backtracking forzado

sincronizar_datos :- 
    write('✅ Datos sincronizados.'), nl.

cerrar_conexion :- 
    catch(odbc_disconnect(pantry_conn), _, true),
    write('🔌 Desconectado.').

% ------------------------------------------------------
% 3. INTERFAZ DE SENSORES
% ------------------------------------------------------

% Llamado por Node.js: actualizar_sensor(cocina, 32, 60).
actualizar_sensor(Zona, NuevaTemp, NuevaHum) :-
    retractall(ambiente_actual(Zona, _, _)),
    assertz(ambiente_actual(Zona, NuevaTemp, NuevaHum)),
    format('📡 Sensor ~w actualizado: T=~w H=~w~n', [Zona, NuevaTemp, NuevaHum]).

% Helper para detectar peligros (usado por Node)
detectar_peligro_calor(Nombre, TempActual) :-
    riesgo_ambiental_temp(ID), 
    producto(ID, Nombre, _, _),
    estante_actual(Estante, ID, _),
    zona_estante(Estante, Zona),
    ambiente_actual(Zona, TempActual, _).

% ------------------------------------------------------
% 4. REGLAS BÁSICAS (Lógica Original)
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

% Riesgos Ambientales
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

% Generador de alertas básicas
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

% ======================================================
%  5. NUEVOS MÓDULOS (EXPANSIÓN PANTRYWATCH)
% ======================================================

% ------------------------------------------------------
% MÓDULO A: EL BUSCADOR (Ubicación y Estantes)
% ------------------------------------------------------

% 1. Listar nombres de productos en un estante
productos_en_estante(Estante, ListaNombres) :-
    findall(Nombre, (
        estante_actual(Estante, ID, _),
        producto(ID, Nombre, _, _)
    ), ListaNombres).

% 2. ¿En qué estante está X producto?
donde_esta_producto(Nombre, Estante) :-
    producto(ID, Nombre, _, _),
    estante_actual(Estante, ID, _).

% 3. ¿El estante está vacío?
estante_vacio(Estante) :-
    zona_estante(Estante, _),
    \+ estante_actual(Estante, _, _).

% 4. Contar items distintos en un estante
cantidad_items_estante(Estante, Cantidad) :-
    findall(ID, estante_actual(Estante, ID, _), Lista),
    length(Lista, Cantidad).

% ------------------------------------------------------
% MÓDULO B: EXPERTO EN CONSERVACIÓN (Análisis Profundo)
% ------------------------------------------------------

% 5. Calcular exceso de temperatura (Grados por encima del maximo)
diferencia_termica(ProductoID, Diferencia) :-
    riesgo_ambiental_temp(ProductoID),
    estante_actual(Estante, ProductoID, _),
    zona_estante(Estante, Zona),
    ambiente_actual(Zona, TActual, _),
    condicion_ideal(ProductoID, TMax, _),
    Diferencia is TActual - TMax.

% 6. Estado: CRITICO (Se pasa por mas de 5 grados)
estado_conservacion(Nombre, 'CRITICO - SE VA A PUDRIR') :-
    producto(ID, Nombre, _, _),
    diferencia_termica(ID, Dif),
    Dif > 5.

% 7. Estado: ADVERTENCIA (Se pasa por poco)
estado_conservacion(Nombre, 'ADVERTENCIA - REVISAR') :-
    producto(ID, Nombre, _, _),
    diferencia_termica(ID, Dif),
    Dif > 0, Dif =< 5.

% 8. Estado: OPTIMO
estado_conservacion(Nombre, 'OPTIMO') :-
    producto(ID, Nombre, _, _),
    \+ riesgo_ambiental_temp(ID),
    \+ riesgo_ambiental_hum(ID).

% 9. Riesgo de Hongos (Humedad Alta + Calor)
riesgo_hongos(Nombre) :-
    producto(ID, Nombre, _, _),
    riesgo_ambiental_hum(ID),
    riesgo_ambiental_temp(ID).

% ------------------------------------------------------
% MÓDULO C: GESTIÓN DE ESPACIO (Logística)
% ------------------------------------------------------

% 10. Peso total en un estante
peso_total_estante(Estante, PesoTotal) :-
    findall(Peso, estante_actual(Estante, _, Peso), Pesos),
    sum_list(Pesos, PesoTotal).

% 11. Calcular espacio libre
espacio_libre(Estante, GramosLibres) :-
    capacidad_maxima_estante(Estante, Max),
    peso_total_estante(Estante, Actual),
    GramosLibres is Max - Actual.

% 12. Alerta de sobrecarga estructural
alerta_sobrecarga(Estante) :-
    espacio_libre(Estante, Libres),
    Libres < 0.

% 13. ¿Cabe un producto nuevo? (Simulación)
cabe_producto_nuevo(Estante, NombreProducto) :-
    producto(_, NombreProducto, PesoUnit, _),
    espacio_libre(Estante, Libres),
    Libres >= PesoUnit.

% ------------------------------------------------------
% MÓDULO D: AUDITORÍA DE STOCK
% ------------------------------------------------------

% 14. Exceso de Stock (Más del triple del minimo)
exceso_de_stock(Nombre) :-
    producto(ID, Nombre, _, _),
    estante_actual(_, ID, PesoActual),
    stock_minimo(ID, Min),
    PesoActual > (Min * 3).

% 15. Stock "Basura" (Botes vacíos ocupando espacio)
stock_basura(Nombre) :-
    producto(ID, Nombre, PesoUnit, _),
    estante_actual(_, ID, PesoActual),
    PesoActual < (PesoUnit * 0.1). 

% 16. Identificar productos que requieren frío
requiere_refrigeracion(Nombre) :-
    producto(ID, Nombre, _, _),
    condicion_ideal(ID, TMax, _),
    TMax < 20.

% 17. Alerta de Cadena de Frío rota
alerta_cadena_frio(Nombre) :-
    requiere_refrigeracion(Nombre),
    producto(ID, Nombre, _, _),
    estante_actual(Estante, ID, _),
    zona_estante(Estante, Zona),
    ambiente_actual(Zona, TActual, _),
    TActual > 25.

% ------------------------------------------------------
% MÓDULO E: DIAGNÓSTICO DE SISTEMA (Sanidad)
% ------------------------------------------------------

% 18. Detectar sensor roto (Temperaturas imposibles)
sensor_fallando(Zona) :-
    ambiente_actual(Zona, Temp, _),
    (Temp < -20 ; Temp > 70).

% 19. Zona Ciega (Configurada pero sin datos)
zona_ciega(Zona) :-
    zona_estante(_, Zona),
    \+ ambiente_actual(Zona, _, _).

% 20. Sugerencia de Reubicación Inteligente
sugerir_mover(Nombre, ZonaActual, ZonaDestino) :-
    riesgo_ambiental_temp(ID),
    producto(ID, Nombre, _, _),
    estante_actual(EActual, ID, _),
    zona_estante(EActual, ZonaActual),
    ambiente_actual(ZonaDestino, TDestino, _),
    condicion_ideal(ID, TMax, _),
    TDestino < TMax. % En la otra zona sí cabe