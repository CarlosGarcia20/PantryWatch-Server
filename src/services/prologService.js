import swipl from 'swipl-stdio';
import { PROLOG_FILE_PATH } from '../config/paths.js';

class PrologService {
    constructor() {
        this.engine = new swipl.Engine();
        this.isInitialized = false;
    }

    async init() {
        if (this.isInitialized) return;
        try {
            console.log("🧠 Servicio Prolog: Iniciando motor...");
            // Normalizamos ruta para evitar errores de espacios/windows
            const ruta = PROLOG_FILE_PATH.replace(/\\/g, '/');
            
            await this.engine.call(`consult('${ruta}')`);
            
            // Esto conecta a la BD y carga las zonas (e1, e2) definidas en Prolog
            await this.engine.call('iniciar_sistema.');
            
            this.isInitialized = true;
            console.log("✅ Servicio Prolog: Listo y conectado a BD.");
        } catch (error) {
            console.error("❌ Error fatal en PrologService:", error);
            throw error;
        }
    }

   async procesarDatosSensor(zonaRecibida, temperatura, humedad, peso_actual) {
        if (!this.isInitialized) await this.init();

        const zona = zonaRecibida.toLowerCase();

        try {
            console.log(`🧠 Prolog: Analizando ${zona} (T:${temperatura} H:${humedad} P:${peso_actual})...`);

            const queryString = `analizar_estado_zona(${zona}, ${temperatura}, ${humedad}, ${peso_actual}, Estado).`;
            const queryEstado = await this.engine.createQuery(queryString);
            
            let estadoGeneral = 'DESCONOCIDO';

            try {
                const result = await queryEstado.next();
                if (result) estadoGeneral = result.Estado; 
            } finally {
                await queryEstado.close();
            }

            let alertasDetalladas = [];

            if (estadoGeneral === 'COLAPSO') {
                alertasDetalladas.push({
                    producto: "ESTRUCTURA ALACENA",
                    mensaje: "🚨 PELIGRO DE DERRUMBE: Exceso de peso detectado"
                });
            }

            else if (estadoGeneral === 'AGOTADO') {
                const queryAgotados = await this.engine.createQuery(
                    `zona_estante(E, ${zona}), 
                    (estante_actual(E, ID, _) ; stock_minimo(ID, _)), 
                    producto(ID, Nombre, _, _), 
                    detectar_agotado(ID).`
                );

                let row;
                const vistos = new Set();
                while (row = await queryAgotados.next()) {
                    const nombreProd = row.Nombre; 
                    if (nombreProd && !vistos.has(nombreProd)) {
                        alertasDetalladas.push({
                            producto: nombreProd,
                            mensaje: "⚠️ STOCK BAJO / AGOTADO"
                        });
                        vistos.add(nombreProd);
                    }
                }
                await queryAgotados.close();
            } 

            else if (estadoGeneral === 'PELIGRO') {
                const queryPeligro = await this.engine.createQuery(
                    `producto(ID, Nombre, _, _), 
                    estante_actual(E, ID, _), 
                    zona_estante(E, ${zona}), 
                    obtener_detalle_alerta(ID, Mensaje).` 
                );

                let row;
                while (row = await queryPeligro.next()) {
                    const nombreProd = row.Nombre;
                    const mensajeError = row.Mensaje;

                    if (nombreProd) {
                        alertasDetalladas.push({
                            producto: nombreProd,
                            mensaje: mensajeError || "🔥 RIESGO AMBIENTAL"
                        });
                    }
                }
                await queryPeligro.close();
            }

            return {
                estado: estadoGeneral,   
                alertas: alertasDetalladas 
            };

        } catch (error) {
            console.error("❌ Error en Prolog Service:", error);
            return { estado: "ERROR", alertas: [] };
        }
    }

    async recargarConocimiento() {
        if (!this.isInitialized) await this.init();

        try {
            console.log("🔄 Prolog: Ejecutando Hot Reload (ETL)...");

            await this.engine.call('sincronizar_datos.');
            
            console.log("✅ Prolog: Conocimiento actualizado exitosamente.");
            return true;
        } catch (error) {
            console.error("❌ Error en Hot Reload:", error);
            return false;
        }
    }
}

export const prologService = new PrologService();