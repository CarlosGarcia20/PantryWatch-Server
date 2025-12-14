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

    async procesarDatosSensor(zonaRecibida, temperatura, humedad) {
        if (!this.isInitialized) await this.init();

        // IMPORTANTE: Prolog necesita átomos en minúscula (cocina, no Cocina)
        const zona = zonaRecibida.toLowerCase();

        try {
            console.log(`🧠 Prolog: Analizando ${zona} (T:${temperatura} H:${humedad})...`);

            // PASO 1: Preguntar el ESTADO GENERAL
            const queryString = `analizar_estado_zona(${zona}, ${temperatura}, ${humedad}, Estado).`;

            const queryEstado = await this.engine.createQuery(queryString);
            
            let estadoGeneral = 'DESCONOCIDO';

            try {
                const result = await queryEstado.next();

                if (result) {
                    estadoGeneral = result.Estado; 
                }
            } finally {
                await queryEstado.close();
            }

            let alertasDetalladas = [];

            // PASO 2: Si está AGOTADO, buscamos qué falta
            if (estadoGeneral === 'AGOTADO') {
                const queryAgotados = await this.engine.createQuery(
                    `zona_estante(E, ${zona}), 
                    (estante_actual(E, ID, _) ; stock_minimo(ID, _)), 
                    producto(ID, Nombre, _, _), 
                    detectar_agotado(ID).`
                );

                let row;
                const vistos = new Set();
                // CORRECCIÓN AQUÍ: row ya es el objeto, no usamos .value
                while (row = await queryAgotados.next()) {
                    // Antes: row.value.Nombre  -> AHORA: row.Nombre
                    const nombreProd = row.Nombre; 
                    
                    if (nombreProd && !vistos.has(nombreProd)) {
                        alertasDetalladas.push({
                            producto: nombreProd,
                            mensaje: "⚠️ PRODUCTO AGOTADO (Rellenar)"
                        });
                        vistos.add(nombreProd);
                    }
                }
                await queryAgotados.close();
            } 
            
            // PASO 3: Si está en PELIGRO, buscamos riesgos ambientales
            else if (estadoGeneral === 'PELIGRO') {
                const queryPeligro = await this.engine.createQuery(
                    `producto(ID, Nombre, _, _), 
                    estante_actual(E, ID, _), 
                    zona_estante(E, ${zona}), 
                    alerta_ambiental(ID).`
                );

                let row;
                while (row = await queryPeligro.next()) {
                    alertasDetalladas.push({
                        producto: row.value.Nombre,
                        mensaje: "🔥 RIESGO AMBIENTAL (Temp/Hum)"
                    });
                }
                await queryPeligro.close();
            }

            // Retornamos el paquete completo
            return {
                estado: estadoGeneral,   
                alertas: alertasDetalladas 
            };

        } catch (error) {
            console.error("❌ Error en Prolog Service:", error);
            // Fallback seguro
            return { estado: "ERROR", alertas: [] };
        }
    }
}

export const prologService = new PrologService();