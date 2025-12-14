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
            await this.engine.call('iniciar_sistema.');
            
            // Config inicial
            await this.engine.call('retractall(zona_estante(_,_))');
            await this.engine.call('assertz(zona_estante(e1, cocina))');
            
            this.isInitialized = true;
            console.log("✅ Servicio Prolog: Listo y conectado a BD.");
        } catch (error) {
            console.error("❌ Error fatal en PrologService:", error);
            throw error;
        }
    }

    async procesarDatosSensor(zona, temperatura, humedad) {
        if (!this.isInitialized) await this.init();

        // 1. Inyectar
        await this.engine.call(`actualizar_sensor(${zona}, ${temperatura}, ${humedad})`);

        // 2. Consultar Peligros
        const query = await this.engine.createQuery('detectar_peligro_calor(Nombre, Temp).');
        let alertas = [];
        let result;
        
        while (result = await query.next()) {
            alertas.push({
                producto: result.Nombre,
                mensaje: `Riesgo crítico a ${result.Temp}°C`
            });
        }
        await query.close();
        
        return alertas;
    }
}

// Exportamos una instancia única (Singleton)
export const prologService = new PrologService();