import { prologService } from "../services/prologService.js";

export class sensorController {
    static async recibirDatosSensor(req,res) {
        try {
            const { zona, temperatura, humedad, peso_actual } = req.body;

            if (!zona || temperatura === undefined || humedad === undefined) {
                return res.status(400).json({ error: "Faltan datos (zona, temperatura, humedad)" });
            }

            console.log(`📥 API: Recibido de ${zona} -> T:${temperatura} H:${humedad} P:${peso_actual}`);

            const alertas = await prologService.procesarDatosSensor(zona, temperatura, humedad);

            if (alertas && alertas.length > 0) {
                console.log(`🔥 Enviando ${alertas.length} alertas al Socket`);

                req.io.emit('alerta_pantry', { 
                    titulo: "⚠️ Atención en Alacena",
                    cuerpo: `Se detectaron ${alertas.length} problemas en ${zona}`,
                    // 3. Enviamos el array directo
                    detalles: alertas 
                });
            }

            return res.status(200).json({ status: "OK", data: [] });
        } catch (error) {
            console.error("Error en controlador:", error);
            return res.status(500).json({ error: "Error interno del servidor" });
        }
    }
}