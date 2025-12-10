import { prologService } from "../services/prologService.js";

export class sensorController {
    static async recibirDatosSensor(req,res) {
        try {
            const { zona, temperatura, humedad, peso_actual } = req.body;

            if (!zona || temperatura === undefined || humedad === undefined) {
                return res.status(400).json({ error: "Faltan datos (zona, temperatura, humedad)" });
            }

            console.log(`📥 API: Recibido de ${zona} -> T:${temperatura} H:${humedad} P:${peso_actual}`);

            // if (req.io) {
            //     req.io.emit('actualizacion_sensores', {
            //         zona: zona,
            //         temperatura: temperatura,
            //         humedad: humedad,
            //         peso: peso_actual,
            //         timestamp: new Date()
            //     });
            //     console.log("📡 Datos enviados a la App Móvil vía Socket");
            // } else {
            //     console.warn("⚠️ No se encontró instancia de Socket.io (req.io)");
            // }

            // Llamamos al servicio (Caja negra)
            const alertas = await prologService.procesarDatosSensor(zona, temperatura, humedad);

            if (alertas.length > 0) {
                console.log("   🔥 Respondiendo ALERTA al ESP32");
                return res.status(200).json({ status: "ALERTA", data: alertas });
            }

            return res.status(200).json({ status: "OK", data: [] });
        } catch (error) {
            console.error("Error en controlador:", error);
            return res.status(500).json({ error: "Error interno del servidor" });
        }
    }
}