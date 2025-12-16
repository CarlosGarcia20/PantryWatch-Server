import { prologService } from "../services/prologService.js";

export class sensorController {
    static async recibirDatosSensor(req, res) {
        try {
            const { zona, temperatura, humedad, peso_actual } = req.body;

            if (!zona || temperatura === undefined) {
                return res.status(400).json({ error: "Faltan datos" });
            }

            console.log(`📥 API: Recibido de ${zona} -> T:${temperatura} H:${humedad} P:${peso_actual}`);

            const resultado = await prologService.procesarDatosSensor(zona, temperatura, humedad, peso_actual);
            console.log(resultado);
            
            const listaAlertas = resultado.alertas;

            if (listaAlertas && listaAlertas.length > 0) {
                req.io.emit('alerta_pantry', { 
                    titulo: `⚠️ Atención: Estado ${resultado.estado}`,
                    cuerpo: `Se detectaron ${listaAlertas.length} problemas en ${zona}`,
                    detalles: listaAlertas
                });
            }
            
            let respuestaParaSensor = {
                status: resultado.estado,
                accion: "NINGUNA"
            };

            if (resultado.estado === 'COLAPSO') {
                console.log("🚨 ORDENANDO AL SENSOR ACTIVAR ALARMA FÍSICA");
                respuestaParaSensor.accion = "ACTIVAR_ALARMA";
            }

            return res.status(200).json(respuestaParaSensor);

        } catch (error) {
            console.error("Error en controlador:", error);
            return res.status(500).json({ error: "Error interno" });
        }
    }
}