import { productoModelo } from "../models/producto.model.js";

export class productoController {
    static async crear(req, res) {
        try {
            const resultado = await productoModelo.crear(req.body);

            if(!resultado.success) {
                
            }
        } catch (error) {
            console.log(error);
            
            return res.status(500).json({ mensaje: "Error Interno del Servidr" });
        }
    }
}