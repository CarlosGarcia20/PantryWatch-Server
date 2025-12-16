import { productoModelo } from "../models/producto.model.js";
import { prologService } from "../services/prologService.js";

export class productoController {
    static async abrirModalFront(req, res) {
        if(req.io) {
            req.io.emit('nuevo_producto', { 
                text: "Nuevo producto detectado"
            });

            return res.status(200).json({ mensaje: "Okey" })
        }
    }

    static async crear(req, res) {
        try {
            const resultado = await productoModelo.crear({
                nombre: req.body.nombre,
                contenedorId: req.body.contenedorId || null,
                pesoUnitario: req.body.pesoUnitario || null,
                pesoActual: req.body.pesoActual || null,
                stockMinimo: req.body.stockMinimo,
                stockActual: req.body.stockActual,
                tempMax: req.body.tempMax,
                humedadMax: req.body.humedadMax
            });
            console.log(resultado);
            
            if(!resultado.success) {
                return res.status(500).json({ mensaje: "Ocurrió un error al crear el producto" });
            }

            console.log("✨ Producto guardado en BD. Avisando a la IA...");
        
            await prologService.recargarConocimiento();

            return res.status(201).json({ mensaje: "Producto creado exitosamente" });
        } catch (error) {
            console.log(error);
            
            return res.status(500).json({ mensaje: "Error Interno del Servidor" });
        }
    }
    
    static async editarProducto(req, res) {
        const { productoId } = req.params;
        
        try {
            const resultado = await productoModelo.editarProducto({
                productoId,
                nombre: req.body.nombre,
                contenedorId: req.body.contenedorId || null,
                pesoUnitario: req.body.pesoUnitario,
                pesoActual: req.body.pesoActual,
                stockMinimo: req.body.stockMinimo,
                stockActual: req.body.stockActual,
                tempMax: req.body.tempMax,
                humedadMax: req.body.humedadMax
            });
            
            if (!resultado.success) {
                return res.status(500).json({ mensaje: "Error al actualizar el producto" });    
            }

            console.log("✨ Producto modificado en BD. Avisando a la IA...");
        
            await prologService.recargarConocimiento();
            
            return res.status(200).json({ mensaje: "Producto actualizado correctamente" });
        } catch (error) {
            return res.status(500).json({ mensaje: "Error Interno del Servidor" });
        }
    }
    
    static async obtenerProductos(req, res) {
        try {
            const resultado = await productoModelo.obtenerProductos();

            if (!resultado.success) {
                return res.status(404).json({ mensaje: "No hay productos registrados" })
            }

            return res.status(200).json({ data: resultado.data });
        } catch (error) {
            return res.status(500).json({ mensaje: "Error Interno del Servidor" });
        }
    }
    
    static async eliminarProducto(req, res) {
        const { productoId } = req.params;
        try {
            const resultado = await productoModelo.eliminarProducto(productoId);

            if (!resultado.success) {
                return res.status(404).json({ mensaje: "No se ha encontrado el producto" });
            }

            console.log("✨ Producto eliminado en BD. Avisando a la IA...");
        
            await prologService.recargarConocimiento();

            return res.sendStatus(204);
        } catch (error) {
            return res.status(500).json({ mensaje: "Error Interno del Servidor" });
        }
    }
}