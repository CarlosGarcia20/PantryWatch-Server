import { contenedorModel } from "../models/contenedor.model.js";
import { eliminarImagenContenedor, subirImagenContenedor } from "../services/imageService.js";

export class contenedorController {
    static async crear(req, res) {
        try {
            let urlImagen = null;

            if(req.file) {
                urlImagen = await subirImagenContenedor(req.file.path);
            }

            const result = await contenedorModel.crear({
                nombre: req.body.nombre,
                peso: req.body.peso,
                capacidadGr: req.body.capacidad_gr,
                imagen: urlImagen
            });
            
            if (!result.success) {
                return res.status(500).json({ mensaje: "Ocurrió un error al crear el contenedor" });
            }

            return res.status(201).json({ mensaje: "Contenedor creado con éxito" });
        } catch (error) {
            return res.status(500).json({ mensaje: "Error Interno del Servidor" });
        }
    }
    
    static async obtenerContenedores(req, res) {
        try {
            const result = await contenedorModel.obtenerContenedores();
            
            if (!result.success) {
                return res.status(404).json({ mensaje: "No hay contenedores disponibles" });
            }

            return res.status(200).json({ data: result.data });
        } catch (error) {
            return res.status(500).json({ mensaje: "Error Interno del Servidor" });
        }
    }
    
    static async editarContenedor(req, res) {
        try {
            const { contenedorId } = req.params;
            let nuevaImagen = null;

            if (req.file) {
                nuevaImagen = await subirImagenContenedor(req.file.path);
            }

            const result = await contenedorModel.editarContenedor({
                contenedorId: contenedorId,
                nombre: req.body.nombre || null,
                peso: req.body.peso || null,
                imagen: nuevaImagen || null,
                capacidad_gr: req.body.capacidad_gr || null
            });
            
            if (!result.success) {
                return res.status(500).json({ 
                    mensaje: "Ocurrió un error al actualizar el contenedor" 
                });
            }

            return res.status(200).json({ mensaje: "Contenedor actualizado correctamente" });
        } catch (error) {
            return res.status(500).json({ mensaje: "Error Interno del Servidor" });
        }
    }
    
    static async eliminarContenedor(req, res) {
        try {
            const { contenedorId } = req.params;

            const contenedor = await contenedorModel.obtenerContenedorPorId(contenedorId);

            if (!contenedor.success) {
                return res.status(404).json({ mensaje: "Contenedor no encontrado" });
            }

            if (contenedor.data.imagen) {
                await eliminarImagenContenedor(contenedor.data.imagen);
            }

            const resultado = await contenedorModel.eliminar(contenedorId);
            
            if(!resultado.success) {
                return res.status(500).json({ 
                    mensaje: "Ocurrió un error al eliminar el contenedor" 
                });
            }

            return res.sendStatus(204);
        } catch (error) {
            return res.status(500).json({ mensaje: "Error Interno del Servidor" });
        }
    }

    // static async registrarNuevoBote(req, res) {
    //     try {
    //         const { peso, zona } = req.body;

    //         console.log(`🆕 Nuevo Bote Detectado en ${zona}: ${peso}g`);

    //         if (req.io) {
    //             req.io.emit('nuevo_bote_detectado', { 
    //                 peso_detectado: peso,
    //                 zona: zona
    //             });
    //         }

    //         res.status(200).json({ 
    //             status: "EXITO",
    //             mensaje: "Recipiente registrado",
    //             accion: "LED_VERDE"
    //         });
    //     } catch (error) {
    //         console.error(error);
    //         res.status(500).json({ error: "Error al registrar bote" });
    //     }
    // }
}