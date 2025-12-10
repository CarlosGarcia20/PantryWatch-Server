import { Router } from "express";
import multer from "multer";
import { contenedorController } from "../controllers/contenedor.controller.js";

const upload = multer({ dest: 'uploads/' });

export const contenedorRouter = Router();

contenedorRouter.get('/', contenedorController.obtenerContenedores);
contenedorRouter.post('/', upload.single('imagen'), contenedorController.crear)
contenedorRouter.put('/:contenedorId', upload.single('imagen'), contenedorController.editarContenedor);
contenedorRouter.delete('/:contenedorId', contenedorController.eliminarContenedor)

contenedorRouter.post('/recipientes', contenedorController.registrarNuevoBote);