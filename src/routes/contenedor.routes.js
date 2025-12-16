import { Router } from "express";
import multer from "multer";
import { contenedorController } from "../controllers/contenedor.controller.js";
import { chefController } from "../controllers/chef.controller.js";

const upload = multer({ dest: 'uploads/' });

export const contenedorRouter = Router();

contenedorRouter.get('/', contenedorController.obtenerContenedores);
contenedorRouter.post('/', upload.single('imagen'), contenedorController.crear)
contenedorRouter.put('/:contenedorId', upload.single('imagen'), contenedorController.editarContenedor);
contenedorRouter.delete('/:contenedorId', contenedorController.eliminarContenedor)

// contenedorRouter.post('/recipientes', contenedorController.registrarNuevoBote);

contenedorRouter.get('/chef', chefController.sugerirReceta);