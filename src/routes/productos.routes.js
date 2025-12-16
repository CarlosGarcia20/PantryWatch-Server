import { Router } from "express";
import { productoController } from "../controllers/producto.controller.js";

export const productoRouter = Router();

productoRouter.post('/modal', productoController.abrirModalFront);

productoRouter.post('/', productoController.crear);

productoRouter.put('/:productoId', productoController.editarProducto);
productoRouter.get('/', productoController.obtenerProductos);
productoRouter.delete('/:productoId', productoController.eliminarProducto);