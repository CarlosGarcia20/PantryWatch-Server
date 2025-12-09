import express from 'express';
import cors from 'cors';
import { sensorRouter } from './src/routes/sensor.routes.js';
import { prologService } from './src/services/prologService.js';
import { productoRouter } from './src/routes/productos.routes.js';

// Inicializar Express
const app = express();
const PORT = 3000;

app.use(cors());
app.use(express.json());

// Rutas
app.use('/api', sensorRouter)
app.use('/api/productos', productoRouter);

// Arrancar servidor
app.listen(PORT, async() => {
    console.log(`🚀 Servidor corriendo en puerto ${PORT}`);

    await prologService.init();
});