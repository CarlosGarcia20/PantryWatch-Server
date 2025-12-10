import express from 'express';
import cors from 'cors';
import http from 'http'
import { Server } from 'socket.io';
 
// Archivos de servicios
import { prologService } from './src/services/prologService.js';
// Arvhivos de rutas
import { sensorRouter } from './src/routes/sensor.routes.js';
import { productoRouter } from './src/routes/productos.routes.js';
import { contenedorRouter } from './src/routes/contenedor.routes.js';

// Inicializar Express
const app = express();
const PORT = 3000;

const server = http.createServer(app);
const io = new Server(server, {
    cors: {
        origin: '*',
        methods: ['GET', 'POST']
    }
});

app.use((req, res, next) => {
    req.io = io;
    next();
});

app.use(cors());
app.use(express.json());

// Rutas
app.use('/api', sensorRouter)
app.use('/api/productos', productoRouter);
app.use('/api/contenedor', contenedorRouter)


// Evento de conexión
io.on('connection', (socket) => {
    console.log('📱 App Móvil conectada al Socket: ' + socket.id);
    
    socket.on('disconnect', () => {
        console.log('📱 App desconectada');
    });
});


// Arrancar servidor
server.listen(PORT, '0.0.0.0', async() => {
    console.log(`🚀 Servidor corriendo en puerto ${PORT}`);

    await prologService.init();
});