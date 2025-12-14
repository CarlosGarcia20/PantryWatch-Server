import { GoogleGenerativeAI } from "@google/generative-ai";
import dotenv from 'dotenv';
import { chefModel } from "../models/chef.model.js";

dotenv.config();

// Inicializamos a Gemini
const genAI = new GoogleGenerativeAI(process.env.GEMINI_API_KEY);

export class chefController {
    static async sugerirReceta(req, res) {
        try {
            console.log("👨‍🍳 Consultando ingredientes...");

            const resultado = await chefModel.obtenerProductosDisponibles();
            
            if (!resultado.success) {
                return res.json({ 
                    mensaje: "No tienes ingredientes. Pide unos tacos 🌮" 
                });
            }

            console.log("❓ Preguntando a Gemini...");

            const ingredientes = resultado.data.map(p => p.nombre).join(', ');

            const model = genAI.getGenerativeModel({ model: "gemini-flash-latest"});

            const prompt = `
                Actúa como un Chef experto y sarcástico.
                Tengo estos ingredientes en mi alacena inteligente: ${ingredientes}.
                Recomiéndame UNA sola receta breve que pueda cocinar con esto (o usando básicos como sal/aceite).
                Dame el título, ingredientes y 3 pasos cortos.
            `;

            const result = await model.generateContent(prompt);
            const response = await result.response;
            const textoReceta = response.text();

            console.log("✨ Gemini respondió!");

            res.status(200).json({
                receta: textoReceta 
            });
        } catch (error) {
            res.status(500).json({
                error: "Error Interno del Servidor" 
            });
        }
    }
}