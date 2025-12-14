import pool from "../config/db.js"

export class chefModel {
    static async obtenerProductosDisponibles() {
        try {
            const { rows } = await pool.query(
                `SELECT 
                    nombre 
                FROM productos 
                WHERE peso_actual > 50`
            );

            if (rows.length < 0) return { success: false }

            return { success: true, data: rows }
        } catch (error) {
            return { success: false, error }
        }
    }
}