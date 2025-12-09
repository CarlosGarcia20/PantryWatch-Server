import pool from "../config/db.js";

export class productoModelo {
    static async crear() {
        try {
            const { rows } = await pool.query(
                `INSERT INTO `
            );
            
            return { success: true, data: rows }
        } catch (error) {
            return { success: false, error }
        }
    }
}