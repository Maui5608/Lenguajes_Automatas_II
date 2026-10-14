import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.*;

public class TAC {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        if (args.length > 0) {
            // Modo archivo original (para compilar factorial.code)
            ejecutarArchivo(args[0]);
            return;
        }

        // Menú interactivo en terminal
        while (true) {
            System.out.println("\n=== COMPILADOR INTERMEDIO - JAVA ===");
            System.out.println("1. Compilar archivo por defecto (factorial.code)");
            System.out.println("2. Ingresar una expresion aritmetica (Polaca, 3 Direcciones y Cuadrupletas)");
            System.out.println("3. Salir");
            System.out.print("Elige una opcion: ");
            
            String opcion = scanner.nextLine().trim();

            if (opcion.equals("1")) {
                ejecutarArchivo("factorial.code");
            } else if (opcion.equals("2")) {
                System.out.print("Escribe la expresion (ej. a+b*c+d o x = a+b): ");
                String expr = scanner.nextLine().trim();
                if (!expr.isEmpty()) {
                    procesarExpresionInteractiva(expr);
                }
            } else if (opcion.equals("3")) {
                System.out.println("Hasta luego.");
                break;
            } else {
                System.out.println("Opcion no valida, intenta de nuevo.");
            }
        }
    }

    private static void ejecutarArchivo(String nombreArchivo) {
        Parser.limpiarTodo();
        try (Reader r = new InputStreamReader(new FileInputStream(nombreArchivo), StandardCharsets.UTF_8)) {
            Parser p = new Parser(new Lexer(r));
            p.parse();
        } catch (Throwable e) {
            System.err.println("ERROR: " + e.getMessage());
            return;
        }

        List<String> codigo = Parser.resultadoFinal();
        System.out.println("\n--- CODIGO TRES DIRECCIONES GENERADO ---");
        for (String linea : codigo) {
            System.out.println(linea);
        }
        try {
            Files.write(Paths.get("factorial.txt"), codigo, StandardCharsets.UTF_8);
            System.out.println("\n[Guardado exitosamente en factorial.txt]");
        } catch (IOException e) {
            System.err.println("No se pudo guardar factorial.txt: " + e.getMessage());
        }
    }

    private static void procesarExpresionInteractiva(String expresion) {
        Parser.limpiarTodo();
        String inputStr = expresion.contains("=") ? expresion + "\n" : "res = " + expresion + "\n";
        
        try {
            Reader r = new StringReader(inputStr);
            Parser p = new Parser(new Lexer(r));
            p.parse();
        } catch (Throwable e) {
            System.err.println("ERROR al procesar expresion: " + e.getMessage());
            return;
        }

        System.out.println("\n" + "============================================================");
        System.out.println("EXPRESION: " + expresion);
        System.out.println("============================================================");
        
        System.out.println("\n[ POLACA INVERSA ]");
        System.out.print("  ");
        for (String s : Parser.polacaSalida) {
            if (!s.equals("=")) {
                System.out.print(s + " ");
            }
        }
        System.out.println();

        System.out.println("\n[ TRES DIRECCIONES ]");
        for (String linea : Parser.resultadoFinal()) {
            System.out.println("  " + linea);
        }

        System.out.println("\n[ CUADRUPLETAS ]");
        System.out.println("  (OP, ARG1, ARG2, RESULTADO)");
        for (String[] cuad : Parser.cuadrupletasSalida) {
            String arg2 = (cuad[2] == null || cuad[2].isEmpty()) ? " " : cuad[2];
            System.out.println("  (" + cuad[0] + ", " + cuad[1] + ", " + arg2 + ", " + cuad[3] + ")");
        }
        System.out.println();
    }
}