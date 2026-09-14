# ============================================================#
#           INSTITUTO TECNOLÓGICO SUPERIOR PROGRESO           #
#                                                             #
#                          Carrera:                           #
#           Ingeniería en Sistemas Computacionales            #
#                                                             #
#                          Materia:                           #
#                   Lenguajes y automatas II                  #
#                                                             #
#                         Actividad:                          #
#          Practica 1: Analizador léxico y sintáctico         #
#                                                             #
#                         Docente:                            #
#               Ing. Gabriela Estefania Canul Sosa            #
#                                                             #
#                       Integrantes:                          #
#              David Ezequiel Caballero González              #
#               Shirley Janeth Silvestre Lopez                #
#                                                             #
#                  Fecha de entrega:                          #
#              Domingo 13 de septiembre de 2026               #
# ============================================================#

# Importar herramientas para crear el analizador léxico y sintáctico
import ply.lex as lex
import ply.yacc as yacc

# --- 1. ANALIZADOR LÉXICO (Tokens) ---

# Tokens basados en colores
tokens = (
    'LILA',       # Variables o identificadores
    'NARANJA',    # Números
    'SALMON',     # +
    'ROJO',       # -
    'MORADO',     # *
    'DORADO',     # /
    'ARCOIRIS',   # =
    'VERDE',      # (
    'AZUL',       # )
    'ROSADO',     # ^
    'AMARILLO'    # XOR
)

# Reglas de expresiones regulares para los símbolos
t_ARCOIRIS = r'='
t_VERDE    = r'\('
t_AZUL     = r'\)'
t_SALMON   = r'\+'
t_ROJO     = r'-'
t_MORADO   = r'\*'
t_DORADO   = r'/'
t_ROSADO   = r'\^'

# Regla para el operador lógico XOR (AMARILLO)
def t_AMARILLO(t):
    r'[xX][oO][rR]'
    return t

# Regla para variables/identificadores (LILA)
def t_LILA(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    return t

# Regla para números enteros y decimales (NARANJA)
def t_NARANJA(t):
    r'\d+(\.\d+)?'
    t.value = float(t.value) if '.' in t.value else int(t.value)
    return t

# Ignorar espacios y tabulaciones
t_ignore = " \t"

# Manejo de errores léxicos
def t_error(t):
    print(f"Carácter no válido '{t.value[0]}'")
    t.lexer.skip(1)

# Crear el analizador léxico
lexer = lex.lex()


# --- 2. CLASE NODO PARA CREAR LOS ÁRBOLES ---
class NodoAST:
    # Representa un nodo en el árbol de sintaxis abstracta (AST)
    def __init__(self, op, left=None, right=None, value=None, id_name=None):
        self.op = op
        self.left = left
        self.right = right
        self.value = value
        self.id_name = id_name

    # Evaluación del nodo según su tipo
    def evaluar(self):
        # Si el nodo es "NARANJA", devuelve su valor
        if self.op == 'NARANJA': return self.value
        # Si el nodo es "LILA", devuelve 0 (devuelve 0 porque no se ha implementado un entorno de variables)
        if self.op == 'LILA': return 0 
        # Si el nodo es una asignacion, evalúa el lado derecho
        if self.op == '=': return self.right.evaluar()
        # Si el nodo es "NEGRO", devuelve el valor
        if self.op == 'NEGRO': return -self.left.evaluar()

        # Si el nodo es una operación binaria, evalúa ambos lados
        if self.left and self.right:
            # Evaluar los subárboles
            l_val = self.left.evaluar()
            r_val = self.right.evaluar()
            # Si el nodo es una operación binaria, evalúa ambos lados
            if self.op == '+': return l_val + r_val
            if self.op == '-': return l_val - r_val
            if self.op == '*': return l_val * r_val
            if self.op == '/': return l_val / r_val
            if self.op == '^': return l_val ** r_val
            # Si el nodo es una operación XOR, entonces 
            if self.op.upper() == 'XOR': return int(l_val) ^ int(r_val)

    # Imprimir el árbol según el modo
    def imprimir(self, mode="expresion", prefix="", is_last=True, is_root=True):
        # Si el modo es "sintac"
        if mode == "sintactico":
            # Si el nodo es "NARANJA", se le pone la etiqueta NARANJA
            if self.op == 'NARANJA': etiqueta = f"NARANJA -> {self.value}"
            # Si el nodo es "LILA", se le pone la etiqueta LILA
            elif self.op == 'LILA': etiqueta = f"LILA -> {self.id_name}"
            # Si el nodo es una asignación, se le pone la etiqueta ARCOIRIS
            elif self.op == '=': etiqueta = "ARCOIRIS"
            # Si el nodo es una operación binaria, se le pone la etiqueta correspondiente
            elif self.op == '+': etiqueta = "SALMON"
            elif self.op == '-': etiqueta = "ROJO"
            elif self.op == '*': etiqueta = "MORADO"
            elif self.op == '/': etiqueta = "DORADO"
            elif self.op == '^': etiqueta = "ROSADO"
            # Si el nodo es una operación XOR, se le pone la etiqueta AMARILLO
            elif self.op == 'XOR': etiqueta = "AMARILLO"

        # Si el modo es "expresion"
        else:
            # Si el nodo es "NARANJA", se le pone la etiqueta num
            if self.op == 'NARANJA': etiqueta = f"num -> {self.value}"
            # Si el nodo es "LILA", se le pone la etiqueta id
            elif self.op == 'LILA': etiqueta = f"id -> {self.id_name}"
            else: etiqueta = self.op.lower()
 
        if is_root:
            print(etiqueta)
            new_prefix = ""
        else:
            marker = "└──> " if is_last else "├──> "
            print(prefix + marker + etiqueta)
            new_prefix = prefix + ("    " if is_last else "│   ")

        children = [c for c in (self.left, self.right) if c is not None]
        for i, child in enumerate(children):
            child.imprimir(mode, new_prefix, is_last=(i == len(children) - 1), is_root=False)


# --- 3. ANALIZADOR SINTÁCTICO (Gramática) ---

# Precedencia utilizando los tokens de colores
precedence = (
    ('left', 'AMARILLO'),        # XOR (Menor prioridad de las operaciones)
    ('left', 'SALMON', 'ROJO'),  # Suma (+) y Resta (-)
    ('left', 'MORADO', 'DORADO'),# Multiplicación (*) y División (/)
    ('right', 'ROSADO'),         # Potencia (^) (Se evalúa de derecha a izquierda)
    ('right', 'NEGRO'),          # Menos unario (Ej. -5) (Mayor prioridad)
)

# REGLA PRINCIPAL: Una instrucción válida puede ser una asignación (X = 5) o una simple expresión matemática (10+6)
def p_instruccion(p):
    '''instruccion : asignacion
                   | expresion'''
    nodo = p[1] # p[1] contiene el árbol completo generado por las reglas de abajo
    
    print("Árbol de expresión:")
    nodo.imprimir(mode="expresion") # Imprime la vista simplificada
    
    print("\nÁrbol sintáctico:")
    nodo.imprimir(mode="sintactico") # Imprime la vista con los tokens 

    # Evalúa matemáticamente el árbol recursivamente y muestra el resultado final
    print(f"\nEl valor de la expresión es: {nodo.evaluar()}")

# Regla de asignación
def p_asignacion(p):
    'asignacion : LILA ARCOIRIS expresion'
    # p[1] es la variable (LILA), p[2] es el igual (ARCOIRIS), p[3] es el valor calculado
    
    # Creamos un nodo terminal (hoja) exclusivo para guardar el nombre de la variable
    nodo_id = NodoAST('LILA', id_name=p[1])
    
    # Creamos el nodo raíz de la asignación (=). Su hijo izquierdo es la variable y el derecho la expresión matemática.
    p[0] = NodoAST('=', left=nodo_id, right=p[3])

# Reglas de operaciones
def p_expresion_binaria(p):
    '''expresion : expresion SALMON expresion
                 | expresion ROJO expresion
                 | expresion MORADO expresion
                 | expresion DORADO expresion
                 | expresion ROSADO expresion
                 | expresion AMARILLO expresion'''
    # Se crea un nuevo nodo padre que contiene la operación, enlazando a sus dos hijos
    p[0] = NodoAST(p[2], left=p[1], right=p[3])

# Regla para números negativos
def p_expresion_unaria(p):
    'expresion : ROJO expresion %prec NEGRO'
    # Usa '%prec NEGRO' para forzar que este operador tenga la mayor prioridad en la tabla 'precedence'
    p[0] = NodoAST('NEGRO', left=p[2])

# Regla para parentesis
def p_expresion_agrupacion(p):
    'expresion : VERDE expresion AZUL'

    # No se crea un nodo nuevo en el árbol para los paréntesis, ya que su única función 
    # fue forzar la prioridad gramatical. Simplemente pasamos el nodo interno (p[2]) hacia arriba.
    p[0] = p[2]

# Regla para números
def p_expresion_numero(p):
    'expresion : NARANJA'

    # Se crea un nodo sin hijos, almacenando únicamente su valor numérico (p[1])
    p[0] = NodoAST('NARANJA', value=p[1])

# Manejo de errores
def p_error(p):
    if p:
        print(f"Error sintáctico en '{p.value}'")
    else:
        print("Error sintáctico al final de la entrada")

# Crear el analizador de frases (parser)
parser = yacc.yacc()

# --- 4. MENÚ INTERACTIVO ---
def iniciar_menu():
    while True:
        print("\n" + "#" * 50)
        print("          ANALIZADOR LÉXICO Y SINTÁCTICO")
        print("#" * 50)
        print("1. Resolver Ejercicio 1: X = ((10+6)/(4*2))^2-(15/3)")
        print("2. Resolver Ejercicio 2: 10 XOR 12")
        print("3. Ingresar una expresión propia")
        print("4. Salir del programa")
        print("#" * 50)
        
        opcion = input("Selecciona una opción (1-4): ")
        
        
        if opcion == '1':
            ejercicio = "X = ((10+6)/(4*2))^2-(15/3)"
            print("#" * 50)
            print(f"\nAnalizando instrucción: {ejercicio}")
            parser.parse(ejercicio)
            
        elif opcion == '2':
            ejercicio = "10 XOR 12"
            print("#" * 50)
            print(f"\nAnalizando instrucción: {ejercicio}")
            parser.parse(ejercicio)
            
        elif opcion == '3':
            ejercicio = input("\nEscribe tu expresión: ")
            if ejercicio.strip():
                print(f"\nAnalizando instrucción: {ejercicio}")
                parser.parse(ejercicio.strip())
            else:
                print("\nNo ingresaste ninguna expresión válida.")
                
        elif opcion == '4':
            print("\nCerrando el analizador... ¡Hasta pronto!")
            break
            
        else:
            print("\nOpción no válida. Por favor elige un número del 1 al 4.")

# Iniciar el programa
if __name__ == '__main__':
    iniciar_menu()