import java_cup.runtime.*;
import java.util.*;

%%

%class Lexer
%unicode
%line
%column
%cup

%{
    private final ArrayDeque<Integer> indents = new ArrayDeque<Integer>();
    private boolean necesitaNL = false;

    { indents.push(0); }

    private Symbol tk(int tipo) {
        return tk(tipo, null);
    }

    private Symbol tk(int tipo, Object valor) {
        necesitaNL = true;
        return new Symbol(tipo, yyline + 1, yycolumn + 1, valor);
    }

    private Symbol especial(int tipo) {
        return new Symbol(tipo, yyline + 1, yycolumn + 1);
    }
%}

%state CODIGO

NL  = \r\n|\n|\r
ID  = [A-Za-z_][A-Za-z_0-9]*
NUM = [0-9]+(\.[0-9]+)?

%%

<YYINITIAL> {
    [ \t]*{NL}          { /* linea en blanco */ }
    [ \t]*"#"[^\r\n]*   { /* linea solo con comentario */ }
    [ \t]*[^ \t\r\n#]   {
        String s = yytext();
        int col = 0;
        for (int i = 0; i < s.length() - 1; i++) {
            col += (s.charAt(i) == '\t') ? 4 : 1;
        }
        int tope = indents.peek();
        if (col > tope) {
            yypushback(1);
            indents.push(col);
            yybegin(CODIGO);
            return especial(sym.INDENT);
        } else if (col < tope) {
            indents.pop();
            if (col > indents.peek()) {
                throw new Error("Sangria inconsistente en la linea " + (yyline + 1));
            }
            yypushback(yylength());
            return especial(sym.DEDENT);
        } else {
            yypushback(1);
            yybegin(CODIGO);
        }
    }
    [ \t]+              { }
}

<CODIGO> {
    {NL}            { yybegin(YYINITIAL); necesitaNL = false; return especial(sym.NEWLINE); }
    [ \t]+          { }
    "#"[^\r\n]*     { }

    "def"           { return tk(sym.DEF); }
    "if"            { return tk(sym.IF); }
    "elif"          { return tk(sym.ELIF); }
    "else"          { return tk(sym.ELSE); }
    "return"        { return tk(sym.RETURN); }
    "print"         { return tk(sym.PRINT); }
    "or"            { return tk(sym.OR); }

    "=="            { return tk(sym.EQ); }
    "!="            { return tk(sym.NE); }
    "<="            { return tk(sym.LE); }
    ">="            { return tk(sym.GE); }
    "<"             { return tk(sym.LT); }
    ">"             { return tk(sym.GT); }
    "="             { return tk(sym.ASIGNA); }
    "+"             { return tk(sym.MAS); }
    "-"             { return tk(sym.MENOS); }
    "**"            { return tk(sym.POT); }
    "*"             { return tk(sym.POR); }
    "/"             { return tk(sym.DIV); }
    "^"             { return tk(sym.POT); }
    "("             { return tk(sym.LPAREN); }
    ")"             { return tk(sym.RPAREN); }
    ","             { return tk(sym.COMA); }
    ":"             { return tk(sym.DOSP); }

    {ID}            { return tk(sym.ID, yytext()); }
    {NUM}           { return tk(sym.NUM, yytext()); }

    [^]             { throw new Error("Caracter no valido '" + yytext() + "' en la linea " + (yyline + 1)); }
}

<<EOF>> {
    if (necesitaNL) {
        necesitaNL = false;
        return especial(sym.NEWLINE);
    }
    if (indents.size() > 1) {
        indents.pop();
        return especial(sym.DEDENT);
    }
    return new Symbol(sym.EOF);
}