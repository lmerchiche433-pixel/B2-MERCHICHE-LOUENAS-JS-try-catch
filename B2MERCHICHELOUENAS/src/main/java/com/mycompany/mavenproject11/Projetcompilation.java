/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Main.java to edit this template
 */
package javaapplication5;
import java.util.ArrayList;
import javax.swing.*;
import java.awt.*;


import java.util.ArrayList;


import java.io.*;
import javax.swing.border.BevelBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableModel;


public class Projetcompilation { 
    
    enum TokenType {
        DEBUGGER, DEFAULT, DELETE, EXTENDS, FINALLY, IMPORT, IN, INSTANCEOF,
        NEW, SUPER, THIS, THROW, TYPEOF, VAR, VOID, WITH, YIELD, LET, CONST, 
        FUNCTION, CLASS, IF, ELSE, TRY, CATCH, FOR, WHILE, DO, SWITCH, CASE,
        BREAK, CONTINUE, RETURN, TRUE, FALSE, NULL, EOF, ERROR, IDENTIFIER,
        NUMBER, STRING, PLUS, MINUS, STAR, SLASH, PERCENT, PLUSPLUS, MINUSMINUS,
        ASSIGN, PLUS_ASSIGN, MINUS_ASSIGN, STAR_ASSIGN, SLASH_ASSIGN, PERCENT_ASSIGN,
        EQUAL, STRICT_EQUAL, NOTEQUAL, STRICT_NOT_EQUAL, LT, GT, LE, GE,
        AND, OR, NOT, BIT_AND, BIT_OR, BIT_XOR, BIT_NOT, SHIFT_LEFT, SHIFT_RIGHT,
        UNSIGNED_SHIFT_RIGHT, QUESTION, COLON, ARROW, SEMICOLON, COMMA, DOT,
        LPAREN, RPAREN, LBRACE, RBRACE, LBRACKET, RBRACKET
    }
    
    static class Token {
        TokenType type;
        String lexeme;
        int line, col;
        Token(TokenType t, String lex, int l, int c) {
            type = t; lexeme = lex; line = l; col = c;
        }
    }
    
    static class ErrorHandler {
        private ArrayList<String> errors = new ArrayList<>();
        private ArrayList<String> syntaxMessages = new ArrayList<>();
        private int lexicalErrors = 0;
        private int syntaxErrors = 0;
        
        public void reportLexicalError(String message, int line, int col, char character) {
            lexicalErrors++;
            errors.add("ERREUR LEXICALE [Ligne " + line + ", Col " + col + "]: " + 
                      message + " (caractere: '" + character + "')");
        }
        
        public void reportSyntaxError(String message, int line, int col, String found) {
            syntaxErrors++;
            errors.add("ERREUR SYNTAXIQUE [Ligne " + line + ", Col " + col + "]: " + 
                      message + " (trouve: '" + found + "')");
        }
        
        public void addSyntaxMessage(String message) {
            syntaxMessages.add(message);
        }
        
        public int getTotalErrors() { return lexicalErrors + syntaxErrors; }
        public int getLexicalErrors() { return lexicalErrors; }
        public int getSyntaxErrors() { return syntaxErrors; }
        public ArrayList<String> getErrors() { return errors; }
        public ArrayList<String> getSyntaxMessages() { return syntaxMessages; }
        public void clear() { 
            errors.clear(); 
            syntaxMessages.clear();
            lexicalErrors = 0; 
            syntaxErrors = 0; 
        }
    }
    
   
    static class Lexer {
        private final String source;
        private final int sourceLength;
        private int index = 0, line = 1, col = 1;
        private ErrorHandler errorHandler;
        
        public Lexer(String source, ErrorHandler errorHandler) {
            this.source = source + "#";
            this.sourceLength = this.source.length();
            this.errorHandler = errorHandler;
        }
        
        private char currentChar() { 
            if (index >= sourceLength) return '#';
            return source.charAt(index); 
        }
        
        private char nextChar() { 
            if (index + 1 >= sourceLength) return '#';
            return source.charAt(index + 1); 
        }
        
        private boolean isEOF() { 
            return index >= sourceLength - 1 || currentChar() == '#'; 
        }
        
        private boolean isLetter(char c) { 
            return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '$'; 
        }
        
        private boolean isDigit(char c) { 
            return c >= '0' && c <= '9'; 
        }
        
        private void advance() {
            char c = source.charAt(index);
            index++;
            if (c == '\n') { line++; col = 1; }
            else { col++; }
        }
        
        private void skipWhitespace() {
            char c = currentChar();
            if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
                advance();
                skipWhitespace();
            }
        }
        
        private void skipComment() {
            if (currentChar() == '/' && nextChar() == '/') {
                advance(); advance();
                skipLineComment();
            } else if (currentChar() == '/' && nextChar() == '*') {
                advance(); advance();
                skipBlockComment();
            }
        }
        
        private void skipLineComment() {
            if (currentChar() != '\n' && currentChar() != '#') {
                advance();
                skipLineComment();
            }
        }
        
        private void skipBlockComment() {
            if (currentChar() == '#') return;
            if (currentChar() == '*' && nextChar() == '/') {
                advance(); advance();
                return;
            }
            advance();
            skipBlockComment();
        }
        
        private TokenType getKeywordType(String word) {
            if (word.equals("debugger")) return TokenType.DEBUGGER;
            if (word.equals("default")) return TokenType.DEFAULT;
            if (word.equals("delete")) return TokenType.DELETE;
            if (word.equals("extends")) return TokenType.EXTENDS;
            if (word.equals("finally")) return TokenType.FINALLY;
            if (word.equals("import")) return TokenType.IMPORT;
            if (word.equals("in")) return TokenType.IN;
            if (word.equals("instanceof")) return TokenType.INSTANCEOF;
            if (word.equals("new")) return TokenType.NEW;
            if (word.equals("super")) return TokenType.SUPER;
            if (word.equals("this")) return TokenType.THIS;
            if (word.equals("throw")) return TokenType.THROW;
            if (word.equals("typeof")) return TokenType.TYPEOF;
            if (word.equals("var")) return TokenType.VAR;
            if (word.equals("void")) return TokenType.VOID;
            if (word.equals("with")) return TokenType.WITH;
            if (word.equals("yield")) return TokenType.YIELD;
            if (word.equals("let")) return TokenType.LET;
            if (word.equals("const")) return TokenType.CONST;
            if (word.equals("function")) return TokenType.FUNCTION;
            if (word.equals("class")) return TokenType.CLASS;
            if (word.equals("if")) return TokenType.IF;
            if (word.equals("else")) return TokenType.ELSE;
            if (word.equals("try")) return TokenType.TRY;
            if (word.equals("catch")) return TokenType.CATCH;
            if (word.equals("for")) return TokenType.FOR;
            if (word.equals("while")) return TokenType.WHILE;
            if (word.equals("do")) return TokenType.DO;
            if (word.equals("switch")) return TokenType.SWITCH;
            if (word.equals("case")) return TokenType.CASE;
            if (word.equals("break")) return TokenType.BREAK;
            if (word.equals("continue")) return TokenType.CONTINUE;
            if (word.equals("return")) return TokenType.RETURN;
            if (word.equals("true")) return TokenType.TRUE;
            if (word.equals("false")) return TokenType.FALSE;
            if (word.equals("null")) return TokenType.NULL;
            return TokenType.IDENTIFIER;
        }
        
        private String readIdentifierRec(String acc) {
            if (isLetter(currentChar()) || isDigit(currentChar())) {
                char c = currentChar();
                advance();
                return readIdentifierRec(acc + c);
            }
            return acc;
        }
        
        private Token readIdentifierOrKeyword() {
            int startLine = line, startCol = col;
            String lexeme = readIdentifierRec("");
            return new Token(getKeywordType(lexeme), lexeme, startLine, startCol);
        }
        
        private String readDigitsRec(String acc) {
            if (isDigit(currentChar())) {
                char c = currentChar();
                advance();
                return readDigitsRec(acc + c);
            }
            return acc;
        }
        
        private Token readNumber() {
            int startLine = line, startCol = col;
            String lexeme = readDigitsRec("");
            
            if (currentChar() == '.' && isDigit(nextChar())) {
                lexeme += currentChar();
                advance();
                lexeme = lexeme + readDigitsRec("");
            }
            
            if (isLetter(currentChar())) {
                String invalid = readIdentifierRec(lexeme);
                errorHandler.reportLexicalError("Identificateur invalide commencant par un chiffre", 
                                               startLine, startCol, invalid.charAt(0));
                return new Token(TokenType.ERROR, invalid, startLine, startCol);
            }
            
            return new Token(TokenType.NUMBER, lexeme, startLine, startCol);
        }
        
        private String readStringRec(char quote, String acc, int startLine, int startCol) {
            if (currentChar() == quote) {
                return acc + currentChar();
            }
            if (currentChar() == '#') {
                errorHandler.reportLexicalError("Chaine de caracteres non fermee (fin de fichier)", 
                                               startLine, startCol, quote);
                return acc;
            }
            if (currentChar() == '\n') {
                errorHandler.reportLexicalError("Chaine de caracteres non fermee", 
                                               startLine, startCol, quote);
                return acc;
            }
            if (currentChar() == '\\' && nextChar() != '#') {
                char c1 = currentChar();
                advance();
                char c2 = currentChar();
                advance();
                return readStringRec(quote, acc + c1 + c2, startLine, startCol);
            }
            char c = currentChar();
            advance();
            return readStringRec(quote, acc + c, startLine, startCol);
        }
        
        private Token readString() {
            int startLine = line, startCol = col;
            char quote = currentChar();
            advance();
            String lexeme = "" + quote + readStringRec(quote, "", startLine, startCol);
            if (currentChar() == quote) advance();
            return new Token(TokenType.STRING, lexeme, startLine, startCol);
        }
        
        public Token nextToken() {
            while (currentChar() != '#') {
                skipWhitespace();
                if (currentChar() == '/' && (nextChar() == '/' || nextChar() == '*')) {
                    skipComment();
                } else {
                    break;
                }
            }
            
            if (isEOF()) return new Token(TokenType.EOF, "", line, col);
            
            int startLine = line, startCol = col;
            char c = currentChar();
            
            if (isLetter(c)) return readIdentifierOrKeyword();
            if (isDigit(c)) return readNumber();
            if (c == '"' || c == '\'') return readString();
            
            advance();
            
            if (c == '+') {
                if (currentChar() == '+') { advance(); return new Token(TokenType.PLUSPLUS, "++", startLine, startCol); }
                if (currentChar() == '=') { advance(); return new Token(TokenType.PLUS_ASSIGN, "+=", startLine, startCol); }
                return new Token(TokenType.PLUS, "+", startLine, startCol);
            }
            if (c == '-') {
                if (currentChar() == '-') { advance(); return new Token(TokenType.MINUSMINUS, "--", startLine, startCol); }
                if (currentChar() == '=') { advance(); return new Token(TokenType.MINUS_ASSIGN, "-=", startLine, startCol); }
                return new Token(TokenType.MINUS, "-", startLine, startCol);
            }
            if (c == '*') {
                if (currentChar() == '=') { advance(); return new Token(TokenType.STAR_ASSIGN, "*=", startLine, startCol); }
                return new Token(TokenType.STAR, "*", startLine, startCol);
            }
            if (c == '/') {
                if (currentChar() == '=') { advance(); return new Token(TokenType.SLASH_ASSIGN, "/=", startLine, startCol); }
                return new Token(TokenType.SLASH, "/", startLine, startCol);
            }
            if (c == '%') {
                if (currentChar() == '=') { advance(); return new Token(TokenType.PERCENT_ASSIGN, "%=", startLine, startCol); }
                return new Token(TokenType.PERCENT, "%", startLine, startCol);
            }
            if (c == '=') {
                if (currentChar() == '=') {
                    advance();
                    if (currentChar() == '=') { advance(); return new Token(TokenType.STRICT_EQUAL, "===", startLine, startCol); }
                    return new Token(TokenType.EQUAL, "==", startLine, startCol);
                }
                if (currentChar() == '>') { advance(); return new Token(TokenType.ARROW, "=>", startLine, startCol); }
                return new Token(TokenType.ASSIGN, "=", startLine, startCol);
            }
            if (c == '!') {
                if (currentChar() == '=') {
                    advance();
                    if (currentChar() == '=') { advance(); return new Token(TokenType.STRICT_NOT_EQUAL, "!==", startLine, startCol); }
                    return new Token(TokenType.NOTEQUAL, "!=", startLine, startCol);
                }
                return new Token(TokenType.NOT, "!", startLine, startCol);
            }
            if (c == '<') {
                if (currentChar() == '=') { advance(); return new Token(TokenType.LE, "<=", startLine, startCol); }
                if (currentChar() == '<') { advance(); return new Token(TokenType.SHIFT_LEFT, "<<", startLine, startCol); }
                return new Token(TokenType.LT, "<", startLine, startCol);
            }
            if (c == '>') {
                if (currentChar() == '=') { advance(); return new Token(TokenType.GE, ">=", startLine, startCol); }
                if (currentChar() == '>') {
                    advance();
                    if (currentChar() == '>') { advance(); return new Token(TokenType.UNSIGNED_SHIFT_RIGHT, ">>>", startLine, startCol); }
                    return new Token(TokenType.SHIFT_RIGHT, ">>", startLine, startCol);
                }
                return new Token(TokenType.GT, ">", startLine, startCol);
            }
            if (c == '&') {
                if (currentChar() == '&') { advance(); return new Token(TokenType.AND, "&&", startLine, startCol); }
                return new Token(TokenType.BIT_AND, "&", startLine, startCol);
            }
            if (c == '|') {
                if (currentChar() == '|') { advance(); return new Token(TokenType.OR, "||", startLine, startCol); }
                return new Token(TokenType.BIT_OR, "|", startLine, startCol);
            }
            if (c == '^') return new Token(TokenType.BIT_XOR, "^", startLine, startCol);
            if (c == '~') return new Token(TokenType.BIT_NOT, "~", startLine, startCol);
            if (c == '?') return new Token(TokenType.QUESTION, "?", startLine, startCol);
            if (c == ':') return new Token(TokenType.COLON, ":", startLine, startCol);
            if (c == ';') return new Token(TokenType.SEMICOLON, ";", startLine, startCol);
            if (c == ',') return new Token(TokenType.COMMA, ",", startLine, startCol);
            if (c == '.') return new Token(TokenType.DOT, ".", startLine, startCol);
            if (c == '(') return new Token(TokenType.LPAREN, "(", startLine, startCol);
            if (c == ')') return new Token(TokenType.RPAREN, ")", startLine, startCol);
            if (c == '{') return new Token(TokenType.LBRACE, "{", startLine, startCol);
            if (c == '}') return new Token(TokenType.RBRACE, "}", startLine, startCol);
            if (c == '[') return new Token(TokenType.LBRACKET, "[", startLine, startCol);
            if (c == ']') return new Token(TokenType.RBRACKET, "]", startLine, startCol);
            
            errorHandler.reportLexicalError("Caractere non reconnu", startLine, startCol, c);
            return new Token(TokenType.ERROR, "" + c, startLine, startCol);
        }
    }



    static class Parser {
        private Lexer lexer;
        private Token currentToken;
        private ErrorHandler errorHandler;
        
        public Parser(Lexer lexer, ErrorHandler errorHandler) {
            this.lexer = lexer;
            this.errorHandler = errorHandler;
            this.currentToken = lexer.nextToken();
        }
        
        private void advance() { currentToken = lexer.nextToken(); }
        
        private boolean match(TokenType... types) {
            for (int i = 0; i < types.length; i++) {
                if (currentToken.type == types[i]) return true;
            }
            return false;
        }
        
        private boolean expect(TokenType type) {
            if (currentToken.type != type) {
                errorHandler.reportSyntaxError("Attendu " + type, currentToken.line, currentToken.col, currentToken.lexeme);
                return false;
            }
            advance();
            return true;
        }
        
        private void synchronize() {
            advance();
            while (currentToken.type != TokenType.EOF) {
                if (match(TokenType.SEMICOLON)) { advance(); return; }
                if (match(TokenType.CLASS, TokenType.FUNCTION, TokenType.VAR, 
                         TokenType.LET, TokenType.CONST, TokenType.TRY)) return;
                advance();
            }
        }
        
        public void parse() {
            errorHandler.addSyntaxMessage("=== DEBUT DE L'ANALYSE SYNTAXIQUE ===\n");
            while (currentToken.type != TokenType.EOF) {
                parseStatement();
            }
            errorHandler.addSyntaxMessage("\n=== FIN DE L'ANALYSE SYNTAXIQUE ===");
        }
        
        private void parseStatement() {
            // Ignorer les tokens d'erreur
            if (currentToken.type == TokenType.ERROR) {
                advance();
                return;
            }
            
            // Declarations de variables
            if (match(TokenType.VAR, TokenType.LET, TokenType.CONST)) {
                parseVarDeclaration();
                return;
            }
            
            // Declaration de fonction
            if (match(TokenType.FUNCTION)) {
                parseFunctionDeclaration();
                return;
            }
            
            // Declaration de classe
            if (match(TokenType.CLASS)) {
                parseClassDeclaration();
                return;
            }
            
            // Try-Catch
            if (match(TokenType.TRY)) {
                parseTryCatch();
                return;
            }
            
            // Affectation : IDENTIFIER (=|+=|-=|...) Expression ;
            if (match(TokenType.IDENTIFIER)) {
                String name = currentToken.lexeme;
                int line = currentToken.line;
                advance();
                
                // Doit etre suivi d'un operateur d'affectation
                if (match(TokenType.ASSIGN, TokenType.PLUS_ASSIGN, TokenType.MINUS_ASSIGN,
                         TokenType.STAR_ASSIGN, TokenType.SLASH_ASSIGN, TokenType.PERCENT_ASSIGN)) {
                    String op = currentToken.lexeme;
                    advance();
                    errorHandler.addSyntaxMessage("✓ Affectation: " + name + " " + op + " <expression> (ligne " + line + ")");
                    parseExpression();
                    if (!match(TokenType.SEMICOLON)) {
                        errorHandler.reportSyntaxError("Point-virgule attendu apres l'affectation", 
                                                      currentToken.line, currentToken.col, currentToken.lexeme);
                    } else {
                        advance();
                    }
                    return;
                } else {
                    // ERREUR: identificateur seul non valide
                    errorHandler.reportSyntaxError("Instruction invalide: identificateur seul '" + name + "' sans affectation", 
                                                  line, currentToken.col, currentToken.lexeme);
                    skipUntilSemicolon();
                    return;
                }
            }
            
            // Instruction vide (juste un point-virgule)
            if (match(TokenType.SEMICOLON)) {
                advance();
                return;
            }
            
            // Instructions non reconnues = ERREUR
            errorHandler.reportSyntaxError("Instruction invalide ou non reconnue", 
                                          currentToken.line, currentToken.col, currentToken.lexeme);
            skipUntilSemicolon();
        }
        
        private void parseVarDeclaration() {
            String varType = currentToken.lexeme;
            int line = currentToken.line;
            advance();
            
            if (!match(TokenType.IDENTIFIER)) {
                errorHandler.reportSyntaxError("Nom de variable attendu apres " + varType, 
                                              currentToken.line, currentToken.col, currentToken.lexeme);
                synchronize();
                return;
            }
            
            String name = currentToken.lexeme;
            advance();
            
            // CONST doit avoir une initialisation
            if (varType.equals("const")) {
                if (!match(TokenType.ASSIGN)) {
                    errorHandler.reportSyntaxError("const doit etre initialise", 
                                                  line, currentToken.col, name);
                    skipUntilSemicolon();
                    return;
                }
                advance();
                parseExpression();
                errorHandler.addSyntaxMessage("✓ Declaration de constante: " + varType + " " + name + " = <expression> (ligne " + line + ")");
            } else {
                // let et var peuvent ne pas avoir d'initialisation
                if (match(TokenType.ASSIGN)) {
                    advance();
                    parseExpression();
                    errorHandler.addSyntaxMessage("✓ Declaration de variable: " + varType + " " + name + " = <expression> (ligne " + line + ")");
                } else {
                    errorHandler.addSyntaxMessage("✓ Declaration de variable: " + varType + " " + name + " (ligne " + line + ")");
                }
            }
            
            if (!match(TokenType.SEMICOLON)) {
                errorHandler.reportSyntaxError("Point-virgule attendu apres la declaration", 
                                              currentToken.line, currentToken.col, currentToken.lexeme);
            } else {
                advance();
            }
        }
        
        private void parseFunctionDeclaration() {
            int line = currentToken.line;
            advance();
            
            if (!match(TokenType.IDENTIFIER)) {
                errorHandler.reportSyntaxError("Nom de fonction attendu", currentToken.line, currentToken.col, currentToken.lexeme);
                synchronize();
                return;
            }
            
            String name = currentToken.lexeme;
            advance();
            
            if (!expect(TokenType.LPAREN)) {
                synchronize();
                return;
            }
            
            int paramCount = 0;
            while (!match(TokenType.RPAREN) && currentToken.type != TokenType.EOF) {
                if (!match(TokenType.IDENTIFIER)) {
                    errorHandler.reportSyntaxError("Parametre attendu", currentToken.line, currentToken.col, currentToken.lexeme);
                    advance();
                    continue;
                }
                paramCount++;
                advance();
                if (match(TokenType.COMMA)) advance();
            }
            
            expect(TokenType.RPAREN);
            errorHandler.addSyntaxMessage("✓ Declaration de fonction: " + name + " (" + paramCount + " parametre(s)) (ligne " + line + ")");
            
            if (!expect(TokenType.LBRACE)) {
                synchronize();
                return;
            }
            
            parseBlock();
            
            if (!expect(TokenType.RBRACE)) {
                while (!match(TokenType.RBRACE) && currentToken.type != TokenType.EOF) advance();
                if (match(TokenType.RBRACE)) advance();
            }
        }
        
        private void parseClassDeclaration() {
            int line = currentToken.line;
            advance();
            
            if (!match(TokenType.IDENTIFIER)) {
                errorHandler.reportSyntaxError("Nom de classe attendu", currentToken.line, currentToken.col, currentToken.lexeme);
                synchronize();
                return;
            }
            
            String name = currentToken.lexeme;
            advance();
            
            String extendsInfo = "";
            if (match(TokenType.EXTENDS)) {
                advance();
                if (match(TokenType.IDENTIFIER)) {
                    extendsInfo = " extends " + currentToken.lexeme;
                    advance();
                } else {
                    errorHandler.reportSyntaxError("Nom de classe parent attendu apres extends", 
                                                  currentToken.line, currentToken.col, currentToken.lexeme);
                }
            }
            
            errorHandler.addSyntaxMessage("✓ Declaration de classe: " + name + extendsInfo + " (ligne " + line + ")");
            
            if (!expect(TokenType.LBRACE)) {
                synchronize();
                return;
            }
            
            while (!match(TokenType.RBRACE) && currentToken.type != TokenType.EOF) {
                if (match(TokenType.IDENTIFIER)) {
                    String methodName = currentToken.lexeme;
                    int methodLine = currentToken.line;
                    advance();
                    
                    if (match(TokenType.LPAREN)) {
                        advance();
                        int paramCount = 0;
                        while (!match(TokenType.RPAREN) && currentToken.type != TokenType.EOF) {
                            if (!match(TokenType.IDENTIFIER)) {
                                errorHandler.reportSyntaxError("Parametre attendu", currentToken.line, currentToken.col, currentToken.lexeme);
                                advance();
                                continue;
                            }
                            paramCount++;
                            advance();
                            if (match(TokenType.COMMA)) advance();
                        }
                        expect(TokenType.RPAREN);
                        errorHandler.addSyntaxMessage("  ✓ Methode: " + methodName + " (" + paramCount + " parametre(s)) (ligne " + methodLine + ")");
                        
                        if (!expect(TokenType.LBRACE)) {
                            skipUntilBrace();
                            continue;
                        }
                        parseBlock();
                        if (!expect(TokenType.RBRACE)) skipUntilBrace();
                    } else {
                        errorHandler.reportSyntaxError("Parenthese ouvrante attendue pour la methode", 
                                                      methodLine, currentToken.col, methodName);
                        skipUntilSemicolon();
                    }
                } else {
                    advance();
                }
            }
            expect(TokenType.RBRACE);
        }
        
        private void skipUntilBrace() {
            while (!match(TokenType.RBRACE, TokenType.LBRACE) && currentToken.type != TokenType.EOF) advance();
        }
        
        private void parseTryCatch() {
            int line = currentToken.line;
            advance();
            errorHandler.addSyntaxMessage("✓ Bloc try-catch (ligne " + line + ")");
            
            if (!expect(TokenType.LBRACE)) {
                synchronize();
                return;
            }
            parseBlock();
            if (!expect(TokenType.RBRACE)) {
                skipUntilBrace();
                if (match(TokenType.RBRACE)) advance();
            }
            
            if (match(TokenType.CATCH)) {
                advance();
                if (match(TokenType.LPAREN)) {
                    advance();
                    if (match(TokenType.IDENTIFIER)) {
                        advance();
                    } else {
                        errorHandler.reportSyntaxError("Parametre catch attendu", currentToken.line, currentToken.col, currentToken.lexeme);
                    }
                    if (!expect(TokenType.RPAREN)) {
                        while (!match(TokenType.RPAREN) && currentToken.type != TokenType.EOF) advance();
                        if (match(TokenType.RPAREN)) advance();
                    }
                }
                if (!expect(TokenType.LBRACE)) {
                    synchronize();
                    return;
                }
                parseBlock();
                if (!expect(TokenType.RBRACE)) {
                    skipUntilBrace();
                    if (match(TokenType.RBRACE)) advance();
                }
            }
        }
        
        private void parseBlock() {
            while (!match(TokenType.RBRACE) && currentToken.type != TokenType.EOF) {
                parseStatement();
            }
        }
        
        private void parseExpression() {
            parseComparison();
        }
        
        private void parseComparison() {
            parseAdditive();
            while (match(TokenType.EQUAL, TokenType.STRICT_EQUAL, TokenType.NOTEQUAL, 
                        TokenType.STRICT_NOT_EQUAL, TokenType.LT, TokenType.GT, 
                        TokenType.LE, TokenType.GE)) {
                advance();
                parseAdditive();
            }
        }
        
        private void parseAdditive() {
            parseMultiplicative();
            while (match(TokenType.PLUS, TokenType.MINUS)) {
                advance();
                parseMultiplicative();
            }
        }
        
        private void parseMultiplicative() {
            parsePrimary();
            while (match(TokenType.STAR, TokenType.SLASH, TokenType.PERCENT)) {
                advance();
                parsePrimary();
            }
        }
        
        private void parsePrimary() {
            if (match(TokenType.NUMBER, TokenType.STRING, TokenType.TRUE, TokenType.FALSE, TokenType.NULL)) {
                advance();
                return;
            }
            if (match(TokenType.IDENTIFIER)) {
                advance();
                return;
            }
            if (match(TokenType.LPAREN)) {
                advance();
                parseExpression();
                if (!expect(TokenType.RPAREN)) {
                    errorHandler.reportSyntaxError("Parenthese fermante manquante", 
                                                  currentToken.line, currentToken.col, currentToken.lexeme);
                }
                return;
            }
            errorHandler.reportSyntaxError("Expression invalide", currentToken.line, currentToken.col, currentToken.lexeme);
            advance();
        }
        
        private void skipUntilSemicolon() {
            while (!match(TokenType.SEMICOLON, TokenType.EOF, TokenType.RBRACE)) advance();
            if (match(TokenType.SEMICOLON)) advance();
        }
    }



    static class CompilerGUI extends JFrame {
        private JTextArea sourceArea;
        private JTextArea tokensArea;
        private JTextArea syntaxArea;
        private JTable errorTable;
        private DefaultTableModel errorModel;
        private JLabel statusLabel;
        private ErrorHandler errorHandler;
        
        public CompilerGUI() {
            setTitle("Compilateur JavaScript - Analyseur Lexical & Syntaxique");
            setSize(1400, 900);
            setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            setLocationRelativeTo(null);
            
            errorHandler = new ErrorHandler();
            
            JPanel mainPanel = new JPanel(new BorderLayout(10, 10));
            mainPanel.setBorder(new EmptyBorder(10, 10, 10, 10));
            
            // Boutons
            JPanel topPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            JButton openButton = new JButton("Ouvrir fichier");
            JButton analyzeButton = new JButton("Analyser");
            JButton clearButton = new JButton("Effacer");
            
            openButton.setFont(new Font("Arial", Font.BOLD, 14));
            analyzeButton.setFont(new Font("Arial", Font.BOLD, 14));
            clearButton.setFont(new Font("Arial", Font.BOLD, 14));
            
            topPanel.add(openButton);
            topPanel.add(analyzeButton);
            topPanel.add(clearButton);
            
            // Split horizontal: Code + Tokens + Syntaxe
            JSplitPane horizontalSplit1 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
            JSplitPane horizontalSplit2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
            
            // Code source
            JPanel sourcePanel = new JPanel(new BorderLayout());
            sourcePanel.setBorder(BorderFactory.createTitledBorder("Code Source JavaScript"));
            sourceArea = new JTextArea();
            sourceArea.setFont(new Font("Monospaced", Font.PLAIN, 14));
            sourceArea.setTabSize(4);
            JScrollPane sourceScroll = new JScrollPane(sourceArea);
            sourcePanel.add(sourceScroll, BorderLayout.CENTER);
            
            // Tokens
            tokensArea = new JTextArea();
            tokensArea.setFont(new Font("Monospaced", Font.PLAIN, 12));
            tokensArea.setEditable(false);
            JScrollPane tokensScroll = new JScrollPane(tokensArea);
            
            // Analyse syntaxique
            syntaxArea = new JTextArea();
            syntaxArea.setFont(new Font("Monospaced", Font.PLAIN, 12));
            syntaxArea.setEditable(false);
            JScrollPane syntaxScroll = new JScrollPane(syntaxArea);
            
            horizontalSplit1.setLeftComponent(sourcePanel);
            horizontalSplit1.setDividerLocation(500);
            
            horizontalSplit2.setLeftComponent(horizontalSplit1);
            horizontalSplit2.setDividerLocation(1000);
            
            // Panel du bas: Tableau des erreurs
            JPanel errorPanel = new JPanel(new BorderLayout());
            errorPanel.setBorder(BorderFactory.createTitledBorder("Erreurs detectees"));
            
            String[] columnNames = {"Type", "Ligne", "Colonne", "Message"};
            errorModel = new DefaultTableModel(columnNames, 0) {
                public boolean isCellEditable(int row, int column) {
                    return false;
                }
            };
            errorTable = new JTable(errorModel);
            errorTable.setFont(new Font("Monospaced", Font.PLAIN, 12));
            errorTable.getColumnModel().getColumn(0).setPreferredWidth(100);
            errorTable.getColumnModel().getColumn(1).setPreferredWidth(60);
            errorTable.getColumnModel().getColumn(2).setPreferredWidth(60);
            errorTable.getColumnModel().getColumn(3).setPreferredWidth(600);
            JScrollPane errorScroll = new JScrollPane(errorTable);
            errorPanel.add(errorScroll, BorderLayout.CENTER);
            
            // Split vertical pour code/tokens/syntaxe et erreurs
            JSplitPane verticalSplit = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
            verticalSplit.setTopComponent(horizontalSplit2);
            verticalSplit.setBottomComponent(errorPanel);
            verticalSplit.setDividerLocation(550);
            
            // Status bar
            statusLabel = new JLabel(" Pret");
            statusLabel.setBorder(new BevelBorder(BevelBorder.LOWERED));
            
            // Ajouter les composants
            mainPanel.add(topPanel, BorderLayout.NORTH);
            mainPanel.add(verticalSplit, BorderLayout.CENTER);
            mainPanel.add(statusLabel, BorderLayout.SOUTH);
            
            add(mainPanel);
            
            // Actions des boutons
            openButton.addActionListener(e -> openFile());
            analyzeButton.addActionListener(e -> analyze());
            clearButton.addActionListener(e -> clear());
        }
        
        private void openFile() {
            JFileChooser fileChooser = new JFileChooser();
            fileChooser.setFileFilter(new javax.swing.filechooser.FileFilter() {
                public boolean accept(File f) {
                    return f.isDirectory() || f.getName().toLowerCase().endsWith(".txt") || 
                           f.getName().toLowerCase().endsWith(".js");
                }
                public String getDescription() {
                    return "Fichiers JavaScript (*.js, *.txt)";
                }
            });
            
            int result = fileChooser.showOpenDialog(this);
            if (result == JFileChooser.APPROVE_OPTION) {
                File file = fileChooser.getSelectedFile();
                try {
                    BufferedReader reader = new BufferedReader(new FileReader(file));
                    String content = "";
                    String line;
                    while ((line = reader.readLine()) != null) {
                        content += line + "\n";
                    }
                    reader.close();
                    sourceArea.setText(content);
                    statusLabel.setText(" Fichier charge: " + file.getName());
                } catch (IOException ex) {
                    JOptionPane.showMessageDialog(this, "Erreur lors de la lecture du fichier: " + ex.getMessage(),
                                                "Erreur", JOptionPane.ERROR_MESSAGE);
                }
            }
        }
        
        private void analyze() {
            String code = sourceArea.getText();
            if (code.trim().length() == 0) {
                JOptionPane.showMessageDialog(this, "Veuillez entrer du code a analyser.",
                                            "Attention", JOptionPane.WARNING_MESSAGE);
                return;
            }
            
            // Reinitialiser
            errorHandler.clear();
            errorModel.setRowCount(0);
            tokensArea.setText("");
            syntaxArea.setText("");
            
            try {
                // ANALYSE LEXICALE
                Lexer lexer1 = new Lexer(code, errorHandler);
                String tokensText = "=== ANALYSE LEXICALE ===\n\n";
                Token token;
                do {
                    token = lexer1.nextToken();
                    if (token.type != TokenType.ERROR && token.type != TokenType.EOF) {
                        String type = "" + token.type;
                        while (type.length() < 20) type += " ";
                        String lexeme = "'" + token.lexeme + "'";
                        while (lexeme.length() < 20) lexeme += " ";
                        tokensText += type + lexeme + "Ligne: " + token.line + " Col: " + token.col + "\n";
                    }
                } while (token.type != TokenType.EOF);
                tokensArea.setText(tokensText);
                
                // ANALYSE SYNTAXIQUE
                Lexer lexer2 = new Lexer(code, errorHandler);
                Parser parser = new Parser(lexer2, errorHandler);
                parser.parse();
                
                String syntaxText = "";
                ArrayList<String> messages = errorHandler.getSyntaxMessages();
                for (int i = 0; i < messages.size(); i++) {
                    syntaxText += messages.get(i) + "\n";
                }
                syntaxArea.setText(syntaxText);
                
                // Afficher les erreurs
                ArrayList<String> errors = errorHandler.getErrors();
                for (int i = 0; i < errors.size(); i++) {
                    String[] parts = parseError(errors.get(i));
                    errorModel.addRow(parts);
                }
                
                // Status
                int totalErrors = errorHandler.getTotalErrors();
                if (totalErrors == 0) {
                    statusLabel.setText(" ✓ Analyse terminee sans erreur");
                    statusLabel.setForeground(new Color(0, 128, 0));
                } else {
                    statusLabel.setText(" ✗ Analyse terminee avec " + totalErrors + " erreur(s) - " +
                                      errorHandler.getLexicalErrors() + " lexicale(s), " +
                                      errorHandler.getSyntaxErrors() + " syntaxique(s)");
                    statusLabel.setForeground(new Color(200, 0, 0));
                }
                
            } catch (Exception ex) {
                JOptionPane.showMessageDialog(this, "Erreur lors de l'analyse: " + ex.getMessage(),
                                            "Erreur", JOptionPane.ERROR_MESSAGE);
                ex.printStackTrace();
            }
        }
        
        private String[] parseError(String error) {
            String[] result = new String[4];
            
            if (error.indexOf("ERREUR LEXICALE") >= 0) {
                result[0] = "Lexicale";
            } else if (error.indexOf("ERREUR SYNTAXIQUE") >= 0) {
                result[0] = "Syntaxique";
            } else {
                result[0] = "Autre";
            }
            
            int ligneStart = error.indexOf("Ligne ") + 6;
            int ligneEnd = error.indexOf(",", ligneStart);
            result[1] = error.substring(ligneStart, ligneEnd);
            
            int colStart = error.indexOf("Col ") + 4;
            int colEnd = error.indexOf("]", colStart);
            result[2] = error.substring(colStart, colEnd);
            
            int messageStart = error.indexOf("]: ") + 3;
            result[3] = error.substring(messageStart);
            
            return result;
        }
        
        private void clear() {
            sourceArea.setText("");
            tokensArea.setText("");
            syntaxArea.setText("");
            errorModel.setRowCount(0);
            errorHandler.clear();
            statusLabel.setText(" Pret");
            statusLabel.setForeground(Color.BLACK);
        }
    }


    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            CompilerGUI gui = new CompilerGUI();
            gui.setVisible(true);
        });
    } }


