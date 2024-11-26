#include <iostream>
#include <stdexcept>
#include "token.h"
#include "scanner.h"
#include "exp.h"
#include "parser.h"

using namespace std;

bool Parser::match(Token::Type ttype) {
    if (check(ttype)) {
        advance();
        return true;
    }
    return false;
}

bool Parser::check(Token::Type ttype) {
    if (isAtEnd()) return false;
    return current->type == ttype;
}

bool Parser::advance() {
    if (!isAtEnd()) {
        Token* temp = current;
        if (previous) delete previous;
        current = scanner->nextToken();
        previous = temp;
        if (check(Token::ERR)) {
            cout << "Error de análisis, carácter no reconocido: " << current->text << endl;
            exit(1);
        }
        return true;
    }
    return false;
}


bool Parser::isAtEnd() {
    return (current->type == Token::END);
}

Parser::Parser(Scanner* sc):scanner(sc) {
    previous = NULL;
    current = scanner->nextToken();
    if (current->type == Token::ERR) {
        cout << "Error en el primer token: " << current->text << endl;
        exit(1);
    }
}

//-----------------------------------------------------PARSER---------------------------------------------------------------------------------------------

VarDec* Parser::parseVarDec() {
    VarDec* vd = NULL;
    // Verificar el tipo de la variable
    if (match(Token::INT) || match(Token::LONG)) {
        string type = previous->text; 
        list<string> ids;
        cout<<"current "<<current->text<<endl;
        if (!match(Token::ID)) {
            cout << "Error: se esperaba un identificador después del tipo '" << type << "'." << endl;
            exit(1);
        }
        ids.push_back(previous->text);

        // Procesar identificadores adicionales separados por comas
        while (match(Token::COMA)) {
            if (!match(Token::ID)) {
                cout << "Error: se esperaba un identificador después de ','." << endl;
                exit(1);
            }
            ids.push_back(previous->text);
        }

        vd = new VarDec(type, ids);
    }
    return vd;
}


VarDecList* Parser::parseVarDecList() {
    VarDecList* vdl = new VarDecList();
    while (check(Token::INT) || check(Token::LONG)) {
        VarDec* vd = parseVarDec();
        if (vd != NULL) {
            vdl->add(vd); 
        } else {
            cout << "Error: declaración de variable inválida." << endl;
            exit(1);
        }
    }
    return vdl;
}


StatementList* Parser::parseStatementList() {
    StatementList* sl = new StatementList();

    while (!check(Token::LLD)) { //Parseamos hasta que se encuentre un '}' que signifique el fin del cuerpo
        if (check(Token::PC)) { //Ignorar punto y coma
            advance(); 
            continue;
        }

        Stm* statement = parseStatement();
        if (statement != NULL) {
            sl->add(statement);
        } else {
            cout << "Error: sentencia inválida." << endl;
            exit(1);
        }

        if (check(Token::PC)) {
            advance();
        }
    }

    return sl;
}



Body* Parser::parseBody() {
    VarDecList* vdl = parseVarDecList();
    StatementList* sl = parseStatementList();
    
    if (!match(Token::LLD)) {
        cout << "Error: se esperaba '}' al final del bloque." << endl;
        exit(1);
    }

    return new Body(vdl, sl);
}

FunDec* Parser::parseFunDec() {
    
    FunDec* fd = NULL;
    if (!match(Token::INT) && !match(Token::LONG)) {
        return NULL;
    }
    string rtype = previous->text; 

    if (!match(Token::ID)) {
        cout << "Error: se esperaba un nombre de función después del tipo de retorno." << endl;
        exit(1);
    }
    string fname = previous->text;  

    if (!match(Token::PI)) {  
        cout << "Error: se esperaba '(' después del nombre de la función." << endl;
        exit(1);
    }

    list<string> types;  
    list<string> vars; 
    if (!check(Token::PD)) { 
        do {
            if (!match(Token::INT) && !match(Token::LONG)) {
                cout << "Error: se esperaba un tipo de parámetro después de '('.";
                exit(1);
            }
            types.push_back(previous->text);

            if (!match(Token::ID)) {
                cout << "Error: se esperaba un nombre de parámetro después del tipo." << endl;
                exit(1);
            }
            vars.push_back(previous->text);

        } while (match(Token::COMA));  // Continuar mientras haya ','
    }

    // Verificar el cierre de la lista de parámetros ')'
    if (!match(Token::PD)) {  
        cout << "Error: se esperaba ')' después de la lista de parámetros." << endl;
        exit(1);
    }

    if (!match(Token::LLI)) { 
        cout << "Error: se esperaba '{' al inicio del cuerpo de la función." << endl;
        exit(1);
    }

    Body* body = parseBody();

    fd = new FunDec(fname, types, vars, rtype, body);

    return fd;
}


FunDecList* Parser::parseFunDecList() {
    FunDecList* fdl = new FunDecList();
    FunDec* aux;
    aux = parseFunDec();
    while (aux != NULL) {
        fdl->add(aux);
        aux = parseFunDec();
    }
    return fdl;
}

list<Stm*> Parser::parseStmList() {
    list<Stm*> slist;
    slist.push_back(parseStatement());
    while(match(Token::PC)) {
        slist.push_back(parseStatement());
    }
    return slist;
}

Stm* Parser::parseStatement() {
    Stm* s = NULL;
    Exp* e = NULL;
    Body* tb = NULL; //true case
    Body* fb = NULL; //false case

    if (current == NULL) {
        cout << "Error: Token actual es NULL" << endl;
        exit(1);
    }    
    if (match(Token::ID)) {
        string lex = previous->text;

        if (match(Token::ASSIGN)) {
            e = parseCExp();
            s = new AssignStatement(lex, e);
        }else{
            if(match(Token::PI)){
                list<Exp*> args;
                if (!check(Token::PD)){
                    args.push_back(parseCExp());
                    while (match(Token::COMA)){
                        args.push_back(parseCExp());
                    }
                }
                if (!match(Token::PD)){
                    cout << "Falta paréntesis derecho" << endl;
                    exit(0);
                }
                s = new FCallStatement(lex, args);
            }
        }

    } else if (match(Token::PRINTF)) {
        if (!match(Token::PI)) {
            cout << "Error: se esperaba '(' después de 'printf'." << endl;
            exit(1);
        }
        if (!match(Token::STRING)) {
            cout << "Error: se esperaba una cadena de texto después de 'printf('." << endl;
            exit(1);
        }

        
        if (!match(Token::COMA)) {
            cout << "Error: se esperaba ',' después de la cadena de texto." << endl;
            exit(1);
        }
        e = parseCExp();
        if (!match(Token::PD)) {
            cout << "Error: se esperaba ')' después de la expresión." << endl;
            exit(1);
        }
        s = new PrintStatement(e);
    }

    else if (match(Token::IF)) {
    if (!match(Token::PI)) {
        cout << "Error: se esperaba '(' después de 'if'." << endl;
        exit(1);
    }
    e = parseCExp();
        if (!match(Token::PD)) {
        cout << "Error: se esperaba ')' después de la expresión del `if`." << endl;
        exit(1);
    }
    if (!match(Token::LLI)) {
        cout << "Error: se esperaba '{' después de la expresión del `if`." << endl;
        exit(1);
    }
    Body* tb = parseBody(); 

    Body* fb = nullptr;
    if (match(Token::ELSE)) {
        if (!match(Token::LLI)) {
            cout << "Error: se esperaba '{' después de 'else'." << endl;
            exit(1);
        }
        fb = parseBody();
    }
    s = new IfStatement(e, tb, fb);
}
    else if (match(Token::WHILE)) {
        e = parseCExp();
        if (!match(Token::DO)) {
            cout << "Error: se esperaba 'do' después de la expresión." << endl;
            exit(1);
        }
        tb = parseBody();
        if (!match(Token::ENDWHILE)) {
            cout << "Error: se esperaba 'endwhile' al final de la declaración." << endl;
            exit(1);
        }
        s = new WhileStatement(e, tb);

    }
    else if(match(Token::FOR)){
        if(!match(Token::PI)){
            cout << "Error: se esperaba '(' después de 'for'." << endl;
            exit(1);
        }
        Exp* start = parseCExp();
        if (!match(Token::COMA)) {
            cout << "Error: se esperaba ',' después de la expresión." << endl;
            exit(1);
        }
        Exp* end = parseCExp();
        if (!match(Token::COMA)) {
            cout << "Error: se esperaba ',' después de la expresión." << endl;
            exit(1);
        }
        Exp* step = parseCExp();
        if (!match(Token::PD)) {
            cout << "Error: se esperaba ')' después de la expresión." << endl;
            exit(1);
        }
        if(!match(Token::LLI)){
            cout << "Error: se esperaba '{' después de la declaración." << endl;
            exit(1);
        }
        tb = parseBody();
        s = new ForStatement(start, end, step, tb);
    }

    else if(match(Token::RETURN)){
        //Parsear return en C
        if (check(Token::PI)){
            advance();
            e = parseCExp();
            if (!match(Token::PD)){
                cout << "Falta paréntesis derecho" << endl;
                exit(0);
            }
        }
        s = new ReturnStatement(e); //Si es null, no hay problema
    }
    else {
        cout << "Error: Se esperaba un statement: " << *current << endl;
        exit(1);
    }
    return s;
}

Exp* Parser::parseCExp(){
    Exp* left = parseExpression();
    if (match(Token::LT) || match(Token::LE) || match(Token::EQ)){
        BinaryOp op;
        if (previous->type == Token::LT){
            op = LT_OP;
        }
        else if (previous->type == Token::LE){
            op = LE_OP;
        }
        else if (previous->type == Token::EQ){
            op = EQ_OP;
        }
        Exp* right = parseExpression();
        left = new BinaryExp(left, right, op);
    }
    return left;
}

Exp* Parser::parseExpression() {
    Exp* left = parseTerm();
    while (match(Token::PLUS) || match(Token::MINUS)) {
        BinaryOp op;
        if (previous->type == Token::PLUS){
            op = PLUS_OP;
        }
        else if (previous->type == Token::MINUS){
            op = MINUS_OP;
        }
        Exp* right = parseTerm();
        left = new BinaryExp(left, right, op);
    }
    return left;
}

Exp* Parser::parseTerm() {
    Exp* left = parseFactor();
    while (match(Token::MUL) || match(Token::DIV)) {
        BinaryOp op;
        if (previous->type == Token::MUL){
            op = MUL_OP;
        }
        else if (previous->type == Token::DIV){
            op = DIV_OP;
        }
        Exp* right = parseFactor();
        left = new BinaryExp(left, right, op);
    }
    return left;
}

Exp* Parser::parseFactor() {
    Exp* e;
    Exp* e1;
    Exp* e2;
    if (match(Token::TRUE)){
        return new BoolExp(1);
    }else if (match(Token::FALSE)){
        return new BoolExp(0);
    }
    else if (match(Token::NUM)) {
        return new NumberExp(stoi(previous->text));
    }
    else if (match(Token::ID)) {
        string texto = previous->text;
        //Parse FCallExp
        if (match(Token::PI)){
            list<Exp*> args;
            if (!check(Token::PD)){
                args.push_back(parseCExp());
                while (match(Token::COMA)){
                    args.push_back(parseCExp());
                }
            }
            if (!match(Token::PD)){
                cout << "Falta paréntesis derecho" << endl;
                exit(0);
            }
            return new FCallExp(texto, args);
        }
        else{
            return new IdentifierExp(previous->text);
        }
            
    }
    else if (match(Token::IFEXP)) {
        match(Token::PI);
        e=parseCExp();
        match(Token::COMA);
        e1=parseCExp();
        match(Token::COMA);
        e2=parseCExp();
        match(Token::PD);
        return new IFExp(e,e1,e2);
    }
    else if (match(Token::PI)){
        e = parseCExp();
        if (!match(Token::PD)){
            cout << "Falta paréntesis derecho" << endl;
            exit(0);
        }
        return e;
    }
    cout << "Error: se esperaba un número o identificador." << endl;
    exit(0);
}


Program* Parser::parseProgram() {
    if(current == NULL){
        cout << "Error: Token actual es NULL" << endl;
        exit(1);
    }

    //Avanza hasta encontrar una llave que indique el inicio de las funciones
    while (!check(Token::MAIN)) {
        advance();
    }
    //Avanza hasta encontrar una llave que indique el inicio de las funciones y guardame una declaracion de variable como main
    //cout << "current: " << current->text << endl;
    VarDecList* v = parseVarDecList();
    FunDecList* b = parseFunDecList();
    return new Program(v, b);
}

