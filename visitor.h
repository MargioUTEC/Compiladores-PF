#ifndef VISITOR_H
#define VISITOR_H
#include "exp.h"

#include <list>

class BinaryExp;
class NumberExp;
class BoolExp;
class IFExp;
class IdentifierExp;
class AssignStatement;
class PrintStatement;
class IfStatement;
class WhileStatement;
class ForStatement;
class VarDec;
class VarDecList;
class StatementList;
class Body;
class Program;
class FunDec;
class FunDecList;
class ReturnStatement;
class FCallExp;
class FCallStatement;

class Visitor {
public:
    virtual int visit(IFExp* exp) = 0;
    virtual int visit(BinaryExp* exp) = 0;
    virtual int visit(NumberExp* exp) = 0;
    virtual int visit(BoolExp* exp) = 0;
    virtual int visit(IdentifierExp* exp) = 0;
    virtual int visit(FCallExp* exp) = 0;
    virtual void visit(AssignStatement* stm) = 0;
    virtual void visit(PrintStatement* stm) = 0;
    virtual void visit(IfStatement* stm) = 0;
    virtual void visit(WhileStatement* stm) = 0;
    virtual void visit(ForStatement* stm) = 0;
    virtual void visit(VarDec* stm) = 0;
    virtual void visit(VarDecList* stm) = 0;
    virtual void visit(StatementList* stm) = 0;
    virtual void visit(Body* b) = 0;
    virtual void visit(Program* p) = 0;
    virtual void visit(FunDec* f) = 0;
    virtual void visit(FunDecList* f) = 0;
    virtual void visit(ReturnStatement* r) = 0;
    virtual void visit(FCallStatement* f) = 0;
};

class PrintVisitor : public Visitor {
public:
    void imprimir(Program* program);
    int visit(IFExp* exp) override;
    int visit(BinaryExp* exp) override;
    int visit(NumberExp* exp) override;
    int visit(BoolExp* exp) override;
    int visit(IdentifierExp* exp) override;
    int visit(FCallExp* exp) override;
    void visit(AssignStatement* stm) override;
    void visit(PrintStatement* stm) override;
    void visit(IfStatement* stm) override;
    void visit(WhileStatement* stm) override;
    void visit(ForStatement* stm) override;
    void visit(VarDec* stm) override;
    void visit(VarDecList* stm) override;
    void visit(StatementList* stm) override;
    void visit(Body* b) override;
    void visit(Program* p) override;
    void visit(FunDec* f) override;
    void visit(FunDecList* f) override;
    void visit(ReturnStatement* r) override;
    void visit(FCallStatement* f) override;

private:
    int indentLevel = 0;
    void increaseIndent() { indentLevel++; }
    void decreaseIndent() { indentLevel--; }
    void printIndent();
};




#endif // VISITOR_H