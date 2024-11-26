#ifndef IMP_INTERPRETER
#define IMP_INTERPRETER

#include <unordered_map>

#include "exp.h"
#include "imp_value_visitor.hh"
#include "environment.hh"

using namespace std;

class ImpInterpreter : public ImpValueVisitor {
private:
  Environment<ImpValue> env;
  Environment<FunDec*> fdecs;
  ImpValue retval;
  bool retcall;

public:
  void interpret(Program*);
  void visit(Program*);
  void visit(Body*);
  void visit(VarDecList*);
  void visit(VarDec*);  
  void visit(FunDecList*);
  void visit(FunDec*);
  void visit(StatementList*);
  void visit(AssignStatement*);
  void visit(PrintStatement*);
  void visit(IfStatement*);
  void visit(WhileStatement*);
  void visit(ReturnStatement* e);
  void visit(ForStatement* e);
  void visit(FCallStatement* e);
  ImpValue visit(BinaryExp* e);
  ImpValue visit(BoolExp* e);
  ImpValue visit(NumberExp* e);
  ImpValue visit(IdentifierExp* e);
  ImpValue visit(IFExp* e);
  ImpValue visit(FCallExp* e);
};


#endif

