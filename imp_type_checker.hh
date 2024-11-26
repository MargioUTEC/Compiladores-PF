#ifndef IMP_TYPECHECKER
#define IMP_TYPECHECKER

#include <unordered_map>
#include <vector>

#include "exp.h"
#include "type_visitor.h"
#include "environment.hh"

using namespace std;


class FEntry {
public:
  string fname;
  ImpType ftype;
  int mem_locals, max_stack;
};

class ImpTypeChecker : public TypeVisitor {
public:
  ImpTypeChecker();
  Environment<FEntry> ftable;  
private:
  Environment<ImpType> env;
  bool has_main;
  ImpType booltype;
  ImpType inttype;
  ImpType voidtype;
  ImpType maintype;

  vector<string> fnames; 
  int sp, max_sp;
  int dir, max_dir;
  void sp_decr(int n);
  void sp_incr(int n);

public:
  void typecheck(Program*);
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
  void visit(ForStatement*);
  void visit(FCallStatement*);
  
  ImpType visit(BinaryExp* e);
  ImpType visit(NumberExp* e);
  ImpType visit(BoolExp* e);
  ImpType visit(IdentifierExp* e);
  ImpType visit(IFExp* e);
  ImpType visit(FCallExp* e);
private:
  void add_fundec(FunDec* fd);
  ImpType process_fcall(FCallExp*);
};


#endif
