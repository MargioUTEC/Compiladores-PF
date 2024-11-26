#ifndef IMP_CODEGEN
#define IMP_CODEGEN

#include <sstream>
#include <iostream>
#include <stdlib.h>
#include <cstring>
#include <string>
#include <fstream>

#include "exp.h"
#include "visitor.h"
#include "environment.hh"
#include "imp_type_checker.hh"

class VarEntry {
public:
  int dir;
  bool is_global;
};

class ImpCodeGen : public Visitor {
public:
  ImpCodeGen(ImpTypeChecker*);
  void codegen(Program*, string outfname);
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
  void visit(ReturnStatement*);
  void visit(ForStatement*);
  void visit(FCallStatement*);

  int visit(BinaryExp* e);
  int visit(NumberExp* e);
  int visit(BoolExp* e);
  int visit(IdentifierExp* e);
  int visit(IFExp* e);
  int visit(FCallExp* e);

private:
  ImpTypeChecker* analysis;
  std::ostringstream code;
  string nolabel;
  int current_label;
  Environment<VarEntry> direcciones;
  int current_dir;
  int max_stack, mem_locals, mem_globals;
  bool process_global;
  int num_params; // nuevo
  void codegen(string label, string instr);
  void codegen(string label, string instr, int arg);
  void codegen(string label, string instr, string jmplabel);
  string next_label();
  string get_flabel(string fname);
};


#endif
