#include "imp_codegen.hh"

ImpCodeGen::ImpCodeGen(ImpTypeChecker* a):analysis(a) {

}

void ImpCodeGen::codegen(string label, string instr) {
  if (label !=  nolabel)
    code << label << ": ";
  code << instr << endl;
}

void ImpCodeGen::codegen(string label, string instr, int arg) {
  if (label !=  nolabel)
    code << label << ": ";
  code << instr << " " << arg << endl;
}

void ImpCodeGen::codegen(string label, string instr, string jmplabel) {
  if (label !=  nolabel)
    code << label << ": ";
  code << instr << " " << jmplabel << endl;
}

string ImpCodeGen::next_label() {
  string l = "L";
  string n = to_string(current_label++);
  l.append(n);
  return l;
}

string ImpCodeGen::get_flabel(string fname) {
  string l = "L";
  l.append(fname);
  return l;
}

void ImpCodeGen::codegen(Program* p, string outfname) {
  nolabel = "";
  current_label = 0;

  p->accept(this);
  ofstream outfile;
  outfile.open(outfname);
  outfile << code.str();
  outfile.close();

  return;
}

void ImpCodeGen::visit(Program* p) {
  current_dir = 0;  
  direcciones.add_level();
  process_global = true;
  p->vardecs->accept(this);
  process_global = false;
  mem_globals = current_dir;
  // codegen
  codegen("start","skip");
  codegen(nolabel,"enter",mem_globals);
  codegen(nolabel,"alloc",mem_globals);
  codegen(nolabel,"mark");
  codegen(nolabel,"pusha",get_flabel("main"));
  codegen(nolabel,"call");
  codegen(nolabel,"halt");
  p->fundecs->accept(this);
  direcciones.remove_level();
  return ;
}

void ImpCodeGen::visit(Body * b) {
  int dir = current_dir;
  direcciones.add_level();
  
  b->vardecs->accept(this);
  b->slist->accept(this);
  
  direcciones.remove_level();

  current_dir = dir;
  return ;
}

void ImpCodeGen::visit(VarDecList* s) {
  list<VarDec*>::iterator it;
  for (it = s->vardecs.begin(); it != s->vardecs.end(); ++it) {
    (*it)->accept(this);
  }  
  return;
}

void ImpCodeGen::visit(VarDec* vd) {
  list<string>::iterator it;
  for (it = vd->vars.begin(); it != vd->vars.end(); ++it){
    current_dir++;
    VarEntry ventry;
    ventry.dir = current_dir;
    ventry.is_global = process_global;
    direcciones.add_var(*it, ventry);
  }
  return ;
}

void ImpCodeGen::visit(FunDecList* s) {
  list<FunDec*>::iterator it;
  for (it = s->flist.begin(); it != s->flist.end(); ++it) {
    (*it)->accept(this);
  }
  return ;
}

void ImpCodeGen::visit(FunDec* fd) {
  FEntry fentry = analysis->ftable.lookup(fd->fname);
  current_dir = 0;
  list<string>::iterator it, vit;
  // Parameters
  int i = 1;
  int m = fd->types.size();
  VarEntry ventry;
  for (it = fd->types.begin(), vit = fd->vars.begin();
       it != fd->types.end(); ++it, ++vit){
    ventry.is_global = false;
    ventry.dir = i-(m+3);
    direcciones.add_var(*vit,ventry);
    i++;
    // cout << *it << " " << *vit;
  }
  ImpType ftype = fentry.ftype;
  if (ftype.types[ftype.types.size()-1] != ImpType::VOID) {
    ventry.dir = -(m+3);
    direcciones.add_var("return", ventry);
  }

  codegen(get_flabel(fd->fname),"skip");
  codegen(nolabel,"enter",fentry.mem_locals+fentry.max_stack);
  codegen(nolabel,"alloc",fentry.mem_locals);

  num_params = m;
  fd->body->accept(this);

  return ;
}


void ImpCodeGen::visit(StatementList* s) {
  list<Stm*>::iterator it;
  for (it = s->stms.begin(); it != s->stms.end(); ++it) {
    (*it)->accept(this);
  }
  return ;
}

void ImpCodeGen::visit(AssignStatement* s) {
  s->rhs->accept(this);
  VarEntry ventry = direcciones.lookup(s->id);
  if (ventry.is_global)
    codegen(nolabel,"store", ventry.dir);
  else
    codegen(nolabel,"storer", ventry.dir);
  return ;
}

void ImpCodeGen::visit(PrintStatement* s) {
  s->e->accept(this);
  code << "print" << endl;;
  return ;
}

void ImpCodeGen::visit(IfStatement* s) {
  string l1 = next_label();
  string l2 = next_label();
  
  s->condition->accept(this);
  codegen(nolabel,"jmpz",l1);
  s->then->accept(this);
  codegen(nolabel,"goto",l2);
  codegen(l1,"skip");
  if (s->els!=NULL) {
    s->els->accept(this);
  }
  codegen(l2,"skip");
 
  return ;
}

void ImpCodeGen::visit(WhileStatement* s) {
  string l1 = next_label();
  string l2 = next_label();

  codegen(l1,"skip");
  s->condition->accept(this);
  codegen(nolabel,"jmpz",l2);
  s->b->accept(this);
  codegen(nolabel,"goto",l1);
  codegen(l2,"skip");

  return ;
}

void ImpCodeGen::visit(ReturnStatement* s) {
  //nuevo
  if (s->e != NULL) {
    VarEntry ventry = direcciones.lookup("return");
    s->e->accept(this);
    codegen(nolabel,"storer", ventry.dir);
  }
  codegen(nolabel,"return",num_params+3);  // fp-m-3
  return ;
}

void ImpCodeGen::visit(ForStatement* s) {

    string l1 = next_label();
    string l2 = next_label();
    string l3 = next_label();

    //alloc 1
    codegen(nolabel, "alloc", 1);
    current_dir++;

    // Inicializar el contador
    s->start->accept(this); 
    codegen(nolabel, "storer", current_dir);

    codegen(l1, "skip");

    // Evaluar la condición
    codegen(nolabel, "loadr", current_dir);
    s->end->accept(this);
    codegen(nolabel, "sub");
    codegen(nolabel, "jmpz", l2);

    // Ejecutar el cuerpo del bucle
    s->b->accept(this);

    // Incrementar el contador
    codegen(nolabel, "loadr", current_dir);
    s->step->accept(this); 
    codegen(nolabel, "add");
    codegen(nolabel, "storer", current_dir);

    codegen(nolabel, "goto", l1);
    codegen(l2, "skip");

  return ;
}

int ImpCodeGen::visit(BinaryExp* e) {
  e->left->accept(this);
  e->right->accept(this);
  string op = "";
  switch(e->op) {
  case PLUS_OP: op =  "add"; break;
  case MINUS_OP: op = "sub"; break;
  case MUL_OP:  op = "mul"; break;
  case DIV_OP:  op = "div"; break;
  case LT_OP:  op = "lt"; break;
  case LE_OP: op = "le"; break;
  case EQ_OP:  op = "eq"; break;
  default: cout << "binop " << Exp::binopToChar(e->op) << " not implemented" << endl;
  }
  codegen(nolabel, op);
  return 0;
}

int ImpCodeGen::visit(NumberExp* e) {
  codegen(nolabel,"push ",e->value);
  return 0;
}

int ImpCodeGen::visit(BoolExp* e) {
  codegen(nolabel,"push",e->value?1:0);
 
  return 0;
}

int ImpCodeGen::visit(IdentifierExp* e) {
  VarEntry ventry = direcciones.lookup(e->name);
  if (ventry.is_global)
    codegen(nolabel,"load",ventry.dir);
  else
    codegen(nolabel,"loadr",ventry.dir);
  return 0;
}

int ImpCodeGen::visit(IFExp* e) {
  string l1 = next_label();
  string l2 = next_label();
 
  e->cond->accept(this);
  codegen(nolabel, "jmpz", l1);
  e->left->accept(this);
  codegen(nolabel, "goto", l2);
  codegen(l1,"skip");
  e->right->accept(this);
  codegen(l2, "skip");
  return 0;
}

int ImpCodeGen::visit(FCallExp* e) {
  // nuevo
  FEntry fentry = analysis->ftable.lookup(e->fname);
  ImpType ftype = fentry.ftype;
  if (ftype.types[ftype.types.size()-1] != ImpType::VOID) {
    codegen(nolabel,"alloc",1);
  }
  list<Exp*>::iterator it;
  for (it = e->args.begin(); it != e->args.end(); ++it) {
    (*it)->accept(this);
  }
  codegen(nolabel,"mark");
  codegen(nolabel,"pusha",get_flabel(e->fname));
  codegen(nolabel,"call");
  return 0;
}

void ImpCodeGen::visit(FCallStatement* s) {
  // nuevo
  FEntry fentry = analysis->ftable.lookup(s->fname);
  ImpType ftype = fentry.ftype;
  if (ftype.types[ftype.types.size()-1] != ImpType::VOID) {
    codegen(nolabel,"alloc",1);
  }
  list<Exp*>::iterator it;
  for (it = s->args.begin(); it != s->args.end(); ++it) {
    (*it)->accept(this);
  }
  codegen(nolabel,"mark");
  codegen(nolabel,"pusha",get_flabel(s->fname));
  codegen(nolabel,"call");
  return ;
}