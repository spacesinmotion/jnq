
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <setjmp.h>

#ifndef _WIN32
#include <unistd.h>
#define JNQ_BIN "./jnq_bin"
#else
#include <io.h>
#define access _access
#define F_OK 0
#define JNQ_BIN "jnq_bin.exe"
#endif

typedef struct State {
  const char *file;
  const char *c;
  size_t line;
  size_t column;
} State;

State State_new(const char *c, const char *file) { return (State){.file = file, .c = c, .line = 1, .column = 1}; }

void State_skip(State *s, int c) {
  s->c += c;
  s->column += c;
}

struct {
  State states[128];
  int size;
  jmp_buf back;
} traceStack;

void FATALX(const char *format, ...);
void traceStack_push(State *s) {
  if (traceStack.size == 128)
    FATALX("not enough buffer for stack trace");
  traceStack.states[traceStack.size] = *s;
  traceStack.size++;
}

void traceStack_pop() { traceStack.size--; }

void FATAL(State *st, const char *format, ...) {
  va_list args;
  fprintf(stderr, "%s:%zu:%zu: error: ", st->file, st->line, st->column);
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  for (int i = traceStack.size - 1; i >= 0; --i) {
    State *sst = &traceStack.states[i];
    fprintf(stderr, " - %s:%zu:%zu\n", sst->file, sst->line, sst->column);
  }

  longjmp(traceStack.back, 1);
}
void FATALX(const char *format, ...) {
  va_list args;
  fprintf(stderr, "%s:%zu:%zu error: ", "unknown.jnq", (size_t)0, (size_t)0);
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  for (int i = traceStack.size - 1; i >= 0; --i) {
    State *sst = &traceStack.states[i];
    fprintf(stderr, " - %s:%zu:%zu\n", sst->file, sst->line, sst->column);
  }

  longjmp(traceStack.back, 1);
}

State back(State *s, int c) {
  State b = *s;
  if (b.column < c) {
    b.column = 0;
    FATAL(&b, "internal error calculating location");
  }
  b.column -= c;
  return b;
}

typedef struct Identifier Identifier;
typedef struct Variable Variable;
typedef struct Brace Brace;
typedef struct Call Call;
typedef struct Construct Construct;
typedef struct Access Access;
typedef struct MemberAccess MemberAccess;
typedef struct Cast Cast;
typedef struct UnaryPrefix UnaryPrefix;
typedef struct UnaryPostfix UnaryPostfix;
typedef struct BinaryOperation BinaryOperation;

typedef struct DeclarationStatement DeclarationStatement;
typedef struct ExpressionStatement ExpressionStatement;

typedef struct ReturnStatement ReturnStatement;
typedef struct BreakStatement BreakStatement;
typedef struct ContinueStatement ContinueStatement;
typedef struct CaseStatement CaseStatement;
typedef struct DefaultStatement DefaultStatement;

typedef struct ScopeStatement ScopeStatement;

typedef struct IfStatement IfStatement;
typedef struct ForStatement ForStatement;
typedef struct WhileStatement WhileStatement;
typedef struct DoWhileStatement DoWhileStatement;
typedef struct SwitchStatement SwitchStatement;

typedef struct Type Type;
typedef struct Module Module;

typedef enum ExpressionType {
  NullA,
  BoolA,
  CharA,
  IntA,
  FloatA,
  DoubleA,
  StringA,
  IdentifierA,
  VarE,
  BraceE,
  CallE,
  ConstructE,
  AccessE,
  MemberAccessE,
  AsCast,
  UnaryPrefixE,
  UnaryPostfixE,
  BinaryOperationE,
} ExpressionType;

typedef struct Expression {
  union {
    bool b;
    char c[3];
    int i;
    double f;
    const char *s;
    Identifier *id;
    Variable *var;
    Brace *brace;
    Call *call;
    Construct *construct;
    Access *access;
    MemberAccess *member;
    Cast *cast;
    UnaryPrefix *unpre;
    UnaryPostfix *unpost;
    BinaryOperation *binop;
  };
  State localtion;
  ExpressionType type;
} Expression;

typedef enum StatementType {
  ExpressionS,
  Return,
  Break,
  Continue,
  Case,
  Default,
  Scope,
  If,
  For,
  While,
  DoWhile,
  Switch,
} StatementType;

typedef struct Statement Statement;
typedef struct Statement {
  union {
    DeclarationStatement *declare;
    ExpressionStatement *express;
    ReturnStatement *ret;
    BreakStatement *brk;
    ContinueStatement *cont;
    CaseStatement *caseS;
    DefaultStatement *defaultS;
    ScopeStatement *scope;
    IfStatement *ifS;
    ForStatement *forS;
    WhileStatement *whileS;
    DoWhileStatement *doWhileS;
    SwitchStatement *switchS;
  };
  StatementType type;
  State location;
  Statement *next;
} Statement;

typedef struct Function Function;
typedef struct EnumEntry EnumEntry;

typedef struct Type Type;

typedef struct Identifier {
  const char *name;
  Type *type;
} Identifier;

typedef struct Variable {
  const char *name;
  Type *type;
  Variable *next;
} Variable;

typedef struct Parameter Parameter;
typedef struct Parameter {
  Expression *p;
  Identifier *v;
  Parameter *next;
} Parameter;

typedef struct Brace {
  Expression *o;
} Brace;

typedef struct Call {
  Expression *o;
  Parameter *p;
} Call;

typedef struct Construct {
  Type *type;
  Parameter *p;
} Construct;

typedef struct Access {
  Expression *o;
  Expression *p;
} Access;

typedef struct MemberAccess {
  Expression *o;
  Type *o_type;
  Identifier *member;
  bool pointer;
} MemberAccess;

typedef struct Cast {
  Expression *o;
  Type *type;
} Cast;

typedef struct UnaryPrefix {
  Expression *o;
  const char *op;
} UnaryPrefix;

typedef struct UnaryPostfix {
  Expression *o;
  const char *op;
} UnaryPostfix;

enum { ASSOC_NONE = 0, ASSOC_LEFT, ASSOC_RIGHT };
typedef struct BinOp {
  const char *op;
  int prec;
  int assoc;
  bool returns_bool;
} BinOp;
BinOp ops[] = {
    {">>=", 100 - 14, ASSOC_RIGHT, false}, //
    {"<<=", 100 - 14, ASSOC_RIGHT, false}, //
                                           //
    {"==", 100 - 7, ASSOC_LEFT, true},     //
    {"!=", 100 - 7, ASSOC_LEFT, true},     //
    {"<=", 100 - 6, ASSOC_LEFT, true},     //
    {">=", 100 - 6, ASSOC_LEFT, true},     //
    {"+=", 100 - 14, ASSOC_RIGHT, false},  //
    {"-=", 100 - 14, ASSOC_RIGHT, false},  //
    {"*=", 100 - 14, ASSOC_RIGHT, false},  //
    {"/=", 100 - 14, ASSOC_RIGHT, false},  //
    {"%=", 100 - 14, ASSOC_RIGHT, false},  //
    {"&=", 100 - 14, ASSOC_RIGHT, false},  //
    {"^=", 100 - 14, ASSOC_RIGHT, false},  //
    {"|=", 100 - 14, ASSOC_RIGHT, false},  //
    {"&&", 100 - 11, ASSOC_LEFT, true},    //
    {"||", 100 - 12, ASSOC_LEFT, true},    //
    {"<<", 100 - 5, ASSOC_LEFT, false},    //
    {">>", 100 - 5, ASSOC_LEFT, false},    //
                                           //
    {"*", 100 - 3, ASSOC_LEFT, false},     //
    {"/", 100 - 3, ASSOC_LEFT, false},     //
    {"%", 100 - 3, ASSOC_LEFT, false},     //
    {"+", 100 - 4, ASSOC_LEFT, false},     //
    {"-", 100 - 4, ASSOC_LEFT, false},     //
    {"<", 100 - 6, ASSOC_LEFT, false},     //
    {">", 100 - 6, ASSOC_LEFT, false},     //
    {"&", 100 - 8, ASSOC_LEFT, false},     //
    {"^", 100 - 9, ASSOC_LEFT, false},     //
    {"|", 100 - 10, ASSOC_LEFT, false},    //
                                           //
    {"=", 100 - 14, ASSOC_RIGHT, false},   //
};
BinOp *getop(const char *ch) {
  for (int i = 0; i < sizeof ops / sizeof ops[0]; ++i)
    if (strcmp(ops[i].op, ch) == 0)
      return ops + i;
  return NULL;
}

typedef struct BinaryOperation {
  Expression *o1;
  Expression *o2;
  BinOp *op;
} BinaryOperation;

typedef struct ExpressionStatement {
  Expression *e;
  Statement next;
} ExpressionStatement;

typedef struct DeclarationStatement {
  Expression *e;
  Variable *v;
} DeclarationStatement;

typedef struct ReturnStatement {
  Expression *e;
} ReturnStatement;

typedef struct BreakStatement {
} BreakStatement;

typedef struct ContinueStatement {
} ContinueStatement;

typedef struct CaseStatement {
  Expression *caseE;
  Statement *body;
} CaseStatement;

typedef struct DefaultStatement {
  Statement *body;
} DefaultStatement;

typedef struct ScopeStatement {
  Statement *body;
} ScopeStatement;

typedef struct IfStatement {
  Expression *condition;
  Statement *ifBody;
  Statement *elseBody;
} IfStatement;

typedef struct ForStatement {
  Expression *init;
  Expression *condition;
  Expression *incr;
  Statement *body;
} ForStatement;

typedef struct WhileStatement {
  Expression *condition;
  Statement *body;
} WhileStatement;

typedef struct DoWhileStatement {
  Statement *body;
  Expression *condition;
} DoWhileStatement;

typedef struct SwitchStatement {
  Statement *body;
  Expression *condition;
} SwitchStatement;

typedef struct Struct {
  Variable *member;
  Module *module;
} Struct;

typedef struct EnumEntry {
  const char *name;
  int value;
  bool valueSet;
  EnumEntry *next;
} EnumEntry;

typedef struct Enum {
  EnumEntry *entries;
  Module *module;
} Enum;

typedef struct UnionTypeEntry UnionTypeEntry;
typedef struct UnionTypeEntry {
  Type *type;
  int index;
  UnionTypeEntry *next;
} UnionTypeEntry;

typedef struct Union {
  UnionTypeEntry *member;
  Module *module;
} Union;

typedef struct Function {
  Variable *parameter;
  Type *returnType;
  bool is_extern_c;
  Statement *body;
  Module *module;
} Function;

typedef enum TypeKind { ModuleT, StructT, UnionT, EnumT, UnionTypeT, ArrayT, PointerT, FnT } TypeKind;

typedef struct Type {
  const char *name;
  union {
    Struct *structT;
    Enum *enumT;
    Union *unionT;
    Function *fnT;
    Module *moduleT;
    int array_count;
  };
  TypeKind kind;
  Type *child;
} Type;

Module *Type_defined_module(Type *t) {
  switch (t->kind) {
  case StructT:
  case UnionT:
    return t->structT->module;
  case EnumT:
    return t->enumT->module;
  case UnionTypeT:
    return t->unionT->module;
  case FnT:
    return t->fnT->module;
  case ModuleT:
  case ArrayT:
  case PointerT:
    FATALX("invalid type for getting defined module.");
    break;
  }
  return NULL;
}

typedef struct TypeList TypeList;
typedef struct TypeList {
  Type *type;
  TypeList *next;
} TypeList;

typedef struct Module {
  const char *path;
  const char *c_name;
  TypeList *types;
  bool finished;
  Module *next;
} Module;

typedef struct Program {
  struct {
    char *buffer;
    size_t len;
    size_t cap;
  } arena;

  Module *modules;
} Program;

Program Program_new(char *buffer, size_t cap) {
  Program p;
  p.arena.buffer = buffer;
  p.arena.len = 0;
  p.arena.cap = cap;
  p.modules = NULL;
  return p;
}

void *Program_alloc(Program *p, size_t size) {
  if (p->arena.len + size > p->arena.cap)
    FATALX("Out of memory");
  void *d = (p->arena.buffer + p->arena.len);
  p->arena.len += size;
  return d;
}

const char *Program_copy_string(Program *p, const char *s, size_t l) {
  char *id = Program_alloc(p, l + 1);
  strncpy(id, s, l);
  return id;
}

char *readFile(const char *filename) {
  char *buffer = 0;
  long length;
  FILE *f = fopen(filename, "rb");

  if (f) {
    fseek(f, 0, SEEK_END);
    length = ftell(f);
    fseek(f, 0, SEEK_SET);
    buffer = (char *)malloc(length + 1);
    if (buffer) {
      fread(buffer, 1, length, f);
      buffer[length] = 0;
    }
    fclose(f);
  }
  return buffer;
}

Module *Program_find_module(Program *p, const char *path) {
  for (Module *m = p->modules; m; m = m->next)
    if (strcmp(m->path, path) == 0)
      return m;

  return NULL;
}

Module *Program_reset_module_finished(Program *p) {
  for (Module *m = p->modules; m; m = m->next)
    m->finished = false;
  return NULL;
}

Module global = (Module){"", "", NULL, true, NULL};
Type Null = (Type){"null_t", .structT = &(Struct){NULL, &global}, StructT, NULL};
Type Bool = (Type){"bool", .structT = &(Struct){NULL, &global}, StructT, NULL};
Type Char = (Type){"char", .structT = &(Struct){NULL, &global}, StructT, NULL};
Type Int = (Type){"int", .structT = &(Struct){NULL, &global}, StructT, NULL};
Type Float = (Type){"float", .structT = &(Struct){NULL, &global}, StructT, NULL};
Type Double = (Type){"double", .structT = &(Struct){NULL, &global}, StructT, NULL};
Type String = (Type){"string", .structT = &(Struct){NULL, &global}, StructT, NULL};

Type Ellipsis = (Type){"...", .structT = &(Struct){NULL, &global}, StructT, NULL};

Function print =
    (Function){&(Variable){"...", &Ellipsis, &(Variable){"format", &String, NULL}}, NULL, true, NULL, &global};
Type Printf = (Type){"printf", .fnT = &print, FnT, NULL};
Function assert = (Function){&(Variable){"cond", &Bool, NULL}, NULL, true, NULL, &global};
Type Assert = (Type){"ASSERT", .fnT = &assert, FnT, NULL};

Type *Module_find_type(Program *p, Module *m, const char *b, const char *e) {
  if (4 == e - b && strncmp(Bool.name, b, 4) == 0)
    return &Bool;
  if (3 == e - b && strncmp(Int.name, b, 3) == 0)
    return &Int;
  if (4 == e - b && strncmp(Char.name, b, 4) == 0)
    return &Char;
  if (5 == e - b && strncmp(Float.name, b, 5) == 0)
    return &Float;
  if (6 == e - b && strncmp(Double.name, b, 5) == 0)
    return &Double;
  if (6 == e - b && strncmp(String.name, b, 6) == 0)
    return &String;
  if (6 == e - b && strncmp(Printf.name, b, 6) == 0)
    return &Printf;
  if (6 == e - b && strncmp(Assert.name, b, 6) == 0)
    return &Assert;

  for (TypeList *tl = m->types; tl; tl = tl->next)
    if (strlen(tl->type->name) == e - b && strncmp(tl->type->name, b, e - b) == 0)
      return tl->type;
  return NULL;
}

bool TypeDeclare_equal(Type *t1, Type *t2) {
  if (!t1 && !t2)
    return false;
  if (t1 == &Null)
    return t2 == &Null || t2->kind == PointerT;
  if (t2 == &Null)
    return t1 == &Null || t1->kind == PointerT;
  if ((!t1 && t2) || (t1 && !t2))
    return false;
  if (t1 == t2)
    return true;
  if (t1->kind != t2->kind)
    return false;
  return TypeDeclare_equal(t1->child, t2->child);
}

Module *Program_add_module(Program *p, const char *pathc) {
  size_t size = strlen(pathc) + 1;
  const char *path = Program_copy_string(p, pathc, size);
  char *cname = (char *)Program_alloc(p, size + 1);
  strcpy(cname, pathc);
  for (char *c = cname; *c; ++c)
    if (*c == '.')
      *c = '_';
  cname[size - 1] = '_';
  cname[size] = 0;
  Module *m = (Module *)Program_alloc(p, sizeof(Module));
  m->path = path;
  m->c_name = cname;
  m->types = NULL;
  m->next = p->modules;
  p->modules = m;
  return m;
}

Type *Program_add_type(Program *p, TypeKind k, const char *name, Module *m) {
  Type *tt = (Type *)Program_alloc(p, sizeof(Type));
  tt->name = name;
  tt->kind = k;
  switch (k) {
  case ModuleT:
    tt->moduleT = NULL;
    break;
  case StructT:
  case UnionT:
    tt->structT = (Struct *)Program_alloc(p, sizeof(Struct));
    tt->structT->member = NULL;
    tt->structT->module = m;
    break;
  case EnumT:
    tt->enumT = (Enum *)Program_alloc(p, sizeof(Enum));
    tt->enumT->entries = NULL;
    tt->enumT->module = m;
    break;
  case UnionTypeT:
    tt->unionT = (Union *)Program_alloc(p, sizeof(Union));
    tt->unionT->member = NULL;
    tt->unionT->module = m;
    break;
  case FnT:
    tt->fnT = Program_alloc(p, sizeof(Function));
    tt->fnT->body = NULL;
    tt->fnT->is_extern_c = false;
    tt->fnT->module = m;
    break;
  case ArrayT:
    tt->array_count = 0;
    // fallthrough
  case PointerT:
    FATALX("Only base types are implemented to add types");
    break;
  }

  TypeList *tl = (TypeList *)Program_alloc(p, sizeof(TypeList));
  tl->type = tt;
  tl->next = m->types;
  m->types = tl;
  return tt;
}

Variable *Program_new_variable(Program *p, Variable *next) {
  Variable *v = Program_alloc(p, sizeof(Variable));
  v->next = next;
  return v;
}

Statement *Program_new_Statement(Program *p, StatementType t, State l, Statement *n) {
  Statement *s = Program_alloc(p, sizeof(Statement));
  s->type = t;
  s->location = l;
  switch (t) {
  case ExpressionS:
    s->express = Program_alloc(p, sizeof(ExpressionStatement));
    break;
  case Return:
    s->ret = Program_alloc(p, sizeof(ReturnStatement));
    break;
  case Break:
    s->brk = Program_alloc(p, sizeof(BreakStatement));
    break;
  case Default:
    s->defaultS = Program_alloc(p, sizeof(DefaultStatement));
    break;
  case Continue:
    s->cont = Program_alloc(p, sizeof(ContinueStatement));
    break;
  case Case:
    s->caseS = Program_alloc(p, sizeof(CaseStatement));
    break;
  case Scope:
    s->scope = Program_alloc(p, sizeof(ScopeStatement));
    break;
  case If:
    s->ifS = Program_alloc(p, sizeof(IfStatement));
    break;
  case For:
    s->forS = Program_alloc(p, sizeof(ForStatement));
    break;
  case While:
    s->whileS = Program_alloc(p, sizeof(WhileStatement));
    break;
  case DoWhile:
    s->doWhileS = Program_alloc(p, sizeof(DoWhileStatement));
    break;
  case Switch:
    s->switchS = Program_alloc(p, sizeof(SwitchStatement));
    break;
  }
  s->next = n;
  return s;
}

Expression *Program_new_Expression(Program *p, ExpressionType t, State l) {
  Expression *e = Program_alloc(p, sizeof(Expression));
  e->type = t;
  e->localtion = l;
  switch (t) {
  case NullA:
  case BoolA:
  case CharA:
  case IntA:
  case FloatA:
  case DoubleA:
  case StringA:
    break;
  case IdentifierA:
    e->id = Program_alloc(p, sizeof(Identifier));
    break;
  case VarE:
    e->var = Program_alloc(p, sizeof(Variable));
    break;
  case BraceE:
    e->brace = Program_alloc(p, sizeof(Brace));
    break;
  case CallE:
    e->call = Program_alloc(p, sizeof(Call));
    break;
  case ConstructE:
    e->construct = Program_alloc(p, sizeof(Construct));
    break;
  case AccessE:
    e->access = Program_alloc(p, sizeof(Access));
    break;
  case MemberAccessE:
    e->access = Program_alloc(p, sizeof(MemberAccess));
    break;
  case AsCast:
    e->access = Program_alloc(p, sizeof(Cast));
    break;
  case UnaryPrefixE:
    e->unpre = Program_alloc(p, sizeof(UnaryPrefix));
    break;
  case UnaryPostfixE:
    e->unpost = Program_alloc(p, sizeof(UnaryPostfix));
    break;
  case BinaryOperationE:
    e->binop = Program_alloc(p, sizeof(BinaryOperation));
    break;
  }
  return e;
}

bool skip_whitespace_space(State *st) {
  State old = *st;
  while (*st->c && isspace(*st->c)) {
    if (*st->c == '\n') {
      st->line++;
      st->column = 1;
    } else
      ++st->column;
    ++st->c;
  }
  return old.c > st->c;
}

bool skip_line_comment(State *st) {
  if (st->c[0] && st->c[0] == '/' && st->c[1] == '/') {
    while (*st->c) {
      if (*st->c == '\n') {
        st->line++;
        st->column = 1;
        ++st->c;
        break;
      } else {
        ++st->column;
        ++st->c;
      }
    }
    return true;
  }
  return false;
}

void skip_whitespace(State *st) {
  while (skip_whitespace_space(st) || skip_line_comment(st))
    ;
}

bool check_whitespace_for_nl(State *st) {
  if (st->line > 1)
    for (int i = 1; i <= st->column; ++i)
      if (!isspace(st->c[-i]))
        return false;
  return st->line > 1;
}

bool check_word(State *st, const char *word) {
  skip_whitespace(st);
  State old = *st;
  while (st->c[0] && *word && *word == st->c[0]) {
    State_skip(st, 1);
    ++word;
  }
  if (*word == 0 && !isalnum(st->c[0]) && st->c[0] != '_')
    return true;
  *st = old;
  return false;
}

bool check_op(State *st, const char *op) {
  skip_whitespace(st);
  State old = *st;
  while (st->c[0] && op[0] && op[0] == st->c[0]) {
    State_skip(st, 1);
    ++op;
  }
  if (op[0] == 0)
    return true;
  *st = old;
  return false;
}

bool check_identifier(State *st) {
  State old = *st;
  if (st->c[0] && (isalpha(st->c[0]) || st->c[0] == '_')) {
    while (st->c[0] && (isalnum(st->c[0]) || st->c[0] == '_'))
      State_skip(st, 1);
  }
  if (old.c < st->c)
    return true;
  *st = old;
  return false;
}

bool read_int(State *st, int *i) {
  char *end;
  *i = strtol(st->c, &end, 10);
  if (end == st->c)
    return false;
  st->column += end - st->c;
  st->c = end;
  return true;
}

bool read_float(State *st, double *f) {
  char *end;
  *f = strtod(st->c, &end);
  if (end == st->c)
    return false;
  st->column += end - st->c;
  st->c = end;
  return true;
}

const char *read_identifier(Program *p, State *st) {
  State old = *st;
  if (check_identifier(st))
    return Program_copy_string(p, old.c, st->c - old.c);
  return NULL;
}

Module *Program_parse_file(Program *p, const char *path, State *st);

bool Program_parse_use_path(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State old = *st;

  char buffer[256] = {0};
  char *name = buffer;
  size_t inb = 0;
  if (check_identifier(st)) {
    inb += snprintf(buffer + inb, 256 - inb, "%.*s", (int)(st->c - old.c), old.c);
    old = *st;
    for (;;) {
      if (!check_op(st, "."))
        break;
      inb += snprintf(buffer + inb, 256 - inb, ".");
      skip_whitespace(st);
      old = *st;

      if (!check_identifier(st))
        FATAL(st, "Error parsing use statement!");
      name = buffer + inb;
      inb += snprintf(buffer + inb, 256 - inb, "%.*s", (int)(st->c - old.c), old.c);
      old = *st;
    }
  } else
    return false;

  Module *use = Program_parse_file(p, buffer, &old);

  char *n = Program_alloc(p, strlen(name) + 1);
  strcpy(n, name);

  Program_add_type(p, ModuleT, n, m)->moduleT = use;
  return true;
}

bool Program_parse_check_declared_type(Program *p, State *st) {
  skip_whitespace(st);
  State old = *st;

  if (check_op(st, "*")) {
    if (Program_parse_check_declared_type(p, st))
      return true;
    *st = old;
  }

  if (check_op(st, "[")) {
    int count = -1;
    read_int(st, &count);
    if (check_op(st, "]")) {
      if (Program_parse_check_declared_type(p, st))
        return true;
    }
    *st = old;
  }

  if (check_identifier(st)) {
    old = *st;
    if (check_op(st, ".")) {
      skip_whitespace(st);
      if (check_identifier(st))
        return true;
      *st = old;
    }
    return true;
  }

  *st = old;
  return false;
}

Type *Program_parse_declared_type(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State old = *st;

  if (check_op(st, "*")) {
    Type *c = Program_parse_declared_type(p, m, st);
    if (c) {
      Type *td = Program_alloc(p, sizeof(Type));
      td->kind = PointerT;
      td->child = c;
      return td;
    }
    *st = old;
  }

  if (check_op(st, "[")) {
    int count = -1;
    read_int(st, &count);
    if (check_op(st, "]")) {
      Type *c = Program_parse_declared_type(p, m, st);
      if (c) {
        Type *td = Program_alloc(p, sizeof(Type));
        td->kind = ArrayT;
        td->array_count = count;
        td->child = c;
        return td;
      }
    }
    *st = old;
  }

  if (check_identifier(st)) {
    Type *t = Module_find_type(p, m, old.c, st->c);
    if (t) {
      old = *st;
      if (t->kind == ModuleT && check_op(st, ".")) {
        skip_whitespace(st);
        const char *s = st->c;
        if (check_identifier(st)) {
          if ((t = Module_find_type(p, t->moduleT, s, st->c)))
            return t;
        }
      }
      *st = old;
      return t;
    }
  }

  *st = old;
  return NULL;
}

Variable *Program_parse_variable_declaration(Program *p, Module *m, State *st, Variable *next) {
  State old = *st;
  Type *type = NULL;
  if (check_identifier(st)) {
    const char *name_end = st->c;
    skip_whitespace(st);
    if (!check_whitespace_for_nl(st))
      if ((type = Program_parse_declared_type(p, m, st))) {
        Variable *top = Program_new_variable(p, next);
        top->name = Program_copy_string(p, old.c, name_end - old.c);
        top->type = type;
        return top;
      }
    *st = old;
  }
  return NULL;
}

Variable *Program_parse_variable_declaration_list(Program *p, Module *m, State *st, const char *end) {
  skip_whitespace(st);
  State old = *st;
  Variable *top = NULL;
  while (*st->c) {
    if (check_op(st, end))
      return top;
    Variable *tn = Program_parse_variable_declaration(p, m, st, top);
    if (!tn)
      FATAL(st, "missing variable name");
    top = tn;
    check_op(st, ",");
  }
  FATAL(st, "Missing closing '%s'", end);
  return NULL;
}

EnumEntry *Program_parse_enum_entry_list(Program *p, State *st) {
  skip_whitespace(st);
  State old = *st;
  EnumEntry *top = NULL;
  while (*st->c) {
    if (check_op(st, "}"))
      return top;

    EnumEntry *e = Program_alloc(p, sizeof(EnumEntry));
    if ((e->name = read_identifier(p, st))) {
      e->valueSet = false;
      if (check_op(st, "=")) {
        e->valueSet = true;
        if (!read_int(st, &e->value))
          FATAL(st, "missing enum entry value ");
      }
    } else
      FATAL(st, "missing enum entry name");
    check_op(st, ",");
    e->next = top;
    top = e;
  }
  FATAL(st, "Missing closing '}' for enum");
  return NULL;
}

UnionTypeEntry *Program_parse_union_entry_list(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State old = *st;
  UnionTypeEntry *top = NULL;
  int size = 0;
  while (*st->c) {
    if (check_op(st, "}")) {
      for (UnionTypeEntry *ua = top; ua; ua = ua->next)
        ua->index = size--;
      return top;
    }

    size++;
    UnionTypeEntry *e = Program_alloc(p, sizeof(UnionTypeEntry));
    e->type = Program_parse_declared_type(p, m, st);
    check_op(st, ",");
    e->next = top;
    top = e;
  }
  FATAL(st, "Missing closing '}' for enum");
  return NULL;
}
void Program_parse_type(Program *p, Module *m, State *st) {
  skip_whitespace(st);

  const char *name = NULL;
  if ((name = read_identifier(p, st))) {
    bool is_union = check_word(st, "union");
    if ((is_union || check_word(st, "struct")) && check_op(st, "{")) {
      Struct *s = Program_add_type(p, is_union ? UnionT : StructT, name, m)->structT;
      s->member = Program_parse_variable_declaration_list(p, m, st, "}");
    } else if (check_word(st, "enum") && check_op(st, "{")) {
      Enum *e = Program_add_type(p, EnumT, name, m)->enumT;
      e->entries = Program_parse_enum_entry_list(p, st);
    } else if (check_word(st, "uniontype") && check_op(st, "{")) {
      Union *u = Program_add_type(p, UnionTypeT, name, m)->unionT;
      u->member = Program_parse_union_entry_list(p, m, st);
    } else
      FATAL(st, "Missing type declaration");
  } else
    FATAL(st, "Missing type name");
}

Expression *Program_parse_atom(Program *p, State *st) {
  skip_whitespace(st);
  State old = *st;
  if (check_identifier(st)) {
    size_t l = st->c - old.c;

    bool isTrue = l == 4 && strncmp(old.c, "true", 4) == 0;
    if (isTrue || (l == 5 && strncmp(old.c, "false", 5) == 0)) {
      Expression *b = Program_new_Expression(p, BoolA, old);
      b->b = isTrue;
      return b;
    }

    if (l == 4 && strncmp(old.c, "null", 4) == 0) {
      return Program_new_Expression(p, NullA, old);
    }

    Expression *e = Program_new_Expression(p, IdentifierA, old);
    e->id->name = Program_copy_string(p, old.c, st->c - old.c);
    e->id->type = NULL;
    return e;
  }

  if (check_op(st, "'")) {
    Expression *e = Program_new_Expression(p, CharA, old);
    if (!st->c[0])
      FATAL(st, "unclosed char constant");
    if (st->c[0] == '\\') {
      e->c[0] = st->c[0];
      e->c[1] = st->c[1];
      e->c[2] = '\0';
      State_skip(st, 2);
    } else {
      e->c[0] = st->c[0];
      e->c[1] = '\0';
      State_skip(st, 1);
    }
    if (st->c[0] != '\'')
      FATAL(st, "Wrong char constant '%s' '%c'", e->c, st->c[0]);
    State_skip(st, 1);
    return e;
  }

  if (check_op(st, "\"")) {
    Expression *e = Program_new_Expression(p, StringA, old);
    while (st->c[1] != '"') {
      if (!st->c[0])
        FATAL(&old, "unclosed string constant");
      State_skip(st, 1);
    }
    State_skip(st, 1);

    e->s = Program_copy_string(p, old.c + 1, st->c - old.c - 1);
    State_skip(st, 1);
    return e;
  }

  double temp_f;
  State f = *st;
  bool is_float = read_float(&f, &temp_f);

  int temp_i;
  if (read_int(st, &temp_i) && (!is_float || st->c >= f.c)) {
    Expression *e = Program_new_Expression(p, IntA, old);
    e->i = temp_i;
    return e;
  } else if (is_float) {
    *st = f;
    Expression *e = Program_new_Expression(p, DoubleA, old);
    if (st->c[0] == 'f') {
      e->type = FloatA;
      State_skip(st, 1);
    }
    e->f = temp_f;
    return e;
  }

  *st = old;
  return NULL;
}

Expression *Program_parse_expression(Program *p, Module *m, State *st);

Parameter *Program_parse_parameter_list(Program *p, Module *m, State *st) {
  Parameter *param = NULL;
  Expression *e = NULL;

  while ((e = Program_parse_expression(p, m, st))) {
    Parameter *pp = Program_alloc(p, sizeof(Parameter));
    pp->next = param;
    pp->p = e;
    pp->v = NULL;
    param = pp;
    if (!check_op(st, ","))
      break;
  }
  return param;
}

Parameter *Program_parse_named_parameter_list(Program *p, Module *m, State *st) {
  Parameter *param = NULL;

  for (;;) {
    skip_whitespace(st);
    State old = *st;
    if (!check_identifier(st))
      break;
    const char *id_end = st->c;
    if (!check_op(st, ":")) {
      *st = old;
      break;
    }
    Expression *e = Program_parse_expression(p, m, st);
    if (!e) {
      *st = old;
      break;
    }
    Parameter *pp = Program_alloc(p, sizeof(Parameter));
    pp->next = param;
    pp->p = e;
    pp->v = Program_alloc(p, sizeof(Identifier));
    pp->v->name = Program_copy_string(p, old.c, id_end - old.c);
    pp->v->type = NULL;
    param = pp;
    if (!check_op(st, ","))
      break;
  }
  return param;
}

Expression *Program_parse_construction(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State old = *st;

  if (Program_parse_check_declared_type(p, st) && check_op(st, "{")) {
    *st = old;
    Type *type = NULL;
    if ((type = Program_parse_declared_type(p, m, st))) {
      const char *id_end = st->c;
      if (check_op(st, "{")) {
        Expression *construct = Program_new_Expression(p, ConstructE, old);
        construct->construct->p = Program_parse_named_parameter_list(p, m, st);
        if (!construct->construct->p)
          construct->construct->p = Program_parse_parameter_list(p, m, st);
        if (!check_op(st, "}"))
          FATAL(st, "unfinished constructor call, missing '}'");
        construct->construct->type = type;
        return construct;
      }
    }
  }

  *st = old;
  return NULL;
}

Expression *Program_parse_suffix_expression(Program *p, Module *m, State *st, Expression *e) {
  skip_whitespace(st);
  State old = *st;
  if (check_whitespace_for_nl(st))
    return e;

  bool pointer = check_op(st, "->");
  if (pointer || check_op(st, ".")) {
    const char *member = read_identifier(p, st);
    if (!member)
      FATAL(st, "missing id for member access");
    Expression *ma = Program_new_Expression(p, MemberAccessE, old);
    ma->member->o = e;
    ma->member->o_type = NULL;
    ma->member->member = Program_alloc(p, sizeof(Identifier));
    ma->member->member->name = member;
    ma->member->member->type = NULL;
    ma->member->pointer = pointer;
    ma = Program_parse_suffix_expression(p, m, st, ma);
    return ma;
  }

  bool decrement = check_op(st, "--");
  if (decrement || check_op(st, "++")) {
    Expression *post = Program_new_Expression(p, UnaryPostfixE, old);
    post->unpost->o = e;
    post->unpost->op = decrement ? "--" : "++";
    post = Program_parse_suffix_expression(p, m, st, post);
    return post;
  }

  if (check_op(st, "[")) {
    Expression *acc = Program_new_Expression(p, AccessE, old);
    acc->access->o = e;
    acc->access->p = Program_parse_expression(p, m, st);
    if (!acc->access->p)
      FATAL(st, "missing '[]' content");
    if (!check_op(st, "]"))
      FATAL(st, "missing closing ']' for subscription '%s'", st->c);
    acc = Program_parse_suffix_expression(p, m, st, acc);
    return acc;
  }

  if (check_op(st, "(")) {
    Expression *call = Program_new_Expression(p, CallE, old);
    call->call->o = e;
    call->call->p = Program_parse_parameter_list(p, m, st);
    if (!check_op(st, ")"))
      FATAL(st, "unfinished function call, missing ')'");
    call = Program_parse_suffix_expression(p, m, st, call);
    return call;
  }

  if (check_word(st, "as")) {
    Expression *cast = Program_new_Expression(p, AsCast, old);
    cast->cast->o = e;
    cast->cast->type = Program_parse_declared_type(p, m, st);
    return cast;
  }
  return e;
}

Expression *Program_parse_unary_operand(Program *p, Module *m, State *st) {
  Expression *prefix = NULL;
  const char *un_pre_ops[] = {"++", "--", "*", "~", "!", "-", "+", "&"};
  for (int i = 0; i < sizeof(un_pre_ops) / sizeof(const char *); ++i) {
    if (check_op(st, un_pre_ops[i])) {
      prefix = Program_new_Expression(p, UnaryPrefixE, back(st, i < 2 ? 2 : 1));
      prefix->unpre->op = un_pre_ops[i];
      break;
    }
  }

  Expression *e = NULL;
  Variable *temp_v = NULL;
  if (check_op(st, "(")) {
    e = Program_new_Expression(p, BraceE, back(st, 1));
    e->brace->o = Program_parse_expression(p, m, st);
    if (!e->brace->o)
      FATAL(st, "missing '(' content");
    if (!check_op(st, ")"))
      FATAL(st, "missing closing ')'");
  } else if ((e = Program_parse_construction(p, m, st))) {
    ;
  } else if ((temp_v = Program_parse_variable_declaration(p, m, st, NULL))) {
    e = Program_alloc(p, sizeof(Expression));
    e->type = VarE;
    e->var = temp_v;
  } else
    e = Program_parse_atom(p, st);

  if (!e && prefix)
    FATAL(st, "prefix operation without expression '%s'", prefix->unpre->op);
  if (!e)
    return NULL;

  e = Program_parse_suffix_expression(p, m, st, e);
  if (prefix) {
    prefix->unpre->o = e;
    return prefix;
  }
  return e;
}

Expression *Program_parse_bin_operator(Program *p, Module *m, State *st) {
  if (check_whitespace_for_nl(st))
    return NULL;

  for (int i = 0; i < sizeof(ops) / sizeof(BinOp); ++i) {
    if (check_op(st, ops[i].op)) {
      Expression *bin = Program_new_Expression(p, BinaryOperationE, back(st, strlen(ops[i].op)));
      bin->binop->op = &ops[i];
      return bin;
    }
  }
  return NULL;
}

typedef struct ShuntYard {
  Expression *op_stack[128];
  int op_stack_size;
  Expression *val_stack[128];
  int val_stack_size;
} ShuntYard;

ShuntYard ShuntYard_create() { return (ShuntYard){.op_stack_size = 0, .val_stack_size = 0}; }

void ShuntYard_push_val(ShuntYard *y, Expression *e) {
  y->val_stack[y->val_stack_size] = e;
  y->val_stack_size++;
}

void ShuntYard_push_op(ShuntYard *y, Expression *e) {
  y->op_stack[y->op_stack_size] = e;
  y->op_stack_size++;
}

void ShuntYard_shunt(ShuntYard *y) {
  Expression *pop = y->op_stack[y->op_stack_size - 1];
  y->op_stack_size--;

  if (y->val_stack_size < 2)
    FATALX("not enough parameter for binary operation '%s'", pop->binop->op->op);

  pop->binop->o2 = y->val_stack[y->val_stack_size - 1];
  y->val_stack_size--;
  pop->binop->o1 = y->val_stack[y->val_stack_size - 1];
  y->val_stack[y->val_stack_size - 1] = pop;
}

Expression *Program_parse_expression(Program *p, Module *m, State *st) {
  Expression *ev = Program_parse_unary_operand(p, m, st);
  if (!ev)
    return NULL;
  ShuntYard yard = ShuntYard_create();
  ShuntYard_push_val(&yard, ev);

  Expression *eop;
  for (;;) {
    if (check_whitespace_for_nl(st))
      break;
    State old = *st;
    eop = Program_parse_bin_operator(p, m, st);
    if (!eop)
      break;
    ev = Program_parse_unary_operand(p, m, st);
    if (!ev) {
      *st = old;
      break;
    }
    if (eop->binop->op->assoc == ASSOC_RIGHT) {
      while (yard.op_stack_size > 0 && eop->binop->op->prec < yard.op_stack[yard.op_stack_size - 1]->binop->op->prec)
        ShuntYard_shunt(&yard);
    } else {
      while (yard.op_stack_size > 0 && eop->binop->op->prec <= yard.op_stack[yard.op_stack_size - 1]->binop->op->prec)
        ShuntYard_shunt(&yard);
    }
    ShuntYard_push_op(&yard, eop);
    ShuntYard_push_val(&yard, ev);
  }

  while (yard.op_stack_size > 0) {
    ShuntYard_shunt(&yard);
  }
  if (yard.val_stack_size != 1)
    FATALX("Expression parsing failed with too many values (%d)", yard.val_stack_size);
  return yard.val_stack[0];
}

Statement *Program_parse_statement(Program *p, Module *m, State *st, Statement *next);
Statement *Program_parse_case_body(Program *p, Module *m, State *st) {
  Statement *body = NULL;
  Expression *temp_e = NULL;
  while (*st->c) {
    skip_whitespace(st);
    State old = *st;
    if (check_op(st, "}") || check_word(st, "case") || check_word(st, "default")) {
      *st = old;
      return body;
    }
    body = Program_parse_statement(p, m, st, body);
    if (!body)
      break;
  }
  FATAL(st, "Missing closing '}' for scope");
  return NULL;
}

Statement *Program_parse_scope(Program *p, Module *m, State *st);
Statement *Program_parse_scope_block(Program *p, Module *m, State *st);
Statement *Program_parse_statement(Program *p, Module *m, State *st, Statement *next) {
  Statement *statement = NULL;
  Expression *temp_e = NULL;
  Variable *temp_v = NULL;
  if (check_op(st, "{")) {
    statement = Program_new_Statement(p, Scope, back(st, 1), next);
    statement->scope->body = Program_parse_scope(p, m, st);
  } else if (check_word(st, "return")) {
    statement = Program_new_Statement(p, Return, back(st, 6), next);
    statement->express->e = Program_parse_expression(p, m, st);
  } else if (check_word(st, "case")) {
    statement = Program_new_Statement(p, Case, back(st, 4), next);
    if (!(statement->caseS->caseE = Program_parse_atom(p, st)))
      FATAL(st, "Missing case expression");
    if (check_op(st, ":"))
      statement->caseS->body = Program_parse_case_body(p, m, st);
    else
      FATAL(st, "Missing ':' in case statement");
  } else if (check_word(st, "default")) {
    statement = Program_new_Statement(p, Default, back(st, 7), next);
    if (check_op(st, ":"))
      statement->defaultS->body = Program_parse_case_body(p, m, st);
    else
      FATAL(st, "Missing ':' for default statement");
  } else if (check_word(st, "break"))
    statement = Program_new_Statement(p, Break, back(st, 5), next);
  else if (check_word(st, "continue"))
    statement = Program_new_Statement(p, Continue, back(st, 8), next);
  else if (check_word(st, "if")) {
    statement = Program_new_Statement(p, If, back(st, 2), next);
    if ((temp_e = Program_parse_expression(p, m, st)))
      statement->ifS->condition = temp_e;
    else
      FATAL(st, "Missing if conditon");
    statement->ifS->ifBody = Program_parse_scope_block(p, m, st);
    if (!statement->ifS->ifBody)
      FATAL(st, "Missing if block");
    if (check_word(st, "else")) {
      statement->ifS->elseBody = Program_parse_scope_block(p, m, st);
      if (!statement->ifS->elseBody)
        FATAL(st, "Missing else block");
    }
  } else if (check_word(st, "for")) {
    statement = Program_new_Statement(p, For, back(st, 3), next);
    if (!check_op(st, "("))
      FATAL(st, "Missing for loop description");
    statement->forS->init = Program_parse_expression(p, m, st);
    if (!check_op(st, ";"))
      FATAL(st, "Missing for init");
    statement->forS->condition = Program_parse_expression(p, m, st);
    if (!check_op(st, ";"))
      FATAL(st, "Missing for condition");
    statement->forS->incr = Program_parse_expression(p, m, st);
    if (!check_op(st, ")"))
      FATAL(st, "Missing closing ')' of for condition");
    statement->forS->body = Program_parse_scope_block(p, m, st);
    if (!statement->forS->body)
      FATAL(st, "Missing for block");
  } else if (check_word(st, "while")) {
    statement = Program_new_Statement(p, While, back(st, 5), next);
    if ((temp_e = Program_parse_expression(p, m, st)))
      statement->whileS->condition = temp_e;
    else
      FATAL(st, "Missing while conditon");
    statement->whileS->body = Program_parse_scope_block(p, m, st);
    if (!statement->whileS->body)
      FATAL(st, "Missing while block");
  } else if (check_word(st, "do")) {
    statement = Program_new_Statement(p, DoWhile, back(st, 2), next);
    statement->doWhileS->body = Program_parse_scope_block(p, m, st);
    if (!check_word(st, "while"))
      FATAL(st, "Missing 'while' for do block");
    if ((temp_e = Program_parse_expression(p, m, st)))
      statement->doWhileS->condition = temp_e;
    else
      FATAL(st, "Missing do while conditon");
  } else if (check_word(st, "switch")) {
    statement = Program_new_Statement(p, Switch, back(st, 6), next);
    if ((temp_e = Program_parse_expression(p, m, st)))
      statement->switchS->condition = temp_e;
    else
      FATAL(st, "Missing switch expression");
    statement->switchS->body = Program_parse_scope_block(p, m, st);
  } else {
    skip_whitespace(st);
    State old = *st;
    if ((temp_e = Program_parse_expression(p, m, st))) {
      statement = Program_new_Statement(p, ExpressionS, old, next);
      statement->express->e = temp_e;
    }
  }
  if (statement)
    check_op(st, ";");
  return statement;
}
Statement *Program_parse_scope(Program *p, Module *m, State *st) {
  Statement *body = NULL;
  Expression *temp_e = NULL;
  while (*st->c) {
    skip_whitespace(st);
    if (check_op(st, "}"))
      return body;
    body = Program_parse_statement(p, m, st, body);
    if (!body)
      break;
  }
  FATAL(st, "Missing closing '}' for scope");
  return NULL;
}
Statement *Program_parse_scope_block(Program *p, Module *m, State *st) {
  if (check_op(st, "{")) {
    Statement *s = Program_new_Statement(p, Scope, back(st, 1), NULL);
    s->scope->body = Program_parse_scope(p, m, st);
    return s;
  } else
    return Program_parse_statement(p, m, st, NULL);
}

void Program_parse_fn(Program *p, Module *m, State *st, bool extc) {
  skip_whitespace(st);

  const char *name = read_identifier(p, st);
  if (name) {
    Function *fn = Program_add_type(p, FnT, name, m)->fnT;
    if (check_op(st, "(")) {
      fn->parameter = Program_parse_variable_declaration_list(p, m, st, ")");
      fn->returnType = Program_parse_declared_type(p, m, st);
      fn->is_extern_c = extc;
      if (!extc) {
        if (check_op(st, "{"))
          fn->body = Program_parse_scope(p, m, st);
        else
          FATAL(st, "Missing function body");
      }
    } else
      FATAL(st, "Missing parameterlist");
  } else
    FATAL(st, "Missing type name");
}

void Program_parse_module(Program *p, Module *m, State *st) {
  while (st->c[0]) {
    skip_whitespace(st);
    traceStack_push(st);
    if (st->c[0]) {
      if (check_word(st, "use"))
        Program_parse_use_path(p, m, st);
      else if (check_word(st, "type"))
        Program_parse_type(p, m, st);
      else if (check_word(st, "fn"))
        Program_parse_fn(p, m, st, false);
      else if (check_word(st, "cfn"))
        Program_parse_fn(p, m, st, true);
      else {
        FATAL(st, "Unknown keyword >>'%s'\n", st->c);
        break;
      }
    }
    traceStack_pop();
  }
}

Module *Program_parse_file(Program *p, const char *path, State *outer_st) {
  Module *m = Program_find_module(p, path);
  if (m && !m->finished)
    FATAL(outer_st, "Circular dependencies!");
  if (m)
    return m;

  m = Program_add_module(p, path);
  m->finished = false;

  char tempPath[256];
  strcpy(tempPath, path);
  for (char *t = tempPath; *t; ++t)
    if (*t == '.')
      *t = '/';
  strcat(tempPath, ".jnq");

  char *code = readFile(tempPath);
  if (!code)
    FATAL(outer_st, "missing file! '%s'", tempPath);

  char *st_path = Program_alloc(p, strlen(tempPath) + 1);
  strcpy(st_path, tempPath);
  State st = State_new(code, st_path);
  Program_parse_module(p, m, &st);
  m->finished = true;

  free(code);
  return m;
}

void c_Module_types(FILE *f, Module *m);

typedef void (*ModuleFileCB)(FILE *, Module *);

void c_use(FILE *f, TypeList *tl, ModuleFileCB cb) {
  if (tl->next)
    c_use(f, tl->next, cb);
  if (tl->type->kind == ModuleT)
    cb(f, tl->type->moduleT);
}

bool c_type_declare(FILE *f, Type *t, const char *var) {
  if (!t)
    return false;

  Type *tt = t->child;
  while (tt && tt->kind == ArrayT)
    tt = tt->child;
  bool hasVarWritten = c_type_declare(f, tt, var);
  switch (t->kind) {
  case ArrayT:
    fprintf(f, " %s", var);
    while (t && t->kind == ArrayT) {
      if (t->array_count > 0)
        fprintf(f, "[%d]", t->array_count);
      else
        fprintf(f, "[]");
      t = t->child;
    }
    return true;
  case PointerT:
    fprintf(f, "*");
    break;
  case ModuleT:
    fprintf(f, "%s", t->name);
    break;
  case StructT:
  case UnionT:
  case EnumT:
  case UnionTypeT:
    fprintf(f, "%s%s", Type_defined_module(t)->c_name, t->name);
    break;
  case FnT:
    if (c_type_declare(f, t->fnT->returnType, ""))
      FATALX("I don't know right now how to handle array function pointer stuff!");
    fprintf(f, "(*())");
    break;
  }
  return hasVarWritten;
}

void c_enum_entry_list(FILE *f, const char *module_name, EnumEntry *ee) {
  if (!ee)
    return;

  c_enum_entry_list(f, module_name, ee->next);
  if (ee->next)
    fprintf(f, ",\n  ");

  fprintf(f, "%s%s", module_name, ee->name);
  if (ee->valueSet)
    fprintf(f, " = %d", ee->value);
}

void c_enum(FILE *f, const char *module_name, const char *name, EnumEntry *entries) {
  fprintf(f, "typedef enum %s%s", module_name, name);
  if (!entries) {
    fprintf(f, " {} %s%s;\n\n", module_name, name);
    return;
  }

  fprintf(f, " {\n  ");
  c_enum_entry_list(f, module_name, entries);
  fprintf(f, "");
  fprintf(f, "\n} %s%s;\n\n", module_name, name);
}

void c_var_list(FILE *f, Variable *v, const char *br) {
  if (!v)
    return;

  c_var_list(f, v->next, br);
  if (v->next)
    fprintf(f, "%s", br);

  if (!c_type_declare(f, v->type, v->name))
    fprintf(f, " %s", v->name);
}

void c_struct(FILE *f, const char *module_name, const char *name, bool is_union, Variable *member) {
  fprintf(f, "typedef %s %s%s", (is_union ? "union" : "struct"), module_name, name);
  if (!member) {
    fprintf(f, " {} %s%s;\n\n", module_name, name);
    return;
  }

  fprintf(f, " {\n  ");
  c_var_list(f, member, ";\n  ");
  fprintf(f, "");
  fprintf(f, ";\n} %s%s;\n\n", module_name, name);
}

void c_union_enum_entry(FILE *f, const char *module_name, const char *name, UnionTypeEntry *entry) {
  if (!entry)
    return;

  c_union_enum_entry(f, module_name, name, entry->next);

  if (entry->next)
    fprintf(f, ",\n  ");
  fprintf(f, "%s_%s_u%d_t", module_name, name, entry->index);
}

void c_uniontype_entry(FILE *f, UnionTypeEntry *entry) {
  if (!entry)
    return;

  c_uniontype_entry(f, entry->next);

  if (entry->next)
    fprintf(f, ";\n    ");
  if (c_type_declare(f, entry->type, "<u>"))
    FATALX("I don't know right now how to handle array union entry stuff!");
  fprintf(f, " _u%d", entry->index);
}

void c_union_forward(FILE *f, const char *module_name, const char *name, UnionTypeEntry *member) {
  if (member) {
    fprintf(f, "typedef enum %s%sType", module_name, name);
    if (!member) {
      fprintf(f, " {} %s_%sType;\n\n", module_name, name);
      return;
    }

    fprintf(f, " {\n  ");
    c_union_enum_entry(f, module_name, name, member);
    fprintf(f, "");
    fprintf(f, "\n} %s%sType;\n", module_name, name);
  }

  fprintf(f, "typedef struct %s%s %s%s;\n", module_name, name, module_name, name);
}

void c_uniontype(FILE *f, const char *module_name, const char *name, UnionTypeEntry *member) {
  fprintf(f, "typedef struct %s%s", module_name, name);
  if (!member) {
    fprintf(f, " {} %s%s;\n\n", module_name, name);
    return;
  }

  fprintf(f, " {\n  union {\n    ");
  c_uniontype_entry(f, member);
  fprintf(f, ";\n  };\n");
  fprintf(f, "  %s%sType type;", module_name, name);
  fprintf(f, "\n} %s%s;\n\n", module_name, name);
}

void c_type(FILE *f, const char *module_name, TypeList *t) {
  if (!t)
    return;

  c_type(f, module_name, t->next);

  switch (t->type->kind) {
  case ModuleT:
    break;
  case UnionT:
  case StructT:
    c_struct(f, module_name, t->type->name, t->type->kind == UnionT, t->type->structT->member);
    break;
  case EnumT:
    break;
  case UnionTypeT:
    c_uniontype(f, module_name, t->type->name, t->type->unionT->member);
    break;
  case FnT:
    // todo!??
    break;
  case ArrayT:
  case PointerT:
    FATALX("Type declaration not implemented for that kind");
    break;
  }
}

void lisp_expression(FILE *f, Expression *e);

void lisp_parameter(FILE *f, Parameter *param) {
  if (!param)
    return;

  lisp_parameter(f, param->next);
  if (param->next)
    fprintf(f, " ");
  lisp_expression(f, param->p);
}

void lisp_expression(FILE *f, Expression *e) {
  if (!e)
    return;

  switch (e->type) {
  case NullA:
    fprintf(f, "%s", "null");
    break;
  case BoolA:
    fprintf(f, "%s", e->b ? "true" : "false");
  case CharA:
    fprintf(f, "'%s'", e->c);
    break;
  case IntA:
    fprintf(f, "%d", e->i);
    break;
  case FloatA:
  case DoubleA:
    fprintf(f, "%g", e->f);
    break;
  case StringA:
    fprintf(f, "\"%s\"", e->s);
    break;
  case IdentifierA:
    fprintf(f, "%s", e->id->name);
    break;
  case VarE:
    fprintf(f, "(:: %s ", e->var->name);
    for (Type *t = e->var->type; t; t = t->child) {
      switch (t->kind) {
      case ArrayT:
        fprintf(f, "[%d]", t->array_count);
        break;
      case PointerT:
        fprintf(f, "*");
        break;
      case ModuleT:
      case StructT:
      case UnionT:
      case EnumT:
      case UnionTypeT:
        fprintf(f, "%s", t->name);
        break;
      case FnT:
        fprintf(f, "(*())");
        break;
      }
    }
    fprintf(f, ")");
    break;
  case BraceE:
    fprintf(f, "(() ");
    lisp_expression(f, e->brace->o);
    fprintf(f, ")");
    break;
  case CallE:
    fprintf(f, "(__ ");
    lisp_expression(f, e->call->o);
    if (e->call->p)
      fprintf(f, " ");
    lisp_parameter(f, e->call->p);
    fprintf(f, ")");
    break;
  case ConstructE:
    fprintf(f, "(## ");
    for (Type *t = e->construct->type; t; t = t->child) {
      switch (t->kind) {
      case ArrayT:
        fprintf(f, "[%d]", t->array_count);
        break;
      case PointerT:
        fprintf(f, "*");
        break;
      case ModuleT:
      case StructT:
      case UnionT:
      case EnumT:
      case UnionTypeT:
        fprintf(f, "%s", t->name);
        break;
      case FnT:
        FATALX("Construction from Function type not implemented!");
        break;
      }
    }
    if (e->call->p)
      fprintf(f, " ");
    lisp_parameter(f, e->construct->p);
    fprintf(f, ")");
    break;
  case AccessE:
    fprintf(f, "([] ");
    lisp_expression(f, e->access->o);
    fprintf(f, " ");
    lisp_expression(f, e->access->p);
    fprintf(f, ")");
    break;
  case MemberAccessE:
    if (e->member->pointer)
      fprintf(f, "(-> ");
    else
      fprintf(f, "(. ");
    lisp_expression(f, e->member->o);
    fprintf(f, " %s)", e->member->member->name);
    break;
  case AsCast:
    break;
  case UnaryPrefixE:
    fprintf(f, "(%s ", e->unpre->op);
    lisp_expression(f, e->unpre->o);
    fprintf(f, ")");
    break;
  case UnaryPostfixE:
    fprintf(f, "(>>%s ", e->unpost->op);
    lisp_expression(f, e->unpost->o);
    fprintf(f, ")");
    break;
  case BinaryOperationE:
    fprintf(f, "(%s ", e->binop->op->op);
    lisp_expression(f, e->binop->o1);
    fprintf(f, " ");
    lisp_expression(f, e->binop->o2);
    fprintf(f, ")");
    break;
  }
}

void c_expression(FILE *f, Expression *e);

void c_parameter(FILE *f, Parameter *param) {
  if (!param)
    return;

  c_parameter(f, param->next);
  if (param->next)
    fprintf(f, ", ");
  if (param->v)
    fprintf(f, ".%s = ", param->v->name);
  c_expression(f, param->p);
}

void c_expression(FILE *f, Expression *e) {
  if (!e)
    return;

  switch (e->type) {
  case NullA:
    fprintf(f, "%s", "NULL");
    break;
  case BoolA:
    fprintf(f, "%s", e->b ? "true" : "false");
    break;
  case CharA:
    fprintf(f, "'%s'", e->c);
    break;
  case IntA:
    fprintf(f, "%d", e->i);
    break;
  case FloatA:
  case DoubleA:
    fprintf(f, "%g", e->f);
    break;
  case StringA:
    fprintf(f, "\"%s\"", e->s);
    break;
  case IdentifierA:
    if (!e->id->type)
      FATAL(&e->localtion, "unknown type for id '%s'", e->id->name);
    if (e->id->type->kind == FnT && !e->id->type->fnT->is_extern_c)
      fprintf(f, "%s", Type_defined_module(e->id->type)->c_name);
    fprintf(f, "%s", e->id->name);
    break;
  case VarE:
    c_var_list(f, e->var, ";");
    break;
  case BraceE:
    fprintf(f, "(");
    c_expression(f, e->brace->o);
    fprintf(f, ")");
    break;
  case CallE:
    c_expression(f, e->call->o);
    if (e->call->o->type != MemberAccessE)
      fprintf(f, "(");
    else if (e->call->p &&
             (e->call->o->member->o->type != IdentifierA || e->call->o->member->o->id->type->kind != ModuleT))
      fprintf(f, ", ");
    c_parameter(f, e->call->p);
    fprintf(f, ")");
    break;
  case ConstructE: {
    if (e->construct->type->kind == UnionTypeT) {
      fprintf(f, "(%s%s){", Type_defined_module(e->construct->type)->c_name, e->construct->type->name);
      if (!e->construct->p->v)
        FATAL(&e->localtion, "internal error: no union entry selected");
      UnionTypeEntry *ua = (UnionTypeEntry *)e->construct->p->v;
      fprintf(f, "._u%d = ", ua->index);
      c_expression(f, e->construct->p->p);
      fprintf(f, ", .type = %s_%s_u%d_t}", Type_defined_module(e->construct->type)->c_name, e->construct->type->name,
              ua->index);
    } else {
      if (e->construct->type->kind == StructT || e->construct->type->kind == UnionT)
        fprintf(f, "(%s%s){", Type_defined_module(e->construct->type)->c_name, e->construct->type->name);
      else if (e->construct->type->kind == ArrayT)
        fprintf(f, "{");
      else
        FATAL(&e->localtion, "unexpect type for construction (or missing impementation)");
      c_parameter(f, e->construct->p);
      fprintf(f, "}");
    }
    break;
  }
  case AccessE:
    c_expression(f, e->access->o);
    fprintf(f, "[");
    c_expression(f, e->access->p);
    fprintf(f, "]");
    break;
  case MemberAccessE: {
    if (!e->member->member->type)
      FATAL(&e->localtion, "unknown type for member '%s'", e->member->member->name);
    switch (e->member->member->type->kind) {
    case EnumT:
      fprintf(f, "%s%s", Type_defined_module(e->member->member->type)->c_name, e->member->member->name);
      break;
    case ModuleT:
    case StructT:
    case UnionT:
    case UnionTypeT:
    case PointerT:
      c_expression(f, e->member->o);
      fprintf(f, "%s%s", (e->member->pointer ? "->" : "."), e->member->member->name);
      break;

    case FnT: {
      if (!e->member->member->type->fnT->parameter)
        FATAL(&e->localtion, "internal error creating member function call!");
      Variable *first = e->member->member->type->fnT->parameter;
      while (first->next)
        first = first->next;
      const char *prefix = "";
      if (e->member->pointer && first->type->kind != PointerT)
        prefix = "*";
      else if (!e->member->pointer && first->type->kind == PointerT)
        prefix = "&";
      if (!e->member->member->type->fnT->is_extern_c)
        fprintf(f, "%s", Type_defined_module(e->member->member->type)->c_name);
      fprintf(f, "%s(%s", e->member->member->name, prefix);
      if (e->member->o->type != IdentifierA || e->member->o->id->type->kind != ModuleT)
        c_expression(f, e->member->o);
      break;
    }

    case ArrayT:
      FATAL(&e->localtion, "array type has no member '%s'", e->member->member->name);
      break;
    }
    break;
  }
  case AsCast:
    fprintf(f, "((");
    if (c_type_declare(f, e->cast->type, ""))
      FATAL(&e->localtion, "I don't know right now how to handle array cast  stuff!");
    fprintf(f, ")(");
    c_expression(f, e->cast->o);
    fprintf(f, "))");
    break;
  case UnaryPrefixE:
    fprintf(f, "%s", e->unpre->op);
    c_expression(f, e->unpre->o);
    break;
  case UnaryPostfixE:
    c_expression(f, e->unpost->o);
    fprintf(f, "%s", e->unpost->op);
    break;
  case BinaryOperationE:
    c_expression(f, e->binop->o1);
    fprintf(f, " %s ", e->binop->op->op);
    c_expression(f, e->binop->o2);
    break;
  }
}

const char *SPACE = "                                                ";
void c_statements(FILE *f, Statement *s, int indent);
void c_scope_as_body(FILE *f, Statement *s, int indent) {
  if (s->type == Scope) {
    fprintf(f, " {\n");
    c_statements(f, s->scope->body, indent + 2);
    fprintf(f, "%.*s}", indent, SPACE);
  } else {
    fprintf(f, "\n");
    c_statements(f, s, indent + 2);
  }
}
void c_statements(FILE *f, Statement *s, int indent) {
  if (!s)
    return;

  c_statements(f, s->next, indent);
  fprintf(f, "%.*s", indent, SPACE);
  switch (s->type) {
  case ExpressionS:
    c_expression(f, s->express->e);
    fprintf(f, ";\n");
    break;
  case Return:
    fprintf(f, "return ");
    c_expression(f, s->ret->e);
    fprintf(f, ";\n");
    break;
  case Break:
    fprintf(f, "break;\n");
    break;
  case Continue:
    fprintf(f, "continue;\n");
    break;
  case Case:
    fprintf(f, "case ");
    c_expression(f, s->caseS->caseE);
    fprintf(f, ": {\n");
    c_statements(f, s->caseS->body, indent + 2);
    fprintf(f, "%.*sbreak;\n", indent + 2, SPACE);
    fprintf(f, "%.*s}\n", indent, SPACE);
    break;
  case Default:
    fprintf(f, "default: {\n");
    c_statements(f, s->defaultS->body, indent + 2);
    fprintf(f, "%.*sbreak;\n", indent + 2, SPACE);
    fprintf(f, "%.*s}\n", indent, SPACE);
    break;
  case Scope:
    fprintf(f, "{\n");
    c_statements(f, s->scope->body, indent + 2);
    fprintf(f, "%.*s}\n", indent, SPACE);
    break;
  case If:
    fprintf(f, "if ");
    c_expression(f, s->ifS->condition);
    c_scope_as_body(f, s->ifS->ifBody, indent);
    if (s->ifS->elseBody) {
      fprintf(f, "%.*s", (s->ifS->ifBody->type == Scope ? 1 : indent), SPACE);
      fprintf(f, "else");
      c_scope_as_body(f, s->ifS->elseBody, indent);
      if (s->ifS->elseBody->type == Scope)
        fprintf(f, "\n");
    } else if (s->ifS->ifBody->type == Scope)
      fprintf(f, "\n");
    break;
  case For:
    fprintf(f, "for (");
    if (!s->forS->init && !s->forS->condition && !s->forS->incr)
      fprintf(f, ";;");
    else {
      c_expression(f, s->forS->init);
      fprintf(f, "; ");
      c_expression(f, s->forS->condition);
      fprintf(f, "; ");
      c_expression(f, s->forS->incr);
    }
    fprintf(f, ")");
    c_scope_as_body(f, s->forS->body, indent);
    if (s->forS->body->type == Scope)
      fprintf(f, "\n");
    break;
  case While:
    fprintf(f, "while ");
    c_expression(f, s->whileS->condition);
    c_scope_as_body(f, s->whileS->body, indent);
    if (s->whileS->body->type == Scope)
      fprintf(f, "\n");
    break;
  case DoWhile:
    fprintf(f, "do");
    c_scope_as_body(f, s->doWhileS->body, indent);
    fprintf(f, "%.*s", (s->doWhileS->body->type == Scope ? 1 : indent), SPACE);
    fprintf(f, "while ");
    c_expression(f, s->doWhileS->condition);
    fprintf(f, ";\n");
    break;
  case Switch:
    fprintf(f, "switch ");
    c_expression(f, s->switchS->condition);
    c_scope_as_body(f, s->doWhileS->body, indent);
    break;
  }
}

void c_fn(FILE *f, const char *module_name, TypeList *tl) {
  if (!tl)
    return;

  c_fn(f, module_name, tl->next);
  if (tl->type->kind != FnT || tl->type->fnT->is_extern_c)
    return;

  Function *fn = tl->type->fnT;
  if (fn->returnType) {
    if (c_type_declare(f, fn->returnType, ""))
      FATALX("array return type not supported -> c backend!");
  } else
    fprintf(f, "void");
  if (strcmp(tl->type->name, "main") == 0)
    fprintf(f, " %s(", tl->type->name);
  else
    fprintf(f, " %s%s(", module_name, tl->type->name);
  if (fn->parameter)
    c_var_list(f, fn->parameter, ", ");
  else
    fprintf(f, "void");
  if (!fn->body)
    fprintf(f, ") {}\n\n");
  else {
    fprintf(f, ") {\n");
    c_statements(f, fn->body, 2);
    fprintf(f, "}\n\n");
  }
}

void c_Module_types(FILE *f, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  if (m->types) {
    fprintf(f, "\n");
    c_use(f, m->types, c_Module_types);
    fprintf(f, "\n");
    c_type(f, m->c_name, m->types);
  }
}

void c_Module_fn(FILE *f, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  if (m->types) {
    fprintf(f, "\n");
    c_use(f, m->types, c_Module_fn);
    fprintf(f, "\n");
    c_fn(f, m->c_name, m->types);
  }
}

void c_type_forward(FILE *f, const char *module_name, TypeList *t) {
  if (!t)
    return;

  c_type_forward(f, module_name, t->next);

  switch (t->type->kind) {
  case StructT:
    fprintf(f, "typedef struct %s%s %s%s;\n", module_name, t->type->name, module_name, t->type->name);
    break;
  case UnionT:
    fprintf(f, "typedef union %s%s %s%s;\n", module_name, t->type->name, module_name, t->type->name);
    break;
  case EnumT:
    c_enum(f, module_name, t->type->name, t->type->enumT->entries);
    break;
  case UnionTypeT:
    c_union_forward(f, module_name, t->type->name, t->type->unionT->member);
    break;
  case ModuleT:
    break;
  case FnT:
    // todo?!?
    break;
  case ArrayT:
  case PointerT:
    FATALX("unexpect type in forward declaration!");
    break;
  }
}

void c_Module_forward_types(FILE *f, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  if (m->types) {
    fprintf(f, "\n");
    c_use(f, m->types, c_Module_forward_types);
    fprintf(f, "\n");
    c_type_forward(f, m->c_name, m->types);
  }
}

void c_fn_forward(FILE *f, const char *module_name, TypeList *tl) {
  if (!tl)
    return;

  c_fn_forward(f, module_name, tl->next);
  if (tl->type->kind != FnT || tl->type->fnT->is_extern_c)
    return;

  Function *fn = tl->type->fnT;
  if (fn->returnType) {
    if (c_type_declare(f, fn->returnType, ""))
      FATALX("array return type not supported -> c backend!");
  } else
    fprintf(f, "void");
  if (strcmp(tl->type->name, "main") == 0)
    fprintf(f, " %s(", tl->type->name);
  else
    fprintf(f, " %s%s(", module_name, tl->type->name);
  if (fn->parameter)
    c_var_list(f, fn->parameter, ", ");
  else
    fprintf(f, "void");
  fprintf(f, ");\n");
}

void c_Module_forward_fn(FILE *f, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  if (m->types) {
    fprintf(f, "\n");
    c_use(f, m->types, c_Module_forward_fn);
    fprintf(f, "\n");
    c_fn_forward(f, m->c_name, m->types);
  }
}

typedef struct StackVar {
  const char *name;
  Type *type;
} StackVar;

typedef struct VariableStack {
  StackVar stack[256];
  int stackSize;
} VariableStack;

void VariableStack_push(VariableStack *s, const char *n, Type *t) {
  if (s->stackSize >= 256)
    FATALX("Variable stack limit reached");
  s->stack[s->stackSize] = (StackVar){n, t};
  s->stackSize++;
}

Type *VariableStack_find(VariableStack *s, const char *n) {
  for (int i = s->stackSize - 1; i >= 0; i--)
    if (strcmp(s->stack[i].name, n) == 0)
      return s->stack[i].type;
  return NULL;
}

bool is_member_fn_for(Type *ot, Type *ft, Identifier *member) {
  if (ft->kind != FnT)
    return false;
  Function *f = ft->fnT;
  if (!f->parameter || strcmp(ft->name, member->name) != 0)
    return false;

  Variable *first = f->parameter;
  while (first->next)
    first = first->next;

  if (TypeDeclare_equal(first->type, ot))
    return true;

  if (first->type->kind == PointerT && TypeDeclare_equal(first->type->child, ot))
    return true;

  return false;
}

Type *Module_find_member(Program *p, Type *t, Identifier *member) {
  switch (t->kind) {
  case UnionT:
  case StructT: {
    for (Variable *v = t->structT->member; v; v = v->next)
      if (strcmp(v->name, member->name) == 0) {
        return v->type;
      }
    break;
  }
  case EnumT: {
    for (EnumEntry *ee = t->enumT->entries; ee; ee = ee->next)
      if (strcmp(ee->name, member->name) == 0)
        return t;
    break;
  }
  case UnionTypeT:
    // union?? -> some build in ()
    break;
  case ModuleT:
    return Module_find_type(p, t->moduleT, member->name, member->name + strlen(member->name));

  case ArrayT:
  case PointerT:
  case FnT:
    FATALX("internal error, call of Module_find_member with wrong type");
    break;
  }
  Module *m = Type_defined_module(t);
  if (m) {
    for (TypeList *tl = m->types; tl; tl = tl->next) {
      if (is_member_fn_for(t, tl->type, member))
        return tl->type;
    }
  }

  return NULL;
}

Type *c_Expression_make_variables_typed(VariableStack *s, Program *p, Module *m, Expression *e) {
  if (!e)
    return NULL;

  switch (e->type) {
  case NullA:
    return &Null;
  case BoolA:
    return &Bool;
  case CharA:
    return &Char;
  case IntA:
    return &Int;
  case FloatA:
    return &Float;
  case DoubleA:
    return &Double;
  case StringA:
    return &String;

  case IdentifierA: {
    if (e->id->type)
      return e->id->type;
    if ((e->id->type = VariableStack_find(s, e->id->name)))
      return e->id->type;
    if ((e->id->type = Module_find_type(p, m, e->id->name, e->id->name + strlen(e->id->name))))
      return e->id->type;
    FATAL(&e->localtion, "unknown type for '%s'", e->id->name);
    return NULL;
  }
  case VarE:
    VariableStack_push(s, e->var->name, e->var->type);
    return e->var->type;
  case BraceE:
    return c_Expression_make_variables_typed(s, p, m, e->brace->o);
  case CallE: {
    Type *t = c_Expression_make_variables_typed(s, p, m, e->call->o);
    if (!t || t->kind != FnT)
      FATAL(&e->localtion, "Need a function to be called!");
    for (Parameter *pa = e->call->p; pa; pa = pa->next)
      c_Expression_make_variables_typed(s, p, m, pa->p);

    Parameter *pa = e->call->p;
    Variable *va = t->fnT->parameter;
    while (pa && va) {
      pa = pa->next;
      va = va->next;
    }
    if (va && !pa && e->call->o->type == MemberAccessE) {
      if (!e->call->o->member->o_type)
        FATAL(&e->call->o->localtion, "Missing type on member access");
      va = va->next;
      // compare pa->type with e->call->o->member->o_type
    }
    if (pa && t->fnT->parameter->type != &Ellipsis)
      FATAL(&e->localtion, "To much parameter for function call");
    if (va && t->fnT->parameter->type != &Ellipsis)
      FATAL(&e->localtion, "Missing parameter for function call");
    return t->fnT->returnType;
  }
  case ConstructE: {
    if (e->construct->type->kind == ArrayT) {
      for (Parameter *pa = e->construct->p; pa; pa = pa->next) {
        Type *pt = c_Expression_make_variables_typed(s, p, m, pa->p);
        Type *et = e->construct->type->child;
        if (!TypeDeclare_equal(pt, et))
          FATAL(&e->localtion, "Type missmatch for array element of '%s'!", e->construct->type->child->name);
      }
    } else if (e->construct->type->kind == UnionTypeT) {
      if (e->construct->p->next)
        FATAL(&e->localtion, "A union type could be only of one kind");
      if (e->construct->p->v)
        FATAL(&e->localtion, "Named initialisation of a union das not work");
      Type *pt = c_Expression_make_variables_typed(s, p, m, e->construct->p->p);
      UnionTypeEntry *ua = e->construct->type->unionT->member;
      for (; ua; ua = ua->next)
        if (TypeDeclare_equal(pt, ua->type))
          break;
      if (!ua)
        FATAL(&e->localtion, "type '%s' not supported by '%s'", pt->name, e->construct->type->name);
      e->construct->p->v = (Identifier *)ua; // unsafe
    } else if (e->construct->type->kind == StructT || e->construct->type->kind == UnionT) {
      for (Parameter *pa = e->construct->p; pa; pa = pa->next) {
        Type *pt = c_Expression_make_variables_typed(s, p, m, pa->p);
        if (pa->v) {
          if (e->construct->type->kind != StructT && e->construct->type->kind != UnionT)
            FATAL(&pa->p->localtion, "Named construction '%s' for none struct type!", pa->v->name);
          Type *vt = Module_find_member(p, e->construct->type, pa->v);
          if (!vt)
            FATAL(&pa->p->localtion, "Type '%s' has no member '%s'!", e->construct->type->name, pa->v->name);
          if (!TypeDeclare_equal(pt, vt))
            FATAL(&pa->p->localtion, "Type missmatch for member '%s' of '%s'!", pa->v->name, e->construct->type->name);
          pa->v->type = pt;
        } else {
          ; // ToDo check type in order
        }
      }
    } else {
      FATAL(&e->localtion, "construction not possible (or not implemented)");
    }
    return e->construct->type;
  }
  case AccessE: {
    Type *subt = c_Expression_make_variables_typed(s, p, m, e->access->p);
    if (subt != &Int)
      FATAL(&e->access->p->localtion, "Expect integral type for array subscription");
    Type *t = c_Expression_make_variables_typed(s, p, m, e->access->o);
    if (t->kind != ArrayT && t->kind != PointerT)
      FATAL(&e->localtion, "Expect array/pointer type for access");
    return t->child;
  }
  case MemberAccessE: {
    Type *t = c_Expression_make_variables_typed(s, p, m, e->member->o);
    if (t->kind == PointerT) {
      t = t->child;
      e->member->pointer = true;
    }
    if (t->kind != StructT && t->kind != UnionT && t->kind != EnumT && t->kind != UnionTypeT && t->kind != ModuleT)
      FATAL(&e->localtion, "Expect non pointer type for member access");
    if (!(e->member->member->type = Module_find_member(p, t, e->member->member)))
      FATAL(&e->localtion, "unknown member '%s' for '%s'", e->member->member->name, t->name);
    e->member->o_type = t;
    return e->member->member->type;
  }
  case AsCast:
    c_Expression_make_variables_typed(s, p, m, e->cast->o);
    // check if cast is valid!
    return e->cast->type;
  case UnaryPrefixE: {
    Type *st = c_Expression_make_variables_typed(s, p, m, e->unpost->o);
    if (strcmp(e->unpre->op, "&") == 0) {
      Type *td = Program_alloc(p, sizeof(Type));
      td->kind = PointerT;
      td->child = st;
      return td;
    } else if (strcmp(e->unpre->op, "*") == 0) {
      if (st->kind != PointerT)
        FATAL(&e->localtion, "dereferenceing none pointer type!");
      return st->child;
    }
    return st;
  }
  case UnaryPostfixE:
    return c_Expression_make_variables_typed(s, p, m, e->unpre->o);
  case BinaryOperationE: {
    Type *t1 = c_Expression_make_variables_typed(s, p, m, e->binop->o1);
    Type *t2 = c_Expression_make_variables_typed(s, p, m, e->binop->o2);
    if (!TypeDeclare_equal(t1, t2)) {
      lisp_expression(stderr, e);
      fprintf(stderr, "\n");
      lisp_expression(stderr, e->binop->o1);
      fprintf(stderr, "\n");
      lisp_expression(stderr, e->binop->o2);
      fprintf(stderr, "\n");
      FATAL(&e->localtion, "Expect equal types for binary operation '%s' (%s, %s) (%d, %d)", e->binop->op->op, t1->name,
            t2->name, t1->kind, t2->kind);
    }
    if (e->binop->op->returns_bool)
      return &Bool;
    return t1;
  }
  }
  FATAL(&e->localtion, "unknown type for expression!");
  return NULL;
}

void c_Statement_make_variables_typed(VariableStack *s, Program *p, Module *m, Statement *st) {
  if (!st)
    return;

  c_Statement_make_variables_typed(s, p, m, st->next);

  switch (st->type) {
  case ExpressionS:
    c_Expression_make_variables_typed(s, p, m, st->express->e);
    break;
  case Return:
    if (st->ret->e)
      c_Expression_make_variables_typed(s, p, m, st->ret->e);
    break;
  case Break:
  case Continue:
    break;
  case Case: {
    int size = s->stackSize;
    c_Expression_make_variables_typed(s, p, m, st->caseS->caseE);
    c_Statement_make_variables_typed(s, p, m, st->caseS->body);
    s->stackSize = size;
    break;
  }
  case Default: {
    int size = s->stackSize;
    c_Statement_make_variables_typed(s, p, m, st->defaultS->body);
    s->stackSize = size;
    break;
  }
  case Scope: {
    int size = s->stackSize;
    c_Statement_make_variables_typed(s, p, m, st->scope->body);
    s->stackSize = size;
    break;
  }
  case If: {
    int size = s->stackSize;
    c_Expression_make_variables_typed(s, p, m, st->ifS->condition);
    int sizeI = s->stackSize;
    c_Statement_make_variables_typed(s, p, m, st->ifS->ifBody);
    s->stackSize = sizeI;
    if (st->ifS->elseBody)
      c_Statement_make_variables_typed(s, p, m, st->ifS->elseBody);
    s->stackSize = size;
    break;
  }
  case For: {
    int size = s->stackSize;
    c_Expression_make_variables_typed(s, p, m, st->forS->init);
    c_Expression_make_variables_typed(s, p, m, st->forS->condition);
    c_Expression_make_variables_typed(s, p, m, st->forS->incr);
    c_Statement_make_variables_typed(s, p, m, st->forS->body);
    s->stackSize = size;
    break;
  }
  case While: {
    int size = s->stackSize;
    c_Expression_make_variables_typed(s, p, m, st->whileS->condition);
    c_Statement_make_variables_typed(s, p, m, st->whileS->body);
    s->stackSize = size;
    break;
  }
  case DoWhile: {
    int size = s->stackSize;
    c_Statement_make_variables_typed(s, p, m, st->doWhileS->body);
    s->stackSize = size;
    c_Expression_make_variables_typed(s, p, m, st->doWhileS->condition);
    break;
  }
  case Switch: {
    int size = s->stackSize;
    c_Expression_make_variables_typed(s, p, m, st->switchS->condition);
    c_Statement_make_variables_typed(s, p, m, st->switchS->body);
    s->stackSize = size;
    break;
  }
  }
}

void c_Function_make_variables_typed(VariableStack *s, Program *p, Module *m, Function *f) {
  int size = s->stackSize;
  for (Variable *p = f->parameter; p; p = p->next)
    VariableStack_push(s, p->name, p->type);

  c_Statement_make_variables_typed(s, p, m, f->body);

  s->stackSize = size;
}

void c_Module_make_variables_typed(Program *p, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  for (TypeList *tl = m->types; tl; tl = tl->next)
    if (tl->type->kind == ModuleT)
      c_Module_make_variables_typed(p, tl->type->moduleT);

  VariableStack stack = (VariableStack){};
  for (TypeList *tl = m->types; tl; tl = tl->next)
    if (tl->type->kind == FnT)
      c_Function_make_variables_typed(&stack, p, m, tl->type->fnT);
}

void c_Program(FILE *f, Program *p, Module *m) {

  fputs("#include <ctype.h>\n", f);
  fputs("#include <stdarg.h>\n", f);
  fputs("#include <stdbool.h>\n", f);
  fputs("#include <stddef.h>\n", f);
  fputs("#include <stdio.h>\n", f);
  fputs("#include <stdlib.h>\n", f);
  fputs("#include <string.h>\n", f);
  fputs("#include <math.h>\n", f);
  fputs("\n", f);

  fputs("#define ASSERT(EXP)                          \\\n", f);
  fputs("  do {                                       \\\n", f);
  fputs("    if (!(EXP)) {                                \\\n", f);
  fputs("      fprintf(stderr, \"FAILED: '%s'\\n \", #EXP); \\\n", f);
  fputs("      exit(1); \\\n", f);
  fputs("    } \\\n", f);
  fputs("  } while (0)\n", f);

  Program_reset_module_finished(p);
  c_Module_make_variables_typed(p, m);

  Program_reset_module_finished(p);
  c_Module_forward_types(f, m);

  Program_reset_module_finished(p);
  c_Module_types(f, m);

  Program_reset_module_finished(p);
  c_Module_forward_fn(f, m);

  Program_reset_module_finished(p);
  c_Module_fn(f, m);
}

int main(int argc, char *argv[]) {
  if (argc <= 1)
    FATALX("missing input file\n");

  int jnq_len = strlen(argv[1]);
  if (jnq_len < 4 || strcmp(argv[1] + (jnq_len - 4), ".jnq") != 0)
    FATALX("invalid input file '%s'\n", argv[1]);

  char main_mod[256] = {0};
  if (jnq_len > 255)
    FATALX("input path too long '%s' (sorry)\n", argv[1]);
  strncpy(main_mod, argv[1], jnq_len - 4);

  traceStack.size = 0;

  char buffer[1024 * 64];
  Program p = Program_new(buffer, 1024 * 64);

  Module *m = Program_parse_file(&p, main_mod, &(State){.file = argv[1], .line = 0, .column = 0});

  char main_c[256] = {0};
  strncpy(main_c, argv[1], jnq_len - 4);
  main_c[jnq_len - 4] = '_';
  main_c[jnq_len - 3] = '.';
  main_c[jnq_len - 2] = 'c';
  main_c[jnq_len - 1] = 0;

  if (access(main_c, F_OK) == 0)
    FATALX("temp file already exisits '%s'", main_c);
  if (access(JNQ_BIN, F_OK) == 0)
    FATALX("temp bin already exisits '" JNQ_BIN "'");

  FILE *c_tmp_file = fopen(main_c, "w");
  if (!c_tmp_file)
    FATALX("could not create temp file '%s'", main_c);

  int error = setjmp(traceStack.back);
  if (error == 0)
    c_Program(c_tmp_file, &p, m);
  fclose(c_tmp_file);

  if (error == 0)
    error = setjmp(traceStack.back);
  if (error == 0) {
    char clang_call[256] = {0};
    // strcat(clang_call, "cat ");
    // strcat(clang_call, main_c);
    // system(clang_call);
    // clang_call[0] = 0;
    strcat(clang_call, "clang -o " JNQ_BIN " -Werror -g "
#ifndef _WIN32
                       "-lm "
#endif
    );
    strcat(clang_call, main_c);
    error = system(clang_call);
    if (error != 0)
      FATALX("failed to compile c '%s'", main_c);
  }
  if (error == 0)
    error = system(JNQ_BIN);

  remove(main_c);
  remove(JNQ_BIN);
#ifdef _WIN32
  remove("jnq_bin.ilk");
  remove("jnq_bin.pdb");
#endif

  return error;
}
