
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct State {
  const char *file;
  const char *c;
  size_t line;
  size_t column;
} State;

State State_new(const char *c, const char *file) { return (State){.file = file, .c = c, .line = 1, .column = 1}; }

struct {
  State states[128];
  int size;
} traceStack;

void FATAL(State *s, const char *format, ...);
void traceStack_push(State *s) {
  if (traceStack.size == 128)
    FATAL(&(State){.file = s->file, .line = 0, .column = 0}, "not enough buffer for stack trace");
  traceStack.states[traceStack.size] = *s;
  traceStack.size++;
}

void traceStack_pop() { traceStack.size--; }

void FATAL(State *st, const char *format, ...) {
  va_list args;
  fprintf(stderr, "%s:%zu:%zu error: ", st->file, st->line, st->column);
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  for (int i = traceStack.size - 1; i >= 0; --i) {
    State *sst = &traceStack.states[i];
    fprintf(stderr, " - %s:%zu:%zu\n", sst->file, sst->line, sst->column);
  }

  exit(1);
}

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
  BoolA,
  CharA,
  IntA,
  FloatA,
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
    char c;
    int i;
    float f;
    const char *s;
    const char *id;
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
  Statement *next;
} Statement;

typedef enum TypeDeclareType { ArrayT, PointerT, TypeT } TypeDeclareType;
typedef struct TypeDeclare TypeDeclare;
typedef struct TypeDeclare {
  union {
    Type *t;
    int count;
  };
  TypeDeclareType type;
  TypeDeclare *next;
} TypeDeclare;

typedef struct Variable {
  const char *name;
  TypeDeclare *type;
  Variable *next;
} Variable;

typedef struct Parameter Parameter;
typedef struct Parameter {
  Expression *p;
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
  const char *member;
  bool pointer;
} MemberAccess;

typedef struct Cast {
  Expression *o;
  TypeDeclare *type;
} Cast;

typedef struct UnaryPrefix {
  Expression *o;
  const char *op;
} UnaryPrefix;

typedef struct UnaryPostfix {
  Expression *o;
  const char *op;
} UnaryPostfix;

typedef struct BinaryOperation {
  Expression *o1;
  Expression *o2;
  const char *op;
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

typedef struct Function Function;
typedef struct Function {
  const char *name;
  Variable *parameter;
  TypeDeclare *returnType;
  Statement *body;
  Function *next;
} Function;

typedef struct Use Use;
typedef struct Use {
  Module *use;
  Use *next;
} Use;

typedef struct EnumEntry EnumEntry;
typedef struct EnumEntry {
  const char *name;
  int value;
  bool valueSet;
  EnumEntry *next;
} EnumEntry;

typedef struct UnionEntry UnionEntry;
typedef struct UnionEntry {
  TypeDeclare *type;
  UnionEntry *next;
} UnionEntry;

typedef enum TypeKind { Klass, Enum, Union } TypeKind;

typedef struct Type {
  const char *name;
  union {
    Variable *member;
    EnumEntry *entries;
    UnionEntry *union_member;
  };
  TypeKind kind;
  Type *next;
} Type;

typedef struct Module {
  const char *path;
  Use *use;
  Type *types;
  Function *fn;
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
    FATAL(&(State){.file = "", .line = 0, .column = 0}, "Out of memory");
  void *d = (p->arena.buffer + p->arena.len);
  p->arena.len += size;
  return d;
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

Type Bool = (Type){"bool", NULL, Klass, NULL};
Type Char = (Type){"char", NULL, Klass, NULL};
Type Int = (Type){"int", NULL, Klass, NULL};
Type Float = (Type){"float", NULL, Klass, NULL};
Type String = (Type){"string", NULL, Klass, NULL};

Type *Module_find_type(Program *p, Module *m, const char *b, const char *e) {
  if (4 == e - b && strncmp(Bool.name, b, 4) == 0)
    return &Bool;
  if (3 == e - b && strncmp(Int.name, b, 3) == 0)
    return &Int;
  if (4 == e - b && strncmp(Char.name, b, 4) == 0)
    return &Bool;
  if (5 == e - b && strncmp(Float.name, b, 5) == 0)
    return &Float;
  if (6 == e - b && strncmp(String.name, b, 6) == 0)
    return &String;

  for (Type *tt = m->types; tt; tt = tt->next) {
    if (strlen(tt->name) == e - b && strncmp(tt->name, b, e - b) == 0)
      return tt;
  }
  for (Use *u = m->use; u; u = u->next)
    for (Type *tt = u->use->types; tt; tt = tt->next)
      if (strlen(tt->name) == e - b && strncmp(tt->name, b, e - b) == 0)
        return tt;
  return NULL;
}

Module *Program_add_module(Program *p, const char *pathc) {
  size_t size = strlen(pathc) + 1;
  char *path = (char *)Program_alloc(p, size);
  strcpy(path, pathc);
  Module *m = (Module *)Program_alloc(p, sizeof(Module));
  m->path = path;
  m->use = NULL;
  m->types = NULL;
  m->fn = NULL;
  m->next = p->modules;
  p->modules = m;
  return m;
}

Use *Program_add_use(Program *p, Module *m, Module *use) {
  Use *u = Program_alloc(p, sizeof(Use));
  u->use = use;
  u->next = m->use;
  m->use = u;
  return u;
}

Type *Program_add_type(Program *p, TypeKind k, const char *name, Module *m) {
  Type *tt = (Type *)Program_alloc(p, sizeof(Type));
  tt->name = name;
  tt->kind = k;
  switch (k) {
  case Klass:
    tt->member = NULL;
    break;
  case Enum:
    tt->entries = NULL;
    break;
  case Union:
    tt->union_member = NULL;
    break;
  }
  tt->next = m->types;
  m->types = tt;
  return tt;
}

Function *Program_add_fn(Program *p, Module *m) {
  Function *fn = Program_alloc(p, sizeof(Function));
  fn->body = NULL;
  fn->next = m->fn;
  m->fn = fn;
  return fn;
}

Variable *Program_new_variable(Program *p, Variable *next) {
  Variable *v = Program_alloc(p, sizeof(Variable));
  v->next = next;
  return v;
}

Statement *Program_new_Statement(Program *p, StatementType t, Statement *n) {
  Statement *s = Program_alloc(p, sizeof(Statement));
  s->type = t;
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

Expression *Program_new_Expression(Program *p, ExpressionType t) {
  Expression *e = Program_alloc(p, sizeof(Expression));
  e->type = t;
  switch (t) {
  case BoolA:
  case CharA:
  case IntA:
  case FloatA:
  case StringA:
  case IdentifierA:
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

void skip_whitespace(State *st) {
  while (*st->c && isspace(*st->c)) {
    if (*st->c == '\n') {
      st->line++;
      st->column = 1;
    } else
      ++st->column;
    ++st->c;
  }
}

bool check_whitespace_for_nl(State *st) {
  if (st->line > 1)
    for (int i = 1; i <= st->column; ++i)
      if (!isspace(st->c[-i]))
        return false;
  return true;
}

bool check_word(State *st, const char *word) {
  skip_whitespace(st);
  State old = *st;
  while (st->c[0] && *word && *word == st->c[0]) {
    ++st->column;
    ++st->c;
    ++word;
  }
  if (*word == 0 && !isalnum(st->c[0]))
    return true;
  *st = old;
  return false;
}

bool check_op(State *st, const char *op) {
  skip_whitespace(st);
  State old = *st;
  while (st->c[0] && op[0] && op[0] == st->c[0]) {
    ++st->column;
    ++st->c;
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
    while (st->c[0] && (isalnum(st->c[0]) || st->c[0] == '_')) {
      ++st->column;
      ++st->c;
    }
  }
  if (old.c < st->c)
    return true;
  *st = old;
  return false;
}

bool read_int(State *st, int *i) {
  State old = *st;
  if (isdigit(*st->c)) {
    while (*st->c && isdigit(*st->c)) {
      ++st->column;
      ++st->c;
    }
    char buf[128] = {0};
    if (st->c - old.c > 126)
      FATAL(&old, "Buffer to short for integer conversion!");
    strncpy(buf, old.c, st->c - old.c);
    *i = atoi(buf);
    return true;
  }
  return false;
}

const char *read_identifier(Program *p, State *st) {
  State old = *st;
  if (check_identifier(st)) {
    char *id = Program_alloc(p, st->c - old.c + 1);
    strncpy(id, old.c, st->c - old.c);
    return id;
  }
  return NULL;
}

Module *Program_parse_file(Program *p, const char *path, State *st);

bool Program_parse_use_path(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State old = *st;

  char buffer[256] = {0};
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
      inb += snprintf(buffer + inb, 256 - inb, "%.*s", (int)(st->c - old.c), old.c);
      old = *st;
    }
  } else
    return false;

  Module *use = Program_parse_file(p, buffer, &old);
  Program_add_use(p, m, use);
  return true;
}

TypeDeclare *Program_parse_declared_type(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State old = *st;

  if (check_op(st, "*")) {
    TypeDeclare *td = Program_alloc(p, sizeof(TypeDeclare));
    td->type = PointerT;
    td->next = Program_parse_declared_type(p, m, st);
    if (!td->next)
      FATAL(st, "missing type name");
    return td;
  }

  if (check_op(st, "[")) {
    TypeDeclare *td = Program_alloc(p, sizeof(TypeDeclare));
    td->type = ArrayT;
    if (!read_int(st, &td->count))
      FATAL(&old, "Unknown array size!");
    if (!check_op(st, "]"))
      FATAL(&old, "missing closing ']'!");
    td->next = Program_parse_declared_type(p, m, st);
    if (!td->next)
      FATAL(st, "missing type name");
    return td;
  }

  if (check_identifier(st)) {
    Type *t = Module_find_type(p, m, old.c, st->c);
    if (!t) {
      *st = old;
      return NULL;
    }
    TypeDeclare *td = Program_alloc(p, sizeof(TypeDeclare));
    td->type = TypeT;
    td->t = t;
    td->next = NULL;
    return td;
  }

  *st = old;
  return NULL;
}

Variable *Program_parse_variable_declaration(Program *p, Module *m, State *st, Variable *next) {
  State old = *st;
  const char *name = NULL;
  TypeDeclare *type = NULL;
  if ((name = read_identifier(p, st))) {
    skip_whitespace(st);
    if (!check_whitespace_for_nl(st))
      if ((type = Program_parse_declared_type(p, m, st))) {
        Variable *top = Program_new_variable(p, next);
        top->name = name;
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
        const bool neg = check_op(st, "-");
        if (!read_int(st, &e->value))
          FATAL(st, "missing enum entry value ");
        if (neg)
          e->value *= -1;
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

UnionEntry *Program_parse_union_entry_list(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State old = *st;
  UnionEntry *top = NULL;
  while (*st->c) {
    if (check_op(st, "}"))
      return top;

    UnionEntry *e = Program_alloc(p, sizeof(UnionEntry));
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
    if (check_word(st, "struct") && check_op(st, "{")) {
      Type *c = Program_add_type(p, Klass, name, m);
      c->member = Program_parse_variable_declaration_list(p, m, st, "}");
    } else if (check_word(st, "enum") && check_op(st, "{")) {
      Type *e = Program_add_type(p, Enum, name, m);
      e->entries = Program_parse_enum_entry_list(p, st);
    } else if (check_word(st, "union") && check_op(st, "{")) {
      Type *u = Program_add_type(p, Union, name, m);
      u->union_member = Program_parse_union_entry_list(p, m, st);
    } else
      FATAL(st, "Missing type declaration");
  } else
    FATAL(st, "Missing type name");
}

Expression *Program_parse_atom(Program *p, State *st) {
  skip_whitespace(st);
  State old = *st;
  if (check_identifier(st)) {
    Expression *e = Program_new_Expression(p, IdentifierA);
    char *id = Program_alloc(p, st->c - old.c + 1);
    strncpy(id, old.c, st->c - old.c);
    e->id = id;
    return e;
  }

  if (check_op(st, "'")) {
    Expression *e = Program_new_Expression(p, CharA);
    if (!st->c[0] || st->c[1] != '\'')
      FATAL(st, "Wrong char constant");
    e->c = st->c[0];
    ++st->column;
    ++st->c;
    ++st->column;
    ++st->c;
    return e;
  }

  if (check_op(st, "\"")) {
    Expression *e = Program_new_Expression(p, StringA);
    while (st->c[1] != '"') {
      if (!st->c[0])
        FATAL(&old, "unclosed string constant");
      ++st->column;
      ++st->c;
    }
    ++st->column;
    ++st->c;
    char *str = Program_alloc(p, st->c - old.c);
    strncpy(str, old.c + 1, st->c - old.c - 1);
    e->s = str;
    ++st->column;
    ++st->c;
    return e;
  }

  int temp_i;
  if (read_int(st, &temp_i)) {
    Expression *e = Program_new_Expression(p, IntA);
    e->i = temp_i;
    return e;
  }

  *st = old;
  return NULL;
}

Expression *Program_parse_expression(Program *p, Module *m, State *st, bool ignoreSuffix);

Parameter *Program_parse_parameter_list(Program *p, Module *m, State *st) {
  Parameter *param = NULL;
  Expression *e = NULL;

  while ((e = Program_parse_expression(p, m, st, false))) {
    Parameter *pp = Program_alloc(p, sizeof(Parameter));
    pp->next = param;
    pp->p = e;
    param = pp;
    if (!check_op(st, ","))
      break;
  }
  return param;
}

Expression *Program_parse_construction(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State old = *st;
  if (check_identifier(st)) {
    const char *id_end = st->c;
    if (check_op(st, "{")) {
      Expression *construct = Program_new_Expression(p, ConstructE);
      construct->construct->p = Program_parse_parameter_list(p, m, st);
      if (!check_op(st, "}"))
        FATAL(st, "unfinished constructor call, missing '}'");
      if (!(construct->construct->type = Module_find_type(p, m, old.c, id_end)))
        FATAL(st, "Unknow type for construction '%.*s'", (int)(id_end - old.c), old.c);

      return construct;
    }
  }

  *st = old;
  return NULL;
}

Expression *Program_parse_expression_suffix(Program *p, Module *m, State *st, Expression *e) {

  bool pointer = check_op(st, "->");
  if (pointer || check_op(st, ".")) {
    const char *member = read_identifier(p, st);
    if (!member)
      FATAL(st, "missing id for member access");
    Expression *ma = Program_new_Expression(p, MemberAccessE);
    ma->member->o = e;
    ma->member->member = member;
    ma->member->pointer = pointer;
    ma = Program_parse_expression_suffix(p, m, st, ma);
    return ma;
  }

  bool decrement = check_op(st, "--");
  if (decrement || check_op(st, "++")) {
    Expression *post = Program_new_Expression(p, UnaryPostfixE);
    post->unpost->o = e;
    post->unpost->op = decrement ? "--" : "++";
    post = Program_parse_expression_suffix(p, m, st, post);
    return post;
  }

  if (check_op(st, "[")) {
    Expression *acc = Program_new_Expression(p, AccessE);
    acc->access->o = e;
    acc->access->p = Program_parse_expression(p, m, st, false);
    if (!acc->access->o)
      FATAL(st, "missing '[]' content");
    if (!check_op(st, "]"))
      FATAL(st, "missing closing ']'");
    acc = Program_parse_expression_suffix(p, m, st, acc);
    return acc;
  }

  if (check_op(st, "(")) {
    Expression *call = Program_new_Expression(p, CallE);
    call->call->o = e;
    call->call->p = Program_parse_parameter_list(p, m, st);
    if (!check_op(st, ")"))
      FATAL(st, "unfinished function call, missing ')'");
    call = Program_parse_expression_suffix(p, m, st, call);
    return call;
  }

  if (check_word(st, "as")) {
    Expression *cast = Program_new_Expression(p, AsCast);
    cast->cast->o = e;
    cast->cast->type = Program_parse_declared_type(p, m, st);
    return cast;
  }
  return e;
}

Expression *Program_parse_binary_operation(Program *p, Module *m, State *st, Expression *e, Expression *prefix) {

  e = Program_parse_expression_suffix(p, m, st, e);
  if (prefix) {
    prefix->unpre->o = e;
    e = prefix;
  }
  if (check_whitespace_for_nl(st))
    return e;

  const char *bin_ops[] = {">>=", "<<=", "==", "!=", ">=", "<=", "+=", "-=", "*=", "/=", "%=", "&=", "|=",
                           "^=",  "&&",  "||", ">>", "<<", "+",  "-",  "*",  "/",  "%",  "&",  "|",  "="};
  for (int i = 0; i < sizeof(bin_ops) / sizeof(const char *); ++i) {
    if (check_op(st, bin_ops[i])) {
      Expression *bin = Program_new_Expression(p, BinaryOperationE);
      bin->binop->o1 = e;
      bin->binop->op = bin_ops[i];
      bin->binop->o2 = Program_parse_expression(p, m, st, false);
      if (!bin->binop->o2)
        FATAL(st, "Missing second operand for '%s' operator", bin_ops[i]);
      return bin;
    }
  }
  return e;
}

Expression *Program_parse_expression(Program *p, Module *m, State *st, bool ignoreSuffix) {

  Expression *prefix = NULL;
  const char *un_pre_ops[] = {"++", "--", "*", "~", "!", "-", "+", "&"};
  for (int i = 0; i < sizeof(un_pre_ops) / sizeof(const char *); ++i) {
    if (check_op(st, un_pre_ops[i])) {
      prefix = Program_new_Expression(p, UnaryPrefixE);
      prefix->unpre->op = un_pre_ops[i];
      break;
    }
  }

  Expression *e = NULL;
  Variable *temp_v = NULL;
  if (check_op(st, "(")) {
    e = Program_new_Expression(p, BraceE);
    e->brace->o = Program_parse_expression(p, m, st, false);
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

  if (!ignoreSuffix)
    e = Program_parse_binary_operation(p, m, st, e, prefix);

  return e;
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
    statement = Program_new_Statement(p, Scope, next);
    statement->scope->body = Program_parse_scope(p, m, st);
  } else if (check_word(st, "return")) {
    statement = Program_new_Statement(p, Return, next);
    statement->express->e = Program_parse_expression(p, m, st, false);
  } else if (check_word(st, "case")) {
    statement = Program_new_Statement(p, Case, next);
    if (!(statement->caseS->caseE = Program_parse_expression(p, m, st, false)))
      FATAL(st, "Missing case expression");
    if (check_op(st, ":"))
      statement->caseS->body = Program_parse_case_body(p, m, st);
    else
      FATAL(st, "Missing ':' in case statement");
  } else if (check_word(st, "default")) {
    statement = Program_new_Statement(p, Default, next);
    if (check_op(st, ":"))
      statement->defaultS->body = Program_parse_case_body(p, m, st);
    else
      FATAL(st, "Missing ':' for default statement");
  } else if (check_word(st, "break"))
    statement = Program_new_Statement(p, Break, next);
  else if (check_word(st, "continue"))
    statement = Program_new_Statement(p, Continue, next);
  else if (check_word(st, "if")) {
    statement = Program_new_Statement(p, If, next);
    if ((temp_e = Program_parse_expression(p, m, st, false)))
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
    statement = Program_new_Statement(p, For, next);
    if (!check_op(st, "("))
      FATAL(st, "Missing for conditon");
    statement->forS->init = Program_parse_expression(p, m, st, false);
    if (!check_op(st, ";"))
      FATAL(st, "Missing for init");
    statement->forS->condition = Program_parse_expression(p, m, st, false);
    if (!check_op(st, ";"))
      FATAL(st, "Missing for condition");
    statement->forS->incr = Program_parse_expression(p, m, st, false);
    if (!check_op(st, ")"))
      FATAL(st, "Missing closing ')' of for condition");
    statement->forS->body = Program_parse_scope_block(p, m, st);
    if (!statement->forS->body)
      FATAL(st, "Missing for block");
  } else if (check_word(st, "while")) {
    statement = Program_new_Statement(p, While, next);
    if ((temp_e = Program_parse_expression(p, m, st, false)))
      statement->whileS->condition = temp_e;
    else
      FATAL(st, "Missing while conditon");
    statement->whileS->body = Program_parse_scope_block(p, m, st);
    if (!statement->whileS->body)
      FATAL(st, "Missing while block");
  } else if (check_word(st, "do")) {
    statement = Program_new_Statement(p, DoWhile, next);
    statement->doWhileS->body = Program_parse_scope_block(p, m, st);
    if (!check_word(st, "while"))
      FATAL(st, "Missing 'while' for do block");
    if ((temp_e = Program_parse_expression(p, m, st, false)))
      statement->doWhileS->condition = temp_e;
    else
      FATAL(st, "Missing do while conditon");
  } else if (check_word(st, "switch")) {
    statement = Program_new_Statement(p, Switch, next);
    if ((temp_e = Program_parse_expression(p, m, st, false)))
      statement->switchS->condition = temp_e;
    else
      FATAL(st, "Missing switch expression");
    statement->switchS->body = Program_parse_scope_block(p, m, st);
  } else if ((temp_e = Program_parse_expression(p, m, st, false))) {
    statement = Program_new_Statement(p, ExpressionS, next);
    statement->express->e = temp_e;
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
    Statement *s = Program_new_Statement(p, Scope, NULL);
    s->scope->body = Program_parse_scope(p, m, st);
    return s;
  } else
    return Program_parse_statement(p, m, st, NULL);
}

void Program_parse_fn(Program *p, Module *m, State *st) {
  skip_whitespace(st);

  Function *fn = Program_add_fn(p, m);
  if ((fn->name = read_identifier(p, st))) {
    if (check_op(st, "(")) {
      fn->parameter = Program_parse_variable_declaration_list(p, m, st, ")");
      fn->returnType = Program_parse_declared_type(p, m, st);
      if (check_op(st, "{"))
        fn->body = Program_parse_scope(p, m, st);
      else
        FATAL(st, "Missing function body");
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
        Program_parse_fn(p, m, st);
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

  State st = State_new(code, tempPath);
  Program_parse_module(p, m, &st);
  m->finished = true;

  free(code);
  return m;
}

void c_Module_types(FILE *f, Module *m);

typedef void (*ModuleFileCB)(FILE *, Module *);

void c_use(FILE *f, Use *u, ModuleFileCB cb) {
  if (u->next)
    c_use(f, u->next, cb);

  cb(f, u->use);
}

void c_type_declare(FILE *f, TypeDeclare *ty) {
  if (!ty)
    return;

  c_type_declare(f, ty->next);
  switch (ty->type) {
  case ArrayT:
    fprintf(f, "[%d]", ty->count);
    break;
  case PointerT:
    fprintf(f, "*");
    break;
  case TypeT:
    fprintf(f, "%s", ty->t->name);
    break;
  }
}

void c_enum_entry_list(FILE *f, EnumEntry *ee) {
  if (!ee)
    return;

  c_enum_entry_list(f, ee->next);
  if (ee->next)
    fprintf(f, ",\n  ");

  fprintf(f, "%s", ee->name);
  if (ee->valueSet)
    fprintf(f, " = %d", ee->value);
}

void c_enum(FILE *f, const char *name, EnumEntry *entries) {
  fprintf(f, "typedef enum %s", name);
  if (!entries) {
    fprintf(f, " {} %s;\n\n", name);
    return;
  }

  fprintf(f, " {\n  ");
  c_enum_entry_list(f, entries);
  fprintf(f, "");
  fprintf(f, "\n} %s;\n\n", name);
}

void c_var_list(FILE *f, Variable *v, const char *br) {
  if (!v)
    return;

  c_var_list(f, v->next, br);
  if (v->next)
    fprintf(f, "%s", br);

  c_type_declare(f, v->type);
  fprintf(f, " %s", v->name);
}

void c_struct(FILE *f, const char *name, Variable *member) {
  fprintf(f, "typedef struct %s", name);
  if (!member) {
    fprintf(f, " {} %s;\n\n", name);
    return;
  }

  fprintf(f, " {\n  ");
  c_var_list(f, member, ";\n  ");
  fprintf(f, "");
  fprintf(f, ";\n} %s;\n\n", name);
}

int c_union_enum_entry(FILE *f, const char *name, UnionEntry *entry, int i) {
  if (!entry)
    return i;

  int all = c_union_enum_entry(f, name, entry->next, i + 1);

  if (entry->next)
    fprintf(f, ",\n  ");
  fprintf(f, "%s_u%d_t", name, all - i);
  return all;
}

int c_union_entry(FILE *f, UnionEntry *entry, int i) {
  if (!entry)
    return i;

  int all = c_union_entry(f, entry->next, i + 1);

  if (entry->next)
    fprintf(f, ";\n    ");
  c_type_declare(f, entry->type);
  fprintf(f, " _u%d", all - i);
  return all;
}

void c_union_forward(FILE *f, const char *name, UnionEntry *member) {
  if (member) {
    fprintf(f, "typedef enum %sType", name);
    if (!member) {
      fprintf(f, " {} %s;\n\n", name);
      return;
    }

    fprintf(f, " {\n  ");
    c_union_enum_entry(f, name, member, 1);
    fprintf(f, "");
    fprintf(f, "\n} %sType;\n", name);
  }

  fprintf(f, "typedef struct %s %s;\n", name, name);
}

void c_union(FILE *f, const char *name, UnionEntry *member) {
  fprintf(f, "typedef struct %s", name);
  if (!member) {
    fprintf(f, " {} %s;\n\n", name);
    return;
  }

  fprintf(f, " {\n  union {\n    ");
  c_union_entry(f, member, 1);
  fprintf(f, "\n  };\n");
  fprintf(f, "  %sType type;", name);
  fprintf(f, "\n} %s;\n\n", name);
}

void c_type(FILE *f, Type *t) {
  if (!t)
    return;

  c_type(f, t->next);

  switch (t->kind) {
  case Klass:
    c_struct(f, t->name, t->member);
    break;
  case Enum:
    // c_enum(f, t->name, t->entries);
    break;
  case Union:
    c_union(f, t->name, t->union_member);
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
  case BoolA:
    fprintf(f, "%s", e->b ? "true" : "false");
    break;
  case CharA:
    fprintf(f, "'%c'", e->c);
    break;
  case IntA:
    fprintf(f, "%d", e->i);
    break;
  case FloatA:
    fprintf(f, "%g", e->f);
    break;
  case StringA:
    fprintf(f, "\"%s\"", e->s);
    break;
  case IdentifierA:
    fprintf(f, "%s", e->id);
    break;
  case VarE:
    fprintf(f, "(:: %s ", e->var->name);
    for (TypeDeclare *t = e->var->type; t; t = t->next) {
      switch (t->type) {
      case ArrayT:
        fprintf(f, "[%d]", t->count);
        break;
      case PointerT:
        fprintf(f, "*");
        break;
      case TypeT:
        fprintf(f, "%s", t->t->name);
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
    fprintf(f, "(## %s ", e->construct->type->name);
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
    fprintf(f, " %s)", e->member->member);
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
    fprintf(f, "(%s ", e->binop->op);
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
  c_expression(f, param->p);
}

void c_expression(FILE *f, Expression *e) {
  if (!e)
    return;

  switch (e->type) {
  case BoolA:
    fprintf(f, "%s", e->b ? "true" : "false");
    break;
  case CharA:
    fprintf(f, "'%c'", e->c);
    break;
  case IntA:
    fprintf(f, "%d", e->i);
    break;
  case FloatA:
    fprintf(f, "%g", e->f);
    break;
  case StringA:
    fprintf(f, "\"%s\"", e->s);
    break;
  case IdentifierA:
    fprintf(f, "%s", e->id);
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
    fprintf(f, "(");
    c_parameter(f, e->call->p);
    fprintf(f, ")");
    break;
  case ConstructE:
    fprintf(f, "(%s){", e->construct->type->name);
    c_parameter(f, e->construct->p);
    fprintf(f, "}");
    break;
  case AccessE:
    c_expression(f, e->access->o);
    fprintf(f, "[");
    c_expression(f, e->access->p);
    fprintf(f, "]");
    break;
  case MemberAccessE:
    c_expression(f, e->member->o);
    fprintf(f, "%s%s", (e->member->pointer ? "->" : "."), e->member->member);
    break;
  case AsCast:
    fprintf(f, "((");
    c_type_declare(f, e->cast->type);
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
    fprintf(f, " %s ", e->binop->op);
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

void c_fn(FILE *f, Function *fn) {
  if (!fn)
    return;

  c_fn(f, fn->next);

  if (fn->returnType)
    c_type_declare(f, fn->returnType);
  else
    fprintf(f, "void");
  fprintf(f, " %s(", fn->name);
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

  if (m->use) {
    fprintf(f, "\n");
    c_use(f, m->use, c_Module_types);
  }
  if (m->types) {
    fprintf(f, "\n");
    c_type(f, m->types);
  }
}

void c_Module_fn(FILE *f, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  if (m->use) {
    fprintf(f, "\n");
    c_use(f, m->use, c_Module_fn);
  }
  if (m->fn) {
    fprintf(f, "\n");
    c_fn(f, m->fn);
  }
}

void c_type_forward(FILE *f, Type *t) {
  if (!t)
    return;

  c_type_forward(f, t->next);

  switch (t->kind) {
  case Klass:
    fprintf(f, "typedef %s struct %s;\n", t->name, t->name);
    break;
  case Enum:
    c_enum(f, t->name, t->entries);
    break;
  case Union:
    c_union_forward(f, t->name, t->union_member);
    break;
  }
}

void c_Module_forward_types(FILE *f, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  if (m->use) {
    fprintf(f, "\n");
    c_use(f, m->use, c_Module_forward_types);
  }
  if (m->types) {
    fprintf(f, "\n");
    c_type_forward(f, m->types);
  }
}

void c_fn_forward(FILE *f, Function *fn) {
  if (!fn)
    return;

  c_fn_forward(f, fn->next);

  if (fn->returnType)
    c_type_declare(f, fn->returnType);
  else
    fprintf(f, "void");
  fprintf(f, " %s(", fn->name);
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

  if (m->use) {
    fprintf(f, "\n");
    c_use(f, m->use, c_Module_forward_fn);
  }
  if (m->fn) {
    fprintf(f, "\n");
    c_fn_forward(f, m->fn);
  }
}

void c_Program(FILE *f, Program *p, Module *m) {
  Program_reset_module_finished(p);
  c_Module_forward_types(f, m);

  Program_reset_module_finished(p);
  c_Module_forward_fn(f, m);

  Program_reset_module_finished(p);
  c_Module_types(f, m);

  Program_reset_module_finished(p);
  c_Module_fn(f, m);
}

int main(int argc, char *argv[]) {
  traceStack.size = 0;

  char buffer[1024 * 64];
  Program p = Program_new(buffer, 1024 * 64);

  Module *m = Program_parse_file(&p, "main", &(State){.file = "main.jnq", .line = 0, .column = 0});

  printf("---------\n");
  c_Program(stdout, &p, m);

  return 0;
}
