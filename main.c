
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

typedef struct Brace Brace;
typedef struct Call Call;
typedef struct Access Access;
typedef struct MemberAccess MemberAccess;
typedef struct UnaryOperation UnaryOperation;
typedef struct BinaryOperation BinaryOperation;

typedef struct DeclarationStatement DeclarationStatement;
typedef struct ExpressionStatement ExpressionStatement;

typedef struct ReturnStatement ReturnStatement;
typedef struct BreakStatement BreakStatement;
typedef struct ContinueStatement ContinueStatement;

typedef struct ScopeStatement ScopeStatement;

typedef struct IfStatement IfStatement;
typedef struct ForStatement ForStatement;
typedef struct WhileStatement WhileStatement;
typedef struct DoWhileStatement DoWhileStatement;

typedef struct Klass Klass;
typedef struct Module Module;

typedef enum ExpressionType {
  BoolA,
  CharA,
  IntA,
  FloatA,
  StringA,
  IdentifierA,
  BraceE,
  CallE,
  AccessE,
  MemberAccessE,
  UnaryOperationE,
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
    Brace *brace;
    Call *call;
    Access *access;
    MemberAccess *member;
    UnaryOperation *unop;
    BinaryOperation *binop;
  };
  ExpressionType type;
} Expression;

typedef enum StatementType {
  Declaration,
  ExpressionS,
  Return,
  Break,
  Continue,
  Scope,
  If,
  For,
  While,
  DoWhile,
} StatementType;

typedef struct Statement Statement;
typedef struct Statement {
  union {
    DeclarationStatement *declare;
    ExpressionStatement *express;
    ReturnStatement *ret;
    BreakStatement *brk;
    ContinueStatement *cont;
    ScopeStatement *scope;
    IfStatement *ifS;
    ForStatement *forS;
    WhileStatement *whileS;
    DoWhileStatement *doWhileS;
  };
  StatementType type;
  Statement *next;
} Statement;

typedef struct Variable Variable;
typedef struct Variable {
  const char *name;
  Klass *type;
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

typedef struct Access {
  Expression *o;
  Expression *p;
} Access;

typedef struct MemberAccess {
  Expression *o;
  const char *member;
} MemberAccess;

typedef struct UnaryOperation {
  Expression *o;
  const char *op;
} UnaryOperation;

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
  Expression e;
  Variable *v;
} DeclarationStatement;

typedef struct ReturnStatement {
  Expression *e;
} ReturnStatement;

typedef struct BreakStatement {
} BreakStatement;

typedef struct ContinueStatement {
} ContinueStatement;

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

typedef struct Function Function;
typedef struct Function {
  const char *name;
  Variable *parameter;
  Klass *returnType;
  Statement *body;
  Function *next;
} Function;

typedef struct Use Use;
typedef struct Use {
  Module *use;
  Use *next;
} Use;

typedef struct Klass {
  const char *name;
  Variable *member;
  Klass *next;
} Klass;

typedef struct Module {
  const char *path;
  Use *use;
  Klass *types;
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

Klass Bool = (Klass){"bool", NULL, NULL};
Klass Char = (Klass){"char", NULL, NULL};
Klass Int = (Klass){"int", NULL, NULL};
Klass Float = (Klass){"float", NULL, NULL};
Klass String = (Klass){"string", NULL, NULL};

Klass *Module_find_type(Program *p, Module *m, const char *b, const char *e) {
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

  for (Klass *c = m->types; c; c = c->next)
    if (strlen(c->name) == e - b && strncmp(c->name, b, e - b) == 0)
      return c;
  for (Use *u = m->use; u; u = u->next)
    for (Klass *c = u->use->types; c; c = c->next)
      if (strlen(c->name) == e - b && strncmp(c->name, b, e - b) == 0)
        return c;
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

Klass *Program_add_type(Program *p, Module *m) {
  Klass *c = (Klass *)Program_alloc(p, sizeof(Klass));
  c->next = m->types;
  m->types = c;
  return c;
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
  case Declaration:
    s->declare = Program_alloc(p, sizeof(DeclarationStatement));
    break;
  case ExpressionS:
    s->express = Program_alloc(p, sizeof(ExpressionStatement));
    break;
  case Return:
    s->ret = Program_alloc(p, sizeof(ReturnStatement));
    break;
  case Break:
    s->brk = Program_alloc(p, sizeof(BreakStatement));
    break;
  case Continue:
    s->cont = Program_alloc(p, sizeof(ContinueStatement));
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
  case BraceE:
    e->brace = Program_alloc(p, sizeof(Brace));
    break;
  case CallE:
    e->call = Program_alloc(p, sizeof(Call));
    break;
  case AccessE:
    e->access = Program_alloc(p, sizeof(Access));
    break;
  case MemberAccessE:
    e->access = Program_alloc(p, sizeof(MemberAccess));
    break;
  case UnaryOperationE:
    e->unop = Program_alloc(p, sizeof(UnaryOperation));
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

Klass *Program_parse_declared_type(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State old = *st;
  if (check_identifier(st)) {
    Klass *t = Module_find_type(p, m, old.c, st->c);
    if (!t)
      FATAL(&old, "Unknown type '%.*s'", (int)(st->c - old.c), old.c);
    return t;
  }

  *st = old;
  return NULL;
}

Variable *Program_parse_variable_declaration(Program *p, Module *m, State *st, const char *end) {
  skip_whitespace(st);
  State old = *st;
  Variable *top = NULL;
  while (*st->c) {
    if (check_op(st, end))
      return top;
    top = Program_new_variable(p, top);
    if ((top->name = read_identifier(p, st))) {
      if ((top->type = Program_parse_declared_type(p, m, st))) {
        ;
      } else
        FATAL(st, "missing type");
    } else
      FATAL(st, "missing variable name");
    check_op(st, ",");
  }
  FATAL(st, "Missing closing '%s'", end);
  return NULL;
}

void Program_parse_type(Program *p, Module *m, State *st) {
  skip_whitespace(st);

  Klass *c = Program_add_type(p, m);
  if ((c->name = read_identifier(p, st))) {
    if (check_word(st, "struct") && check_op(st, "{"))
      c->member = Program_parse_variable_declaration(p, m, st, "}");
    else
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

  *st = old;
  return NULL;
}

Expression *Program_parse_expression(Program *p, State *st, bool ignoreSuffix);

Parameter *Program_parse_parameter_list(Program *p, State *st) {
  Parameter *param = NULL;
  Expression *e = NULL;

  while ((e = Program_parse_expression(p, st, false))) {
    Parameter *pp = Program_alloc(p, sizeof(Parameter));
    pp->next = param;
    pp->p = e;
    param = pp;
    if (!check_op(st, ","))
      break;
  }
  return param;
}

Expression *Program_parse_expression_suffix(Program *p, State *st, Expression *e) {

  if (check_op(st, ".")) {
    const char *member = read_identifier(p, st);
    if (!member)
      FATAL(st, "missing id for member access");
    Expression *ma = Program_new_Expression(p, MemberAccessE);
    ma->member->o = e;
    ma->member->member = member;
    ma = Program_parse_expression_suffix(p, st, ma);
    return ma;
  }

  if (check_op(st, "[")) {
    Expression *acc = Program_new_Expression(p, AccessE);
    acc->access->o = e;
    acc->access->p = Program_parse_expression(p, st, false);
    if (!acc->access->o)
      FATAL(st, "missing '[]' content");
    if (!check_op(st, "]"))
      FATAL(st, "missing closing ']'");
    acc = Program_parse_expression_suffix(p, st, acc);
    return acc;
  }

  if (check_op(st, "(")) {
    Expression *call = Program_new_Expression(p, CallE);
    call->call->o = e;
    call->call->p = Program_parse_parameter_list(p, st);
    if (!check_op(st, ")"))
      FATAL(st, "unfinished function call, missing ')'");
    call = Program_parse_expression_suffix(p, st, call);
    return call;
  }
  return e;
}

Expression *Program_parse_binary_operation(Program *p, State *st, Expression *e, Expression *prefix) {

  e = Program_parse_expression_suffix(p, st, e);
  if (prefix) {
    prefix->unop->o = e;
    e = prefix;
  }
  const char *bin_ops[] = {">>=", "<<=", "==", "!=", ">=", "<=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=",
                           "&&",  "||",  "->", ">>", "<<", "+",  "-",  "*",  "/",  "%",  "&",  "|",  "."};
  for (int i = 0; i < sizeof(bin_ops) / sizeof(const char *); ++i) {
    if (check_op(st, bin_ops[i])) {
      Expression *bin = Program_new_Expression(p, BinaryOperationE);
      bin->binop->o1 = e;
      bin->binop->op = bin_ops[i];
      bin->binop->o2 = Program_parse_expression(p, st, false);
      return bin;
    }
  }
  return e;
}

Expression *Program_parse_expression(Program *p, State *st, bool ignoreSuffix) {

  Expression *prefix = NULL;
  const char *un_pre_ops[] = {"*", "~", "!", "-", "+", "&"};
  for (int i = 0; i < sizeof(un_pre_ops) / sizeof(const char *); ++i) {
    if (check_op(st, un_pre_ops[i])) {
      prefix = Program_new_Expression(p, UnaryOperationE);
      prefix->unop->op = un_pre_ops[i];
      break;
      FATAL(st, "missing implementation for unary operation '-'", un_pre_ops[i]);
    }
  }

  Expression *e = NULL;
  if (check_op(st, "(")) {
    e = Program_new_Expression(p, BraceE);
    e->brace->o = Program_parse_expression(p, st, false);
    if (!e->brace->o)
      FATAL(st, "missing '(' content");
    if (!check_op(st, ")"))
      FATAL(st, "missing closing ')'");
  } else
    e = Program_parse_atom(p, st);

  if (!e && prefix)
    FATAL(st, "prefix operation without expression '%s'", prefix->unop->op);
  if (!e)
    return NULL;

  if (!ignoreSuffix)
    e = Program_parse_binary_operation(p, st, e, prefix);

  return e;
}

Statement *Program_parse_scope_block(Program *p, State *st);
Statement *Program_parse_statement(Program *p, State *st, Statement *next) {
  Statement *statement = NULL;
  Expression *temp_e = NULL;
  if (check_word(st, "return")) {
    statement = Program_new_Statement(p, Return, next);
    statement->express->e = Program_parse_expression(p, st, false);
  } else if (check_word(st, "break"))
    statement = Program_new_Statement(p, Break, next);
  else if (check_word(st, "continue"))
    statement = Program_new_Statement(p, Continue, next);
  else if (check_word(st, "if")) {
    statement = Program_new_Statement(p, If, next);
    if ((temp_e = Program_parse_expression(p, st, false)))
      statement->ifS->condition = temp_e;
    else
      FATAL(st, "Missing if conditon");
    statement->ifS->ifBody = Program_parse_scope_block(p, st);
    if (!statement->ifS->ifBody)
      FATAL(st, "Missing if block");
    if (check_word(st, "else")) {
      statement->ifS->elseBody = Program_parse_scope_block(p, st);
      if (!statement->ifS->elseBody)
        FATAL(st, "Missing else block");
    }
  } else if (check_word(st, "for")) {
    statement = Program_new_Statement(p, For, next);
    if (!check_op(st, "("))
      FATAL(st, "Missing for conditon");
    statement->forS->init = Program_parse_expression(p, st, false);
    if (!check_op(st, ";"))
      FATAL(st, "Missing for init");
    statement->forS->condition = Program_parse_expression(p, st, false);
    if (!check_op(st, ";"))
      FATAL(st, "Missing for condition");
    statement->forS->incr = Program_parse_expression(p, st, false);
    if (!check_op(st, ")"))
      FATAL(st, "Missing closing ')' of for condition");
    statement->forS->body = Program_parse_scope_block(p, st);
    if (!statement->forS->body)
      FATAL(st, "Missing for block");
  } else if (check_word(st, "while")) {
    statement = Program_new_Statement(p, While, next);
    if ((temp_e = Program_parse_expression(p, st, false)))
      statement->whileS->condition = temp_e;
    else
      FATAL(st, "Missing while conditon");
    statement->whileS->body = Program_parse_scope_block(p, st);
    if (!statement->whileS->body)
      FATAL(st, "Missing while block");
  } else if (check_word(st, "do")) {
    statement = Program_new_Statement(p, DoWhile, next);
    statement->doWhileS->body = Program_parse_scope_block(p, st);
    if (!check_word(st, "while"))
      FATAL(st, "Missing 'while' for do block");
    if ((temp_e = Program_parse_expression(p, st, false)))
      statement->doWhileS->condition = temp_e;
    else
      FATAL(st, "Missing do while conditon");
  } else if ((temp_e = Program_parse_expression(p, st, false))) {
    statement = Program_new_Statement(p, ExpressionS, next);
    statement->express->e = temp_e;
  }
  return statement;
}
Statement *Program_parse_scope(Program *p, State *st) {
  Statement *body = NULL;
  Expression *temp_e = NULL;
  while (*st->c) {
    skip_whitespace(st);
    if (check_op(st, "}"))
      return body;
    body = Program_parse_statement(p, st, body);
    if (!body)
      break;
  }
  FATAL(st, "Missing closing '}' for function body");
  return NULL;
}
Statement *Program_parse_scope_block(Program *p, State *st) {
  if (check_op(st, "{")) {
    Statement *s = Program_new_Statement(p, Scope, NULL);
    s->scope->body = Program_parse_scope(p, st);
    return s;
  } else
    return Program_parse_statement(p, st, NULL);
}

void Program_parse_fn(Program *p, Module *m, State *st) {
  skip_whitespace(st);

  Function *fn = Program_add_fn(p, m);
  if ((fn->name = read_identifier(p, st))) {
    if (check_op(st, "(")) {
      fn->parameter = Program_parse_variable_declaration(p, m, st, ")");
      fn->returnType = Program_parse_declared_type(p, m, st);
      if (check_op(st, "{"))
        fn->body = Program_parse_scope(p, st);
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

void c_use(FILE *f, Use *u) {
  if (u->next)
    c_use(f, u->next);

  fprintf(f, "use %s\n", u->use->path);
}

void c_type_declare(FILE *f, Klass *ty) {
  if (ty)
    printf("%s", ty->name);
  else
    printf("void");
}

void c_var_list(FILE *f, Variable *v) {
  if (!v)
    return;

  c_var_list(f, v->next);
  if (v->next)
    fprintf(f, ", ");

  c_type_declare(f, v->type);
  fprintf(f, " %s", v->name);
}

void c_type(FILE *f, Klass *t) {
  if (!t)
    return;

  c_type(f, t->next);
  fprintf(f, "type %s\n", t->name);
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
    fprintf(f, "%c", e->c);
  case IntA:
    fprintf(f, "\"%d\"", e->i);
  case FloatA:
    fprintf(f, "%g", e->f);
  case StringA:
    fprintf(f, "\"%s\"", e->s);
  case IdentifierA:
    fprintf(f, "%s", e->id);
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
  case AccessE:
    fprintf(f, "([] ");
    lisp_expression(f, e->access->o);
    fprintf(f, " ");
    lisp_expression(f, e->access->p);
    fprintf(f, ")");
    break;
  case MemberAccessE:
    fprintf(f, "(. ");
    lisp_expression(f, e->member->o);
    fprintf(f, " %s)", e->member->member);
    break;
  case UnaryOperationE:
    fprintf(f, "(%s ", e->unop->op);
    lisp_expression(f, e->unop->o);
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
    fprintf(f, "%c", e->c);
  case IntA:
    fprintf(f, "\"%d\"", e->i);
  case FloatA:
    fprintf(f, "%g", e->f);
  case StringA:
    fprintf(f, "\"%s\"", e->s);
  case IdentifierA:
    fprintf(f, "%s", e->id);
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
  case AccessE:
    c_expression(f, e->access->o);
    fprintf(f, "[");
    c_expression(f, e->access->p);
    fprintf(f, "]");
    break;
  case MemberAccessE:
    c_expression(f, e->member->o);
    fprintf(f, ".%s", e->member->member);
    break;
  case UnaryOperationE:
    fprintf(f, "%s", e->unop->op);
    c_expression(f, e->unop->o);
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
  case Declaration:
    fprintf(f, "<<decl>>;\n");
    break;
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
  }
}

void c_fn(FILE *f, Function *fn) {
  if (!fn)
    return;

  c_fn(f, fn->next);

  c_type_declare(f, fn->returnType);
  printf(" %s(", fn->name);
  c_var_list(f, fn->parameter);
  if (!fn->body)
    printf(") {}\n\n");
  else {
    printf(") {\n");
    c_statements(f, fn->body, 2);
    printf("}\n\n");
  }
}

void c_Module(FILE *f, const Module *m) {
  if (m->use) {
    printf("\n");
    c_use(f, m->use);
  }
  if (m->types) {
    printf("\n");
    c_type(f, m->types);
  }
  if (m->fn) {
    printf("\n");
    c_fn(f, m->fn);
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

int main(int argc, char *argv[]) {
  traceStack.size = 0;

  char buffer[1024 * 64];
  Program p = Program_new(buffer, 1024 * 64);

  Module *m = Program_parse_file(&p, "main", &(State){.file = "main.jnq", .line = 0, .column = 0});

  printf("---------\n");
  c_Module(stdout, m);

  return 0;
}
