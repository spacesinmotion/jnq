
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

State State_new(const char *c, const char *file) {
  return (State){.file = file, .c = c, .line = 1, .column = 1};
}

struct {
  State states[128];
  int size;
} traceStack;

void FATAL(State *s, const char *format, ...);
void traceStack_push(State *s) {
  if (traceStack.size == 128)
    FATAL(&(State){.file = s->file, .line = 0, .column = 0},
          "not enough buffer for stack trace");
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

typedef struct Call Call;
typedef struct Access Access;
typedef struct UnaryOperation UnaryOperation;
typedef struct BinaryOperation BinaryOperation;

typedef struct DeclarationStatement DeclarationStatement;
typedef struct ExpressionStatement ExpressionStatement;

typedef struct ReturnStatement ReturnStatement;
typedef struct BreakStatement BreakStatement;
typedef struct ContinueStatement ContinueStatement;

typedef struct IfStatement IfStatement;
typedef struct ForStatement ForStatement;
typedef struct WhileStatement WhileStatement;
typedef struct DoWhileStatement DoWhileStatement;

typedef struct Klass Klass;
typedef struct Module Module;

typedef struct Expression {
  union {
    Call *call;
    Access *access;
    UnaryOperation *unop;
    BinaryOperation *binop;
  };
} Expression;

typedef struct Statement {
  union {
    DeclarationStatement *declare;
    ExpressionStatement *express;
    ReturnStatement *ret;
    BreakStatement *brk;
    ContinueStatement *cont;
    IfStatement *ifS;
    ForStatement *forS;
    WhileStatement *whileS;
    DoWhileStatement *doWhileS;
  };

} Statement;

typedef struct Variable Variable;
typedef struct Variable {
  const char *name;
  Klass *type;
  Variable *next;
} Variable;

typedef struct Parameter Parameter;
typedef struct Parameter {
  Expression p;
  Parameter *next;
} Parameter;

typedef struct Call {
  Expression o;
  Parameter p;
} Call;

typedef struct Access {
  Expression o;
  Expression p;
} Access;

typedef struct UnaryOperation {
  Expression o;
} UnaryOperation;

typedef struct BinaryOperation {
  Expression o1;
  Expression o2;
} BinaryOperation;

typedef struct ExpressionStatement {
  Expression e;
  Statement next;
} ExpressionStatement;

typedef struct DeclarationStatement {
  Expression e;
  Variable *v;
  Statement next;
} DeclarationStatement;

typedef struct ReturnStatement {
  Expression e;
  Statement next;
} ReturnStatement;

typedef struct BreakStatement {
  Statement next;
} BreakStatement;

typedef struct ContinueStatement {
  Statement next;
} ContinueStatement;

typedef struct IfStatement {
  Expression condition;
  Statement ifBody;
  Statement elseBody;
} IfStatement;

typedef struct ForStatement {
  Expression init;
  Expression condition;
  Expression incr;
  Statement body;
} ForStatement;

typedef struct WhileStatement {
  Expression condition;
  Statement body;
} WhileStatement;

typedef struct DoWhileStatement {
  Statement body;
  Expression condition;
} DoWhileStatement;

typedef struct Function Function;
typedef struct Function {
  const char *name;
  Variable *parameter;
  Statement body;
  Klass *returnType;
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
  fn->next = m->fn;
  m->fn = fn;
  return fn;
}

Variable *Program_new_variable(Program *p, Variable *next) {
  Variable *v = Program_alloc(p, sizeof(Variable));
  v->next = next;
  return v;
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
    inb +=
        snprintf(buffer + inb, 256 - inb, "%.*s", (int)(st->c - old.c), old.c);
    old = *st;
    for (;;) {
      if (!check_op(st, "."))
        break;
      inb += snprintf(buffer + inb, 256 - inb, ".");
      skip_whitespace(st);
      old = *st;

      if (!check_identifier(st))
        FATAL(st, "Error parsing use statement!");
      inb += snprintf(buffer + inb, 256 - inb, "%.*s", (int)(st->c - old.c),
                      old.c);
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

Variable *Program_parse_variable_declaration(Program *p, Module *m, State *st,
                                             const char *end) {
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

void Program_parse_function_body(Program *p, Function *fn, State *st) {
  skip_whitespace(st);
  while (*st->c) {
    if (check_op(st, "}"))
      return;
    break;
  }
  FATAL(st, "Missing closing '}' for function body");
}

void Program_parse_fn(Program *p, Module *m, State *st) {
  skip_whitespace(st);

  Function *fn = Program_add_fn(p, m);
  if ((fn->name = read_identifier(p, st))) {
    if (check_op(st, "(")) {
      fn->parameter = Program_parse_variable_declaration(p, m, st, ")");
      fn->returnType = Program_parse_declared_type(p, m, st);
      if (check_op(st, "{"))
        Program_parse_function_body(p, fn, st);
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

int main(int argc, char *argv[]) {
  traceStack.size = 0;

  char buffer[1024 * 64];
  Program p = Program_new(buffer, 1024 * 64);

  Program_parse_file(&p, "main",
                     &(State){.file = "main.jnq", .line = 0, .column = 0});

  return 0;
}
