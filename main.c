
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct State {
  const char *c;
  size_t line;
  size_t column;
} State;

State State_new(const char *c) {
  return (State){.c = c, .line = 1, .column = 1};
}

void FATAL(const char *format, ...) {
  va_list args;
  va_start(args, format);
  fputs("Error: ", stderr);
  vfprintf(stderr, format, args);
  va_end(args);
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
  Klass *use;
  Use *next;
} Use;

typedef struct Klass {
  const char *path;
  Use *use;
  Variable *member;
  Function *fn;
  bool finished;
  Klass *next;
} Klass;

typedef struct Program {
  struct {
    char *buffer;
    size_t len;
    size_t cap;
  } arena;

  Klass *classes;
} Program;

Program Program_new(char *buffer, size_t cap) {
  Program p;
  p.arena.buffer = buffer;
  p.arena.len = 0;
  p.arena.cap = cap;
  p.classes = NULL;
  return p;
}

void *Program_alloc(Program *p, size_t size) {
  if (p->arena.len + size > p->arena.cap)
    FATAL("Out of memory");
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

Klass *Program_find_class(Program *p, const char *path) {
  for (Klass *c = p->classes; c; c = c->next) {
    if (strcmp(c->path, path) == 0)
      return c;
  }
  return NULL;
}

Klass *Program_add_class(Program *p, const char *pathc) {
  size_t size = strlen(pathc) + 1;
  char *path = (char *)Program_alloc(p, size);
  strcpy(path, pathc);
  Klass *c = (Klass *)Program_alloc(p, sizeof(Klass));
  c->path = path;
  c->use = NULL;
  c->member = NULL;
  c->fn = NULL;
  c->next = p->classes;
  p->classes = c;
  return c;
}

Use *Program_add_use(Program *p, Klass *c, Klass *use) {
  Use *u = Program_alloc(p, sizeof(Use));
  u->use = use;
  u->next = c->use;
  c->use = u;
  return u;
}

Function *Program_add_fn(Program *p, Klass *c) {
  Function *fn = Program_alloc(p, sizeof(Function));
  fn->next = c->fn;
  c->fn = fn;
  return fn;
}

void skip_whitespace(State *st) {
  while (*st->c && isspace(*st->c)) {
    if (*st->c == '\n') {
      st->line = 1;
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

Klass *Program_parse_file(Program *p, const char *path);

bool Program_parse_use_path(Program *p, Klass *c, State *st) {
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
        FATAL("Error parsing use statement!");
      inb += snprintf(buffer + inb, 256 - inb, "%.*s", (int)(st->c - old.c),
                      old.c);
      old = *st;
    }
  } else
    return false;

  Klass *use = Program_parse_file(p, buffer);
  Program_add_use(p, c, use);
  return true;
}

void Program_parse_variable_declaration(Program *p, State *st,
                                        const char *end) {
  skip_whitespace(st);
  State old = *st;
  while (*st->c) {
    if (check_op(st, end))
      return;
    if (check_identifier(st)) {
      skip_whitespace(st);
      if (check_identifier(st)) {
        ;
      } else
        FATAL("missing member name");
    } else
      FATAL("missing type name");
    check_op(st, ",");
  }
  FATAL("Missing closing '%s'", end);
}

void Program_parse_struct_body(Program *p, Klass *c, State *st) {
  skip_whitespace(st);

  if (check_op(st, "{")) {
    Program_parse_variable_declaration(p, st, "}");
  } else
    FATAL("Missing struct body");
}

void Program_parse_type(Program *p, Klass *c, State *st) {
  skip_whitespace(st);

  if (check_identifier(st)) {
    if (check_word(st, "struct")) {
      Program_parse_struct_body(p, c, st);
    } else
      FATAL("Missing type declaration");
  } else
    FATAL("Missing type name");
}

void Program_parse_function_body(Program *p, Klass *c, Function *fn,
                                 State *st) {
  skip_whitespace(st);
  while (*st->c) {
    if (check_op(st, "}"))
      return;
    break;
  }
  FATAL("Missing closing '}' for function body");
}

void Program_parse_fn(Program *p, Klass *c, State *st) {
  skip_whitespace(st);

  Function *fn = Program_add_fn(p, c);
  if ((fn->name = read_identifier(p, st))) {

    if (check_op(st, "(")) {
      Program_parse_variable_declaration(p, st, ")");
      skip_whitespace(st);
      if (check_identifier(st)) {
        ;
      } else
        ; // void
      if (check_op(st, "{"))
        Program_parse_function_body(p, c, NULL, st);
      else
        FATAL("Missing function body");
    } else
      FATAL("Missing parameterlist");
  } else
    FATAL("Missing type name");
}

void Program_parse_class(Program *p, Klass *c, State *st) {
  while (st->c[0]) {
    skip_whitespace(st);
    if (!st->c[0])
      break;
    if (check_word(st, "use"))
      Program_parse_use_path(p, c, st);
    else if (check_word(st, "type"))
      Program_parse_type(p, c, st);
    else if (check_word(st, "fn"))
      Program_parse_fn(p, c, st);
    else {
      FATAL("Unknown keyword >>'%s'\n", st->c);
      break;
    }
  }
}

Klass *Program_parse_file(Program *p, const char *path) {
  Klass *c = Program_find_class(p, path);
  if (c && !c->finished)
    FATAL("Circular dependencies!");
  if (c)
    return c;

  c = Program_add_class(p, path);
  c->finished = false;

  char tempPath[256];
  strcpy(tempPath, path);
  for (char *t = tempPath; *t; ++t)
    if (*t == '.')
      *t = '/';
  strcat(tempPath, ".jnq");

  char *code = readFile(tempPath);
  if (!code)
    FATAL("missing file! '%s'", tempPath);

  State st = State_new(code);
  Program_parse_class(p, c, &st);
  c->finished = true;

  free(code);
  return c;
}

int main(int argc, char *argv[]) {
  char buffer[1024 * 64];
  Program p = Program_new(buffer, 1024 * 64);

  Program_parse_file(&p, "main");

  return 0;
}
