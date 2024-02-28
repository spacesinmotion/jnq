
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <setjmp.h>

#ifndef _WIN32
#include <unistd.h>
#define JNQ_BIN "./jnq_bin"
#else
#include <io.h>
#include <windows.h>

#define access _access
#define F_OK 0
#define JNQ_BIN "jnq_bin.exe"
#endif

#ifdef __GNUC__
#define PACK(__Declaration__) __Declaration__ __attribute__((__packed__))
#elif _MSC_VER
#define PACK(__Declaration__) __pragma(pack(push, 1)) __Declaration__ __pragma(pack(pop))
#endif

char lib_path[256] = {0};

typedef PACK(struct Location {
  const char *file;
  uint16_t line;
  uint16_t column;
}) Location;

typedef struct LocationRange {
  const char *file;
  uint16_t start_line;
  uint16_t start_column;
  uint16_t end_line;
  uint16_t end_column;
} LocationRange;

LocationRange NewRange(Location s, Location e) {
  return (LocationRange){
      .file = s.file,
      .start_line = s.line,
      .start_column = s.column,
      .end_line = e.line,
      .end_column = e.column,
  };
}

LocationRange NewRangeLength(Location s, int l) {
  return (LocationRange){
      .file = s.file,
      .start_line = s.line,
      .start_column = s.column,
      .end_line = s.line,
      .end_column = s.column + l,
  };
}

LocationRange NewRangeWord(Location s, const char *e) { return NewRangeLength(s, strlen(e)); }

Location RangeStart(LocationRange r) { return (Location){r.file, r.start_line, r.start_column}; }
bool RangeOk(LocationRange r) { return r.start_line > 0 && r.start_column > 0; }

bool inRange(LocationRange r, int line, int column) {
  if (line < r.start_line || line > r.end_line)
    return false;
  if (line == r.start_line && column < r.start_column)
    return false;
  if (line == r.end_line && column > r.end_column)
    return false;
  return true;
}

typedef struct State {
  const char *c;
  Location location;
} State;

State State_new(const char *c, const char *file) {
  return (State){.c = c, .location = (Location){.file = file, .line = 1, .column = 1}};
}

#define null_location ((Location){.file = "", .line = 1, .column = 1})

void State_skip(State *s, int c) {
  s->c += c;
  s->location.column += c;
}

jmp_buf long_jump_end;

void FATAL(Location *l, const char *format, ...) {
  va_list args;
  fprintf(stderr, "%s:%hu:%hu: error: ", l->file, l->line, l->column);
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);
  longjmp(long_jump_end, 1);
}
void FATALR(LocationRange l, const char *format, ...) {
  va_list args;
  fprintf(stderr, "%s:%hu:%hu: error: ", l.file, l.start_line, l.start_column);
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);
  longjmp(long_jump_end, 1);
}
void FATALX(const char *format, ...) {
  va_list args;
  fprintf(stderr, "%s:%zu:%zu error: ", "unknown.jnq", (size_t)0, (size_t)0);
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);
  longjmp(long_jump_end, 1);
}

Location back(State *s, size_t c) {
  Location b = s->location;
  if (b.column < c) {
    b.column = 0;
    FATAL(&b, "internal error calculating location");
  }
  b.column -= c;
  return b;
}

Location back_to_none_whitespace(State s, State old) {
  while (s.c > old.c) {
    s.c--;
    if (!isspace(*s.c))
      break;
  }
  s.c++;
  while (s.c > old.c) {
    if (*old.c == '\n') {
      old.location.line++;
      old.location.column = 1;
    } else
      ++old.location.column;
    old.c++;
  }
  return old.location;
}

typedef struct BufString {
  char s[256];
} BuffString;

BuffString str(const char *format, ...) {
  va_list args;
  BuffString s;
  va_start(args, format);
  vsnprintf(s.s, sizeof(s.s), format, args);
  va_end(args);
  return s;
}

typedef struct BaseConst BaseConst;
typedef struct Identifier Identifier;
typedef struct Variable Variable;
typedef struct AutoTypeDeclaration AutoTypeDeclaration;
typedef struct Brace Brace;
typedef struct Call Call;
typedef struct Construct Construct;
typedef struct New New;
typedef struct Access Access;
typedef struct Slice Slice;
typedef struct MemberAccess MemberAccess;
typedef struct Cast Cast;
typedef struct UnaryPrefix UnaryPrefix;
typedef struct UnaryPostfix UnaryPostfix;
typedef struct BinaryOperation BinaryOperation;
typedef struct TernaryOperation TernaryOperation;

typedef struct CDelegate CDelegate;

typedef struct ExpressionStatement ExpressionStatement;

typedef struct ReturnStatement ReturnStatement;
typedef struct BreakStatement BreakStatement;
typedef struct ContinueStatement ContinueStatement;
typedef struct CaseStatement CaseStatement;
typedef struct DefaultStatement DefaultStatement;
typedef struct DeleteStatement DeleteStatement;

typedef struct ScopeStatement ScopeStatement;

typedef struct IfStatement IfStatement;
typedef struct ForStatement ForStatement;
typedef struct WhileStatement WhileStatement;
typedef struct DoWhileStatement DoWhileStatement;
typedef struct SwitchStatement SwitchStatement;

typedef struct Type Type;
typedef struct Module Module;

typedef enum ExpressionType {
  BaseA,
  IdentifierA,
  AutoTypeE,
  BraceE,
  CallE,
  ConstructE,
  NewE,
  AccessE,
  SliceE,
  MemberAccessE,
  AsCast,
  UnaryPrefixE,
  UnaryPostfixE,
  BinaryOperationE,
  TernaryOperationE,
  CDelegateE,
} ExpressionType;

typedef PACK(struct Expression {
  union {
    BaseConst *baseconst;
    Identifier *id;
    Variable *var;
    AutoTypeDeclaration *autotype;
    Brace *brace;
    Call *call;
    Construct *construct;
    New *newE;
    Access *access;
    Slice *slice;
    MemberAccess *member;
    Cast *cast;
    UnaryPrefix *unpre;
    UnaryPostfix *unpost;
    BinaryOperation *binop;
    TernaryOperation *ternop;
    CDelegate *cdelegate;
  };
  LocationRange range;
  uint8_t type;
}) Expression;

typedef enum StatementType {
  ExpressionS,
  Return,
  Break,
  Continue,
  Case,
  Default,
  Delete,
  Scope,
  If,
  For,
  While,
  DoWhile,
  Switch,
} StatementType;

typedef struct Statement Statement;
typedef PACK(struct Statement {
  union {
    ExpressionStatement *express;
    ReturnStatement *ret;
    BreakStatement *brk;
    ContinueStatement *cont;
    CaseStatement *caseS;
    DefaultStatement *defaultS;
    DeleteStatement *deleteS;
    ScopeStatement *scope;
    IfStatement *ifS;
    ForStatement *forS;
    WhileStatement *whileS;
    DoWhileStatement *doWhileS;
    SwitchStatement *switchS;
  };
  Statement *next;
  uint8_t type;
}) Statement;

typedef struct Function Function;
typedef struct EnumEntry EnumEntry;

typedef struct Type Type;

typedef struct BaseConst {
  const char *text;
  Type *type;
} BaseConst;

typedef struct Identifier {
  const char *name;
  Type *type;
} Identifier;

typedef PACK(struct Variable {
  const char *name;
  Type *type;
  LocationRange location;
  bool is_const;
}) Variable;

typedef PACK(struct VariableList {
  Variable *v;
  uint16_t len;
}) VariableList;

typedef struct AutoTypeDeclaration {
  Expression *e;
  const char *name;
  Type *type;
} AutoTypeDeclaration;

typedef struct Parameter {
  Expression *p;
  const char *name;
} Parameter;

typedef PACK(struct ParameterList {
  Parameter *p;
  uint16_t len;
}) ParameterList;

typedef struct Brace {
  Expression *o;
} Brace;

typedef PACK(struct Call {
  Expression *o;
  ParameterList p;
}) Call;

typedef PACK(struct Construct {
  Type *type;
  ParameterList p;
  bool pointer;
}) Construct;

typedef struct New {
  Expression *o;
} New;

typedef struct Access {
  Expression *o;
  Expression *p;
} Access;

typedef struct Slice {
  Expression *o;
  Expression *begin;
  Expression *end;
} Slice;

typedef struct MemberAccess {
  Expression *o;
  Type *o_type;
  Identifier *member;
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
    {"<", 100 - 6, ASSOC_LEFT, true},      //
    {">", 100 - 6, ASSOC_LEFT, true},      //
    {"&", 100 - 8, ASSOC_LEFT, false},     //
    {"^", 100 - 9, ASSOC_LEFT, false},     //
    {"|", 100 - 10, ASSOC_LEFT, false},    //
                                           //
    {"=", 100 - 14, ASSOC_RIGHT, false},   //
};
BinOp *getop(const char *ch) {
  for (size_t i = 0; i < sizeof(ops) / sizeof(ops[0]); ++i)
    if (strcmp(ops[i].op, ch) == 0)
      return ops + i;
  return NULL;
}

typedef struct BinaryOperation {
  Expression *o1;
  Expression *o2;
  BinOp *op;
} BinaryOperation;

typedef struct TernaryOperation {
  Expression *condition;
  Expression *if_e;
  Expression *else_e;
} TernaryOperation;

typedef struct CDelegate {
  Expression *o;
  const char *delegate;
} CDelegate;

typedef struct ExpressionStatement {
  Expression *e;
} ExpressionStatement;

typedef struct DeclarationStatement {
  Expression *e;
  Variable *v;
} DeclarationStatement;

typedef struct ReturnStatement {
  Expression *e;
} ReturnStatement;

typedef struct DeleteStatement {
  Expression *e;
} DeleteStatement;

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
  VariableList member;
  Module *module;
  LocationRange location;
} Struct;

typedef PACK(struct FunctionDecl {
  VariableList parameter;
  Type *returnType;
  LocationRange return_type_location;
}) FunctionDecl;

typedef PACK(struct FnVec {
  Type *fns;
  uint16_t len;
}) FnVec;

typedef struct TypeList TypeList;

typedef PACK(struct Interface {
  FnVec methods;
  Module *module;
  LocationRange location;
  TypeList *used_types;
}) Interface;

typedef PACK(struct EnumEntry {
  const char *name;
  EnumEntry *next;
  Location location;
  int value;
  bool valueSet;
}) EnumEntry;

#define reverse_list(T, head)                                                                                          \
  {                                                                                                                    \
    T *current = head;                                                                                                 \
    T *prev = NULL, *next = NULL;                                                                                      \
    while (current != NULL) {                                                                                          \
      next = current->next;                                                                                            \
      current->next = prev;                                                                                            \
      prev = current;                                                                                                  \
      current = next;                                                                                                  \
    }                                                                                                                  \
    head = prev;                                                                                                       \
  }

typedef PACK(struct Enum {
  EnumEntry *entries;
  Module *module;
  LocationRange location;
}) Enum;

typedef PACK(struct Function {
  FunctionDecl d;
  Statement *body;
  Module *module;
  LocationRange location;
  bool is_extern_c;
}) Function;

typedef PACK(struct Use {
  Module *module;
  Type **type;
  uint16_t type_len;
  LocationRange location;
  bool take_all;
}) Use;

typedef enum TypeKind {
  UseT,
  BaseT,
  StructT,
  CStructT,
  UnionT,
  EnumT,
  CEnumT,
  InterfaceT,
  ArrayT,
  DynArrayT,
  PointerT,
  SliceT,
  FnT,
  MacroT,
  ConstantWrapperT,
  PlaceHolder
} TypeKind;

typedef PACK(struct Type {
  const char *name;
  union {
    const char *c_name;
    Struct *structT;
    Enum *enumT;
    Interface *interfaceT;
    Function *fnT;
    Module *placeholerModule;
    Module *constantModule;
    Use *useT;
    const char *macro_name;
    int array_count;
  };
  uint8_t kind;
  Type *child;
}) Type;

Module *global_module();

Module *Type_defined_module(Type *t) {
  switch ((TypeKind)t->kind) {
  case BaseT:
    return global_module();
  case StructT:
  case CStructT:
  case UnionT:
    return t->structT->module;
  case EnumT:
  case CEnumT:
    return t->enumT->module;
  case InterfaceT:
    return t->interfaceT->module;
  case FnT:
    return t->fnT->module;
  case MacroT:
    return global_module();
  case ArrayT:
  case SliceT:
  case DynArrayT:
  case PointerT: {
    if (!t->child)
      FATALX("missing base type for pointer or array.");
    while (t->child)
      t = t->child;
    return Type_defined_module(t);
  }
  case PlaceHolder:
    return t->placeholerModule;

  case ConstantWrapperT:
    return Type_defined_module(t->child);

  case UseT:
    FATALX("Can't get defined module for use-type.");
    break;
  }
  return NULL;
}

Type *Type_remove_referening(Type *t) {
  while (t->kind == PointerT || t->kind == SliceT || t->kind == DynArrayT || t->kind == ArrayT)
    t = t->child;
  return t;
}

LocationRange *Type_location(Type *t) {
  switch ((TypeKind)t->kind) {
  case StructT:
  case CStructT:
  case UnionT:
    return &t->structT->location;
  case EnumT:
  case CEnumT:
    return &t->enumT->location;
  case InterfaceT:
    return &t->interfaceT->location;
  case MacroT:
    break;
  case FnT:
    return &t->fnT->location;
  case UseT:
    return &t->useT->location;
  case ConstantWrapperT:
    return Type_location(t->child);
  case ArrayT:
  case SliceT:
  case DynArrayT:
  case PointerT:
  case PlaceHolder:
  case BaseT:
    break;
  }
  return NULL;
}

typedef struct TypeList {
  Type *type;
  TypeList *next;
} TypeList;

typedef struct ConstantList ConstantList;
typedef struct ConstantList {
  Expression *autotype;
  ConstantList *next;
  bool is_extern_c;
} ConstantList;

typedef struct Module {
  const char *path;
  const char *c_name;
  TypeList *types;
  ConstantList *constants;
  bool finished;
  Module *next;
} Module;

typedef struct CBlock CBlock;
typedef struct CBlock {
  const char *block;
  bool at_start;
  CBlock *next;
} CBlock;

typedef enum ProgramMode {
  Run = 0,
  Symbols,
  Declaration,
  References,
  Build,
  Transpile,
} ProgramMode;

typedef struct Program {
  struct {
    char *buffer;
    size_t len;
    size_t cap;
  } arena;

  Module *modules;
  CBlock *cblocks;
  const char *main_file;
  const char *output;
  ProgramMode mode;
} Program;

Module global = (Module){"", "", NULL, NULL, true, NULL};

Module *global_module() { return &global; }

Program Program_new(char *buffer, size_t cap) {
  Program p;
  p.arena.buffer = buffer;
  p.arena.len = 0;
  p.arena.cap = cap;
  p.modules = &global;
  p.cblocks = NULL;
  p.main_file = NULL;
  p.output = JNQ_BIN;
  p.mode = Run;
  return p;
}

void *Program_alloc(Program *p, size_t size) {
  if (p->arena.len + size >= p->arena.cap)
    FATALX("Out of memory");
  void *d = (p->arena.buffer + p->arena.len);
  p->arena.len += size;
  return d;
}

typedef struct StringView {
  const char *text;
  int size;
} StringView;

static inline StringView sv(const char *c, int s) { return (StringView){c, s}; }
static inline StringView c_sv(const char *c) { return (StringView){c, strlen(c)}; }
static inline StringView be_sv(const char *b, const char *e) { return (StringView){b, e - b}; }

bool sv_eq(StringView a, StringView b) { return a.size == b.size && strncmp(a.text, b.text, a.size) == 0; }

char *Program_copy_string(Program *p, StringView s) {
  char *id = (char *)Program_alloc(p, s.size + 1);
  strncpy(id, s.text, s.size);
  id[s.size] = '\0';
  return id;
}

char *Program_copy_str(Program *p, BuffString s) { return Program_copy_string(p, c_sv(s.s)); }

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

void Program_reset_module_finished(Program *p) {
  global.finished = false;
  for (Module *m = p->modules; m; m = m->next)
    m->finished = false;
}

bool Type_is_import_type(Type *t) {
  return t->kind == StructT || t->kind == CStructT || t->kind == InterfaceT || t->kind == UnionT || t->kind == FnT ||
         t->kind == CEnumT || t->kind == EnumT || t->kind == PlaceHolder;
}

Type Null = (Type){"null_t", .c_name = "null_t", BaseT, NULL};
Type Bool = (Type){"bool", .c_name = "bool", BaseT, NULL};
Type Char = (Type){"char", .c_name = "char", BaseT, NULL};
Type WChar = (Type){"wchar_t", .c_name = "wchar_t", BaseT, NULL};
Type i8 = (Type){"i8", .c_name = "int8_t", BaseT, NULL};
Type i16 = (Type){"i16", .c_name = "int16_t", BaseT, NULL};
Type i32 = (Type){"i32", .c_name = "int32_t", BaseT, NULL};
Type i64 = (Type){"i64", .c_name = "int64_t", BaseT, NULL};
Type u8 = (Type){"u8", .c_name = "uint8_t", BaseT, NULL};
Type u16 = (Type){"u16", .c_name = "uint16_t", BaseT, NULL};
Type u32 = (Type){"u32", .c_name = "uint32_t", BaseT, NULL};
Type u64 = (Type){"u64", .c_name = "uint64_t", BaseT, NULL};
Type f32 = (Type){"f32", .c_name = "float", BaseT, NULL};
Type f64 = (Type){"f64", .c_name = "double", BaseT, NULL};
Type String = (Type){"string", .c_name = "char *", BaseT, NULL};
Type FnPtr = (Type){"fn_ptr", .c_name = "void *", BaseT, NULL};
Type Any = (Type){"any", .c_name = "void *", BaseT, NULL};
Type Void = (Type){"void", .c_name = "void", BaseT, NULL};

Type Ellipsis = (Type){"...", .structT = &(Struct){{}, &global, (LocationRange){}}, BaseT, NULL};

Type *Module_constant_type(Module *m, StringView name) {
  for (ConstantList *cl = m->constants; cl; cl = cl->next)
    if (sv_eq(name, c_sv(cl->autotype->autotype->name)))
      return cl->autotype->autotype->type;

  for (TypeList *tl = m->types; tl; tl = tl->next) {
    if (tl->type->kind == UseT && tl->type->useT->take_all) {
      Type *mtt = Module_constant_type(tl->type->useT->module, name);
      if (mtt)
        return mtt;
    } else if (tl->type->kind == UseT && tl->type->useT->type_len > 0) {
      // missing
    }
  }
  return NULL;
}

Type *Module_find_type(Module *m, StringView name) {
  if (sv_eq(name, (StringView){Bool.name, 4}))
    return &Bool;
  if (sv_eq(name, (StringView){i8.name, 2}))
    return &i8;
  if (sv_eq(name, (StringView){i16.name, 3}))
    return &i16;
  if (sv_eq(name, (StringView){"int", 3}) || sv_eq(name, (StringView){i32.name, 3}))
    return &i32;
  if (sv_eq(name, (StringView){i64.name, 3}))
    return &i64;
  if (sv_eq(name, (StringView){u8.name, 2}))
    return &u8;
  if (sv_eq(name, (StringView){u16.name, 3}))
    return &u16;
  if (sv_eq(name, (StringView){u32.name, 3}))
    return &u32;
  if (sv_eq(name, (StringView){u64.name, 3}) || sv_eq(name, (StringView){"size_t", 6}))
    return &u64;
  if (sv_eq(name, (StringView){f32.name, 3}) || sv_eq(name, (StringView){"float", 5}))
    return &f32;
  if (sv_eq(name, (StringView){f64.name, 3}) || sv_eq(name, (StringView){"double", 6}))
    return &f64;
  if (sv_eq(name, (StringView){Char.name, 4}))
    return &Char;
  if (sv_eq(name, (StringView){WChar.name, 7}))
    return &WChar;
  if (sv_eq(name, (StringView){FnPtr.name, 6}))
    return &FnPtr;
  if (sv_eq(name, (StringView){Any.name, 3}))
    return &Any;

  for (TypeList *tl = m->types; tl; tl = tl->next) {
    if (tl->type->kind == UseT && tl->type->useT->take_all) {
      for (TypeList *xtll = tl->type->useT->module->types; xtll; xtll = xtll->next) {
        if (Type_is_import_type(xtll->type) && sv_eq(name, c_sv(xtll->type->name)))
          return xtll->type;
      }
    } else if (tl->type->kind == UseT && tl->type->useT->type_len > 0) {
      Type **xt = tl->type->useT->type;
      for (int i = 0; i < tl->type->useT->type_len; ++i) {
        if (Type_is_import_type(xt[i]) && sv_eq(name, c_sv(xt[i]->name)))
          return xt[i];
      }
    } else if (sv_eq(name, c_sv(tl->type->name))) {
      return tl->type;
    }
  }
  for (TypeList *tl = global.types; tl; tl = tl->next)
    if (sv_eq(name, c_sv(tl->type->name)))
      return tl->type;
  return NULL;
}

Type *Program_add_type(Program *p, TypeKind k, const char *name, Module *m);

Type *Module_type_or_placeholder(Program *p, Module *m, StringView name) {
  Type *t = Module_find_type(m, name);
  if (t)
    return t;
  const char *n = Program_copy_string(p, name);
  return Program_add_type(p, PlaceHolder, n, m);
}

bool Type_equal(Type *t1, Type *t2) {
  if (!t1 || !t2)
    FATALX("there should be no null type pointer?!");

  if (t1->kind == ConstantWrapperT)
    return Type_equal(t1->child, t2);
  if (t2->kind == ConstantWrapperT)
    return Type_equal(t1, t2->child);

  if (t1 == &Null)
    return t2 == &Null || t2->kind == PointerT || t2->kind == DynArrayT || t2->kind == InterfaceT || t2 == &Any;
  if (t2 == &Null)
    return t1 == &Null || t1->kind == PointerT || t1->kind == DynArrayT || t1->kind == InterfaceT || t2 == &Any;

  if ((t1->kind == PointerT && t2->kind == ArrayT) || (t2->kind == PointerT && t1->kind == ArrayT))
    return Type_equal(t1->child, t2->child);
  if ((t1->kind == PointerT && t2->kind == DynArrayT) || (t2->kind == PointerT && t1->kind == DynArrayT))
    return Type_equal(t1->child, t2->child);

  if (t1 == &String && t2->kind == PointerT && t2->child == &Char)
    return true;
  if (t2 == &String && t1->kind == PointerT && t1->child == &Char)
    return true;

  if (t1 == &Ellipsis || t2 == &Ellipsis)
    return true;

  return t1 == t2;
}

bool Type_convertable(Type *expect, Type *got) {
  if (Type_equal(expect, got))
    return true;

  if (expect->kind == ConstantWrapperT)
    return Type_convertable(expect->child, got);
  if (got->kind == ConstantWrapperT)
    return Type_convertable(expect, got->child);

  if (expect->kind == BaseT && got->kind == BaseT) {
    if (expect == &f64 && got == &f32)
      return true;

    if (expect == &i64 && (got == &i32 || got == &i16 || got == &i8 || got == &u32 || got == &u16 || got == &u8))
      return true;
    if (expect == &i32 && (got == &i16 || got == &i8 || got == &u16 || got == &u8))
      return true;
    if (expect == &i16 && (got == &i8 || got == &u8))
      return true;

    if (expect == &u64 && (got == &u32 || got == &u16 || got == &u8))
      return true;
    if (expect == &u32 && (got == &u16 || got == &u8))
      return true;
    if (expect == &u16 && got == &u8)
      return true;
  }

  if (expect->kind == ArrayT && got->kind == ArrayT && Type_convertable(expect->child, got->child)) {
    if (got->array_count <= expect->array_count)
      return true;
  }

  if (expect->kind == SliceT && (got->kind == ArrayT || got->kind == DynArrayT) &&
      Type_convertable(expect->child, got->child)) {
    return true;
  }

  if (expect == &FnPtr && (TypeKind)got->kind == FnT) {
    return true;
  }

  if (expect == &Any &&
      ((TypeKind)got->kind == ArrayT || (TypeKind)got->kind == PointerT || (TypeKind)got->kind == DynArrayT)) {
    return true;
  }

  if (expect == &Bool && (got == &Any || (TypeKind)got->kind == PointerT || (TypeKind)got->kind == DynArrayT ||
                          (TypeKind)got->kind == InterfaceT)) {
    return true;
  }

  return false;
}

BuffString Type_name(Type *t) {
  if (!t)
    return (BuffString){{0}};

  BuffString s;
  char *ss = s.s;
  int i = 0;
  while (t->child) {
    switch ((TypeKind)t->kind) {
    case DynArrayT:
      i += snprintf(ss + i, sizeof(s.s) - i, "new[%d]", t->array_count);
      break;
    case SliceT:
      i += snprintf(ss + i, sizeof(s.s) - i, "[:]");
      break;
    case ArrayT:
      if (t->array_count > 0)
        i += snprintf(ss + i, sizeof(s.s) - i, "[%d]", t->array_count);
      else
        i += snprintf(ss + i, sizeof(s.s) - i, "[]");
      break;
    case PointerT:
      i += snprintf(ss + i, sizeof(s.s) - i, "*");
      break;

    case ConstantWrapperT:
      return Type_name(t->child);
      break;
    case UseT:
    case StructT:
    case CStructT:
    case InterfaceT:
    case UnionT:
    case EnumT:
    case CEnumT:
    case MacroT:
    case FnT:
    case BaseT:
    case PlaceHolder:
      i += snprintf(ss + i, sizeof(s.s) - i, "<>");
      break;
    }
    t = t->child;
  }
  i += snprintf(ss + i, sizeof(s.s) - i, "%s", t->name);
  return s;
}

Module *Program_add_module(Program *p, const char *pathc) {
  size_t size = strlen(pathc);
  const char *path = Program_copy_string(p, sv(pathc, size));
  char *cname = Program_copy_string(p, sv(pathc, size + 1));
  for (char *c = cname; *c; ++c)
    if (*c == '.')
      *c = '_';
  cname[size] = '_';
  cname[size + 1] = 0;
  Module *m = (Module *)Program_alloc(p, sizeof(Module));
  m->path = path;
  m->c_name = cname;
  m->types = NULL;
  m->constants = NULL;
  m->next = p->modules;
  p->modules = m;
  return m;
}

Type *Program_add_type(Program *p, TypeKind k, const char *name, Module *m) {
  Type *tt = NULL;
  if (name && name[0] != '\0') {
    tt = Module_find_type(m, c_sv(name));
    if (tt && tt->kind != PlaceHolder)
      FATALX("Type '%s' allready defined!", name);
  }
  bool was_placeholder = tt && tt->kind == PlaceHolder;
  if (!tt)
    tt = (Type *)Program_alloc(p, sizeof(Type));

  tt->name = name;
  tt->kind = k;
  switch ((TypeKind)k) {
  case UseT:
    tt->useT = (Use *)Program_alloc(p, sizeof(Use));
    tt->useT->type = NULL;
    tt->useT->type_len = 0;
    tt->useT->module = NULL;
    tt->useT->take_all = false;
    break;
  case StructT:
  case CStructT:
  case UnionT:
    tt->structT = (Struct *)Program_alloc(p, sizeof(Struct));
    tt->structT->member = (VariableList){NULL, 0};
    tt->structT->module = m;
    break;
  case EnumT:
  case CEnumT:
    tt->enumT = (Enum *)Program_alloc(p, sizeof(Enum));
    tt->enumT->entries = NULL;
    tt->enumT->module = m;
    break;
  case InterfaceT:
    tt->interfaceT = (Interface *)Program_alloc(p, sizeof(Interface));
    tt->interfaceT->methods = (FnVec){NULL, 0};
    tt->interfaceT->module = m;
    tt->interfaceT->used_types = NULL;
    break;
  case FnT:
    tt->fnT = (Function *)Program_alloc(p, sizeof(Function));
    tt->fnT->body = NULL;
    tt->fnT->is_extern_c = false;
    tt->fnT->module = m;
    break;
  case MacroT:
    tt->macro_name = "missing_name";
  case PlaceHolder:
    tt->placeholerModule = m;
    break;
  case ConstantWrapperT:
    tt->constantModule = m;
    break;
  case SliceT:
  case ArrayT:
  case DynArrayT:
    tt->array_count = 0;
    break;
  case BaseT:
    FATALX("internal error");
    break;
  case PointerT:
    break;
  }
  if (was_placeholder) {
    if (m->types->type == tt)
      return tt;
    for (TypeList *tl = m->types; tl->next; tl = tl->next) {
      if (tl->next->type == tt) {
        TypeList *tlt = tl->next;
        tl->next = tlt->next;
        tlt->next = m->types;
        m->types = tlt;
        return tt;
      }
    }
    FATALX("did not find place holder type '%s'", Type_name(tt).s);
    return tt;
  }

  TypeList *tl = (TypeList *)Program_alloc(p, sizeof(TypeList));
  tl->type = tt;
  tl->next = m->types;
  m->types = tl;
  return tt;
}

ConstantList *Program_add_constant(Program *p, Module *m, Expression *e) {
  ConstantList *cl = (ConstantList *)Program_alloc(p, sizeof(ConstantList));
  cl->autotype = e;
  cl->is_extern_c = false;
  cl->next = m->constants;
  m->constants = cl;
  return cl;
}

Type *Program_add_type_after(Program *p, TypeKind k, Module *m, Type *child) {
  Type *t = Program_add_type(p, k, "", m);
  t->child = child;
  if (!m->types || t != m->types->type)
    FATALX("internal error expect new types to be added up front!");

  TypeList *vecT = m->types;
  m->types = m->types->next;

  if (Type_defined_module(child) == &global) {
    vecT->next = global.types;
    global.types = vecT;
    vecT = NULL;
  } else {
    for (TypeList *tl = m->types; tl; tl = tl->next) {
      if (tl->type != child)
        continue;

      vecT->next = tl->next;
      tl->next = vecT;
      vecT = NULL;
      break;
    }
  }

  if (vecT != NULL)
    FATALX("internal error did not find child type for '%s'!", Type_name(t).s);

  return t;
}

Statement *Program_new_Statement(Program *p, StatementType t, Statement *n) {
  Statement *s = (Statement *)Program_alloc(p, sizeof(Statement));
  s->type = t;
  switch ((StatementType)t) {
  case ExpressionS:
    s->express = (ExpressionStatement *)Program_alloc(p, sizeof(ExpressionStatement));
    break;
  case Return:
    s->ret = (ReturnStatement *)Program_alloc(p, sizeof(ReturnStatement));
    break;
  case Break:
    s->brk = (BreakStatement *)Program_alloc(p, sizeof(BreakStatement));
    break;
  case Default:
    s->defaultS = (DefaultStatement *)Program_alloc(p, sizeof(DefaultStatement));
    break;
  case Delete:
    s->deleteS = (DeleteStatement *)Program_alloc(p, sizeof(DeleteStatement));
    s->deleteS->e = NULL;
    break;
  case Continue:
    s->cont = (ContinueStatement *)Program_alloc(p, sizeof(ContinueStatement));
    break;
  case Case:
    s->caseS = (CaseStatement *)Program_alloc(p, sizeof(CaseStatement));
    break;
  case Scope:
    s->scope = (ScopeStatement *)Program_alloc(p, sizeof(ScopeStatement));
    break;
  case If:
    s->ifS = (IfStatement *)Program_alloc(p, sizeof(IfStatement));
    break;
  case For:
    s->forS = (ForStatement *)Program_alloc(p, sizeof(ForStatement));
    break;
  case While:
    s->whileS = (WhileStatement *)Program_alloc(p, sizeof(WhileStatement));
    break;
  case DoWhile:
    s->doWhileS = (DoWhileStatement *)Program_alloc(p, sizeof(DoWhileStatement));
    break;
  case Switch:
    s->switchS = (SwitchStatement *)Program_alloc(p, sizeof(SwitchStatement));
    break;
  }
  s->next = n;
  return s;
}

Expression *Program_new_Expression(Program *p, ExpressionType t) {
  Expression *e = (Expression *)Program_alloc(p, sizeof(Expression));
  e->type = t;
  switch ((ExpressionType)t) {
  case BaseA:
    e->baseconst = (BaseConst *)Program_alloc(p, sizeof(BaseConst));
    break;
  case IdentifierA:
    e->id = (Identifier *)Program_alloc(p, sizeof(Identifier));
    e->id->type = NULL;
    break;
  case AutoTypeE:
    e->autotype = (AutoTypeDeclaration *)Program_alloc(p, sizeof(AutoTypeDeclaration));
    e->autotype->e = NULL;
    e->autotype->name = NULL;
    e->autotype->type = NULL;
    break;
  case BraceE:
    e->brace = (Brace *)Program_alloc(p, sizeof(Brace));
    break;
  case CallE:
    e->call = (Call *)Program_alloc(p, sizeof(Call));
    break;
  case ConstructE:
    e->construct = (Construct *)Program_alloc(p, sizeof(Construct));
    e->construct->pointer = false;
    break;
  case NewE:
    e->newE = (New *)Program_alloc(p, sizeof(New));
    e->newE->o = NULL;
    break;
  case AccessE:
    e->access = (Access *)Program_alloc(p, sizeof(Access));
    e->access->o = NULL;
    e->access->p = NULL;
    break;
  case SliceE:
    e->slice = (Slice *)Program_alloc(p, sizeof(Slice));
    e->slice->o = e->slice->begin = e->slice->end = NULL;
    break;
  case MemberAccessE:
    e->member = (MemberAccess *)Program_alloc(p, sizeof(MemberAccess));
    break;
  case AsCast:
    e->cast = (Cast *)Program_alloc(p, sizeof(Cast));
    break;
  case UnaryPrefixE:
    e->unpre = (UnaryPrefix *)Program_alloc(p, sizeof(UnaryPrefix));
    break;
  case UnaryPostfixE:
    e->unpost = (UnaryPostfix *)Program_alloc(p, sizeof(UnaryPostfix));
    break;
  case BinaryOperationE:
    e->binop = (BinaryOperation *)Program_alloc(p, sizeof(BinaryOperation));
    break;
  case TernaryOperationE:
    e->ternop = (TernaryOperation *)Program_alloc(p, sizeof(TernaryOperation));
    e->ternop->condition = e->ternop->if_e = e->ternop->else_e = NULL;
    break;
  case CDelegateE:
    e->cdelegate = (CDelegate *)Program_alloc(p, sizeof(CDelegate));
    e->cdelegate->o = NULL;
    e->cdelegate->delegate = NULL;
    break;
  }
  return e;
}

bool skip_whitespace_space(State *st) {
  State old = *st;
  while (*st->c && isspace(*st->c)) {
    if (*st->c == '\n') {
      st->location.line++;
      st->location.column = 1;
    } else
      ++st->location.column;
    ++st->c;
  }
  return old.c > st->c;
}

bool skip_line_comment(State *st) {
  if (st->c[0] && st->c[0] == '/' && st->c[1] == '/') {
    while (*st->c) {
      if (*st->c == '\n') {
        st->location.line++;
        st->location.column = 1;
        ++st->c;
        break;
      } else {
        ++st->location.column;
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
  if (st->location.line > 1)
    for (size_t i = 1; i <= st->location.column; ++i)
      if (!isspace(st->c[-i]))
        return false;
  return st->location.line > 1;
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

bool check_key_word(const char *b, const char *e) {
  if ((e - b) == 2 && strncmp(b, "as", 2) == 0)
    return true;
  if ((e - b) == 2 && strncmp(b, "if", 2) == 0)
    return true;
  // if ((e - b) == 2 && strncmp(b, "fn", 2) == 0)
  //   return true;
  if ((e - b) == 2 && strncmp(b, "do", 2) == 0)
    return true;
  // if ((e - b) == 3 && strncmp(b, "cfn", 3) == 0)
  //   return true;
  if ((e - b) == 3 && strncmp(b, "for", 3) == 0)
    return true;
  // if ((e - b) == 3 && strncmp(b, "use", 3) == 0)
  //   return true;
  // if ((e - b) == 4 && strncmp(b, "type", 4) == 0)
  //   return true;
  if ((e - b) == 5 && strncmp(b, "while", 5) == 0)
    return true;
  return false;
}

bool check_type_key_word_at(const char *b) {
  return strncmp(b, "fn", 2) == 0 || strncmp(b, "cfn", 3) == 0 || strncmp(b, "use", 3) == 0 ||
         strncmp(b, "type", 4) == 0;
}

bool check_identifier(State *st) {
  State old = *st;
  if (st->c[0] && (isalpha(st->c[0]) || st->c[0] == '_')) {
    while (st->c[0] && (isalnum(st->c[0]) || st->c[0] == '_'))
      State_skip(st, 1);
  }
  if (old.c < st->c && !check_key_word(old.c, st->c))
    return true;
  *st = old;
  return false;
}

bool read_int(State *st, int *i, Type **t) {
  char *end;
  *i = strtol(st->c, &end, 10);
  if (end == st->c)
    return false;
  if (*end == 'u') {
    *t = &u32;
    end++;
    if (*end == 'l') {
      *t = &u64;
      end++;
    }
  } else if (*end == 'l') {
    *t = &i64;
    end++;
    if (*end == 'u') {
      *t = &u64;
      end++;
    }
  } else
    *t = &i32;
  st->location.column += end - st->c;
  st->c = end;
  return true;
}

bool read_float(State *st, double *f) {
  char *end;
  *f = strtod(st->c, &end);
  if (end == st->c)
    return false;
  st->location.column += end - st->c;
  st->c = end;
  return true;
}

Module *Program_parse_file(Program *p, const char *path);

Module *Program_parse_sub_file(Program *p, const char *path, Module *parent) {

  char tempPath[256];
  strcpy(tempPath, parent->path);
  int l = strlen(tempPath) - 1;
  while (l >= 0) {
    if (tempPath[l] == '.') {
      tempPath[l + 1] = '\0';
      break;
    }
    --l;
  }
  strcat(tempPath, path);
  Module *mod = Program_parse_file(p, tempPath);
  if (!mod)
    mod = Program_parse_file(p, path);

  if (!mod) {
    strcpy(tempPath, lib_path);
    strcat(tempPath, path);
    mod = Program_parse_file(p, tempPath);
  }

  return mod;
}

bool Program_parse_use_path(Program *p, Module *m, State *st) {
  Location sl = back(st, 3);
  skip_whitespace(st);
  State old = *st;

  StringView imp[32];
  int imp_len = 0;

  bool is_type_list = false;
  bool take_all = false;
  const char *b = st->c;
  if (check_identifier(st)) {
    imp[imp_len++] = (StringView){b, st->c - b};
    for (;;) {
      if (check_word(st, "from")) {
        is_type_list = true;
        break;
      }
      if (check_op(st, ",")) {
        skip_whitespace(st);
        b = st->c;
        if (check_identifier(st)) {
          if (imp_len > (int)(sizeof(imp) / sizeof(StringView)))
            FATALX("too many use imports!");
          imp[imp_len++] = (StringView){b, st->c - b};
          continue;
        }
      }
      break;
    }
  } else if (check_op(st, "*") && check_word(st, "from")) {
    is_type_list = true;
    take_all = true;
  }

  if (!is_type_list)
    FATAL(&old.location, "missing import type list");

  skip_whitespace(st);
  old = *st;

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
        FATAL(&st->location, "Error parsing use statement!");
      name = buffer + inb;
      inb += snprintf(buffer + inb, 256 - inb, "%.*s", (int)(st->c - old.c), old.c);
      old = *st;
    }
  } else
    return false;

  Module *use = Program_parse_sub_file(p, buffer, m);
  if (!use)
    FATAL(&old.location, "import not found '%s'", buffer);

  const char *n = Program_copy_string(p, c_sv(name));
  Use *u = Program_add_type(p, UseT, n, m)->useT;
  u->module = use;
  u->location = NewRange(sl, st->location);

  u->take_all = take_all;
  if (!take_all) {
    u->type_len = imp_len;
    u->type = Program_alloc(p, imp_len * sizeof(Type *));
    for (int i = 0; i < imp_len; ++i) {
      u->type[i] = Module_type_or_placeholder(p, use, imp[i]);
      if (!u->type[i])
        FATALX("Did not found type '%*.s' in '%s'", imp[i].size, imp[i].text, name);
    }
  }

  return true;
}

bool Program_check_declared_type(Program *p, State *st) {
  skip_whitespace(st);
  State old = *st;

  if (check_op(st, "*")) {
    if (Program_check_declared_type(p, st))
      return true;
    *st = old;
  }

  if (check_op(st, "[")) {
    if (!check_op(st, "*") && !check_op(st, ":")) {
      int count = -1;
      Type *t_count;
      read_int(st, &count, &t_count);
    }
    if (check_op(st, "]")) {
      if (Program_check_declared_type(p, st))
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

Type *Module_find_pointer_type(Module *m, Type *child) {
  for (TypeList *tl = m->types; tl; tl = tl->next)
    if (tl->type->kind == PointerT && tl->type->child == child)
      return tl->type;
  return NULL;
}

Type *Module_find_slice_type(Module *m, Type *child) {
  for (TypeList *tl = m->types; tl; tl = tl->next)
    if (tl->type->kind == SliceT && tl->type->child == child)
      return tl->type;
  return NULL;
}

Type *Module_find_dyn_array_type(Module *m, Type *child) {
  for (TypeList *tl = m->types; tl; tl = tl->next)
    if (tl->type->kind == DynArrayT && tl->type->child == child)
      return tl->type;
  return NULL;
}

Type *Module_find_array_type(Module *m, int count, Type *child) {
  for (TypeList *tl = m->types; tl; tl = tl->next)
    if (tl->type->kind == ArrayT && tl->type->array_count == count && tl->type->child == child)
      return tl->type;
  return NULL;
}

Type *Program_parse_declared_type(Program *p, Module *m, State *st, bool as_argument) {
  skip_whitespace(st);
  State old = *st;

  if (check_op(st, "*")) {
    Type *c = Program_parse_declared_type(p, m, st, as_argument);
    if (c) {
      Module *cm = Type_defined_module(c);
      if (!cm)
        FATALX("internal problem finding module for type");
      Type *td = Module_find_pointer_type(cm, c);
      if (!td) {
        td = Program_add_type(p, PointerT, "", cm);
        td->child = c;
      }
      return td;
    }
    *st = old;
  }

  if (check_op(st, "[")) {
    if (check_op(st, "*")) {
      if (check_op(st, "]")) {
        Type *c = Program_parse_declared_type(p, m, st, as_argument);
        if (c) {
          Module *cm = Type_defined_module(c);
          if (!cm)
            FATALX("internal problem finding module for type");
          Type *td = Module_find_dyn_array_type(cm, c);
          if (!td) {
            td = Program_add_type(p, DynArrayT, "", cm);
            td->array_count = -1;
            td->child = c;
          }
          return td;
        }
      }
    } else if (as_argument && check_op(st, "]")) {
      Type *c = Program_parse_declared_type(p, m, st, as_argument);
      if (c) {
        Module *cm = Type_defined_module(c);
        if (!cm)
          FATALX("internal problem finding module for type");
        Type *td = Module_find_slice_type(cm, c);
        if (!td) {
          td = Program_add_type(p, SliceT, "", cm);
          // td->array_count = -1;
          td->child = c;
        }
        return td;
      }
    } else {
      int count = -1;
      Type *t_count;
      read_int(st, &count, &t_count);
      if (check_op(st, "]")) {
        Type *c = Program_parse_declared_type(p, m, st, as_argument);
        if (c) {
          Module *cm = Type_defined_module(c);
          if (!cm)
            FATALX("internal problem finding module for type");
          Type *td = Module_find_array_type(cm, count, c);
          if (!td) {
            td = Program_add_type(p, ArrayT, "", cm);
            td->array_count = count;
            td->child = c;
          }
          return td;
        }
      }
    }
    *st = old;
  }

  if (check_identifier(st)) {
    Type *t = Module_type_or_placeholder(p, m, be_sv(old.c, st->c));
    if (t) {
      old = *st;
      if (t->kind == UseT && check_op(st, ".")) {
        skip_whitespace(st);
        const char *s = st->c;
        if (check_identifier(st)) {
          if ((t = Module_type_or_placeholder(p, t->useT->module, be_sv(s, st->c))))
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

Variable Program_parse_variable_declaration(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State old = *st;

  const bool is_const = check_word(st, "const");
  skip_whitespace(st);
  const char *name_beg = st->c;

  Type *type = NULL;
  if (check_identifier(st)) {
    const char *name_end = st->c;
    skip_whitespace(st);
    if (!check_whitespace_for_nl(st)) {
      if ((type = Program_parse_declared_type(p, m, st, true))) {
        return (Variable){Program_copy_string(p, be_sv(name_beg, name_end)), type, NewRange(old.location, st->location),
                          is_const};
      }
    }
    *st = old;
  }
  return (Variable){NULL, NULL, {}, false};
}

VariableList Program_parse_variable_declaration_list(Program *p, Module *m, State *st, const char *end) {
  skip_whitespace(st);

  Variable v[256];
  int v_len = 0;

  while (*st->c) {
    if (check_op(st, end)) {
      VariableList vl = {Program_alloc(p, v_len * sizeof(Variable)), v_len};
      for (int i = 0; i < v_len; ++i)
        vl.v[i] = v[i];
      return vl;
    }

    if (v_len >= 256)
      FATALX("Out of internal memory for variable list!");
    v[v_len] = Program_parse_variable_declaration(p, m, st);
    if (!v[v_len].name && check_op(st, "...")) {
      v[v_len].name = "...";
      v[v_len].location = NewRange(back(st, 3), st->location);
      v[v_len].type = &Ellipsis;
    }
    if (!v[v_len].name)
      FATAL(&st->location, "missing variable name");

    v_len++;
    check_op(st, ",");
  }
  FATAL(&st->location, "Missing closing '%s'", end);
  return (VariableList){NULL, 0};
}

EnumEntry *Program_parse_enum_entry_list(Program *p, State *st) {
  skip_whitespace(st);
  EnumEntry *top = NULL;
  while (*st->c) {
    if (check_op(st, "}")) {
      reverse_list(EnumEntry, top);
      return top;
    }

    skip_whitespace(st);
    State old = *st;
    EnumEntry *e = (EnumEntry *)Program_alloc(p, sizeof(EnumEntry));
    if (check_identifier(st)) {
      e->location = old.location;
      e->name = Program_copy_string(p, be_sv(old.c, st->c));
      e->valueSet = false;
      if (check_op(st, "=")) {
        e->valueSet = true;
        int value;
        Type *t_value;
        if (!read_int(st, &value, &t_value))
          FATAL(&st->location, "missing enum entry value ");
        e->value = value;
      }
    } else
      FATAL(&st->location, "missing enum entry name");
    check_op(st, ",");
    e->next = top;
    top = e;
  }
  FATAL(&st->location, "Missing closing '}' for enum");
  return NULL;
}

bool Program_parse_fn_decl(Program *p, Module *m, FunctionDecl *fnd, State *st) {
  skip_whitespace(st);
  State old = *st;

  if (check_op(st, "(")) {
    fnd->parameter = Program_parse_variable_declaration_list(p, m, st, ")");
    skip_whitespace(st);
    Location rt_start = st->location;
    if (!check_type_key_word_at(st->c))
      fnd->returnType = Program_parse_declared_type(p, m, st, true);
    fnd->return_type_location = NewRange(rt_start, st->location);
    if (!fnd->returnType)
      fnd->returnType = &Void;
    return true;
  }

  *st = old;
  return false;
}

FnVec Program_parse_interface_fns(Program *p, Module *m, Type *in, State *st) {
  skip_whitespace(st);

  Type tt[32];
  int tt_len = 0;

  while (*st->c) {
    if (check_op(st, "}")) {
      FnVec tv = {Program_alloc(p, tt_len * sizeof(Type)), tt_len};
      for (int i = 0; i < tt_len; ++i)
        tv.fns[i] = tt[i];
      return tv;
    }

    if (tt_len >= 32)
      FATALX("Out of internal memory for function list!");

    if (!check_word(st, "fn"))
      FATAL(&st->location, "expect a function declaration!");
    skip_whitespace(st);
    State old = *st;
    if (!check_identifier(st))
      FATAL(&st->location, "missing function name!");
    BuffString sn = str("%s%.*s", in->name, (int)(st->c - old.c), old.c);
    tt[tt_len].name = Program_copy_string(p, c_sv(sn.s));
    tt[tt_len].kind = FnT;
    tt[tt_len].fnT = (Function *)Program_alloc(p, sizeof(Function));
    tt[tt_len].fnT->module = m;
    tt[tt_len].fnT->body = NULL;
    tt[tt_len].fnT->is_extern_c = false;
    if (!Program_parse_fn_decl(p, m, &tt[tt_len].fnT->d, st))
      FATAL(&st->location, "missing function declaration!");
    tt_len++;
    check_op(st, ",");
  }
  FATAL(&st->location, "Missing closing '}' for interface declaration");
  return (FnVec){};
}

TypeKind Program_parse_type_kind(State *st) {
  if (check_word(st, "union"))
    return UnionT;
  else if (check_word(st, "struct"))
    return StructT;
  else if (check_word(st, "cstruct"))
    return CStructT;
  else if (check_word(st, "enum"))
    return EnumT;
  else if (check_word(st, "cenum"))
    return CEnumT;
  else if (check_word(st, "interface"))
    return InterfaceT;

  return PlaceHolder;
}

void Program_parse_type(Program *p, Module *m, State *st) {
  Location old = back(st, 4);
  skip_whitespace(st);

  const char *name_start = st->c;
  if (check_identifier(st)) {
    const char *name = Program_copy_string(p, be_sv(name_start, st->c));
    TypeKind tk = Program_parse_type_kind(st);
    if (tk == PlaceHolder)
      FATAL(&st->location, "Missing type declaration");
    if (!check_op(st, "{"))
      FATAL(&st->location, "Expect '{' for type declaration");

    if (tk == UnionT || tk == StructT || tk == CStructT) {
      Struct *s = Program_add_type(p, tk, name, m)->structT;
      s->member = Program_parse_variable_declaration_list(p, m, st, "}");
      s->location = NewRange(old, st->location);
    } else if (tk == EnumT || tk == CEnumT) {
      Enum *e = Program_add_type(p, tk, name, m)->enumT;
      e->entries = Program_parse_enum_entry_list(p, st);
      e->location = NewRange(old, st->location);
    } else if (tk == InterfaceT) {
      Type *in = Program_add_type(p, InterfaceT, name, m);
      in->interfaceT->methods = Program_parse_interface_fns(p, m, in, st);
      in->interfaceT->location = NewRange(old, st->location);
    } else
      FATAL(&st->location, "Unhandled type kind");
  } else
    FATAL(&st->location, "Missing type name");
}

Expression *Program_parse_atom(Program *p, State *st) {
  skip_whitespace(st);
  State old = *st;
  if (check_identifier(st)) {
    size_t l = st->c - old.c;

    bool isTrue = l == 4 && strncmp(old.c, "true", 4) == 0;
    if (isTrue || (l == 5 && strncmp(old.c, "false", 5) == 0)) {
      Expression *b = Program_new_Expression(p, BaseA);
      b->baseconst->text = isTrue ? "true" : "false";
      b->baseconst->type = &Bool;
      b->range = NewRangeWord(old.location, b->baseconst->text);
      return b;
    }

    if (l == 4 && strncmp(old.c, "null", 4) == 0) {
      Expression *nu = Program_new_Expression(p, BaseA);
      nu->baseconst->text = "null";
      nu->baseconst->type = &Null;
      nu->range = NewRangeWord(old.location, nu->baseconst->text);
      return nu;
    }

    Expression *e = Program_new_Expression(p, IdentifierA);
    e->id->name = Program_copy_string(p, be_sv(old.c, st->c));
    e->id->type = NULL;
    e->range = NewRangeWord(old.location, e->id->name);
    return e;
  }

  if (check_op(st, "'")) {
    Expression *e = Program_new_Expression(p, BaseA);
    e->baseconst->type = &Char;
    if (!st->c[0])
      FATAL(&st->location, "unclosed char constant");
    if (st->c[0] == '\\') {
      e->baseconst->text = Program_copy_str(p, str("%c%c", st->c[0], st->c[1]));
      State_skip(st, 2);
    } else {
      e->baseconst->text = Program_copy_str(p, str("%c", st->c[0]));
      State_skip(st, 1);
    }
    if (st->c[0] != '\'')
      FATAL(&st->location, "Wrong char constant '%s' '%c'", e->baseconst->text, st->c[0]);
    State_skip(st, 1);
    e->range = NewRangeLength(old.location, strlen(e->baseconst->text) + 2);
    return e;
  }

  if (check_op(st, "\"")) {
    Expression *e = Program_new_Expression(p, BaseA);
    e->baseconst->type = &String;
    bool slash_escaped = false;
    if (st->c[0] != '"') {
      while (st->c[1] != '"' || (st->c[0] == '\\' && !slash_escaped)) {
        if (!st->c[0])
          FATAL(&old.location, "unclosed string constant");
        slash_escaped = st->c[0] == '\\';
        State_skip(st, 1);
      }
      State_skip(st, 1);
    }

    e->baseconst->text = Program_copy_string(p, be_sv(old.c + 1, st->c));
    e->range = NewRangeLength(old.location, strlen(e->baseconst->text) + 2);
    State_skip(st, 1);
    return e;
  }

  double temp_f;
  State f = *st;
  bool is_float = read_float(&f, &temp_f);

  int temp_i;
  Type *t_temp_i;
  if (read_int(st, &temp_i, &t_temp_i) && (!is_float || st->c >= f.c)) {
    Expression *e = Program_new_Expression(p, BaseA);
    e->baseconst->type = t_temp_i;
    e->baseconst->text = Program_copy_string(p, be_sv(old.c, st->c));
    e->range = NewRangeWord(old.location, e->baseconst->text);
    return e;
  } else if (is_float) {
    *st = f;
    Expression *e = Program_new_Expression(p, BaseA);
    if (st->c[0] == 'f') {
      e->baseconst->type = &f32;
      State_skip(st, 1);
    } else {
      e->baseconst->type = &f64;
    }
    e->baseconst->text = Program_copy_string(p, be_sv(old.c, st->c));
    e->range = NewRangeWord(old.location, e->baseconst->text);
    return e;
  }

  *st = old;
  return NULL;
}

Expression *Program_parse_expression(Program *p, Module *m, State *st);

ParameterList Program_parse_parameter_list(Program *p, Module *m, State *st) {
  Parameter param[256];
  int param_len = 0;

  Expression *e = NULL;
  while ((e = Program_parse_expression(p, m, st))) {
    if (param_len >= 256)
      FATALX("out of local memory for parameter list");
    Parameter *pp = &param[param_len++];
    pp->p = e;
    pp->name = NULL;
    if (!check_op(st, ","))
      break;
  }

  ParameterList pl;
  pl.len = param_len;
  pl.p = (Parameter *)Program_alloc(p, (pl.len + 1) * sizeof(Parameter));
  for (int i = 0; i < param_len; ++i)
    pl.p[i] = param[i];

  return pl;
}

ParameterList Program_parse_named_parameter_list(Program *p, Module *m, State *st) {
  Parameter param[32];
  int param_len = 0;

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
    if (param_len >= 32)
      FATALX("out of local memory for parameter list");
    Parameter *pp = &param[param_len++];
    pp->p = e;
    pp->name = Program_copy_string(p, be_sv(old.c, id_end));
    if (!check_op(st, ","))
      break;
  }

  ParameterList pl;
  pl.len = param_len;
  pl.p = (Parameter *)Program_alloc(p, (pl.len + 1) * sizeof(Parameter));
  for (int i = 0; i < param_len; ++i)
    pl.p[i] = param[i];
  return pl;
}

Expression *Program_parse_construction(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State new_loc = *st;

  bool new_construct = check_word(st, "new");
  skip_whitespace(st);
  State old = *st;

  if (Program_check_declared_type(p, st) && check_op(st, "{")) {
    *st = old;
    Type *type = Program_parse_declared_type(p, m, st, false);
    if (type && check_op(st, "{")) {
      Expression *construct = Program_new_Expression(p, ConstructE);
      construct->construct->p = Program_parse_named_parameter_list(p, m, st);
      if (construct->construct->p.len == 0)
        construct->construct->p = Program_parse_parameter_list(p, m, st);
      if (!check_op(st, "}"))
        FATAL(&st->location, "unfinished constructor call, missing '}'");
      construct->construct->type = type;
      construct->range = NewRange(old.location, st->location);
      if (new_construct) {
        Expression *new_c = Program_new_Expression(p, NewE);
        new_c->newE->o = construct;
        new_c->range = NewRange(new_loc.location, st->location);
        return new_c;
      }
      return construct;
    }
  }
  if (new_construct)
    FATAL(&st->location, "broken 'new' construction");

  *st = old;
  return NULL;
}

Expression *Program_parse_array_construction(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State old = *st;

  if (check_op(st, "[")) {
    Expression *construct = Program_new_Expression(p, ConstructE);
    construct->construct->p = Program_parse_parameter_list(p, m, st);
    if (!check_op(st, "]"))
      FATAL(&st->location, "unfinished constructor call, missing ']'");
    construct->construct->type = NULL;
    construct->range = NewRange(old.location, st->location);
    return construct;
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
    skip_whitespace(st);
    const char *b = st->c;
    if (!check_identifier(st))
      FATAL(&st->location, "missing id for member access");
    const char *member = Program_copy_string(p, be_sv(b, st->c));
    Expression *ma = Program_new_Expression(p, MemberAccessE);
    ma->member->o = e;
    ma->member->o_type = NULL;
    ma->member->member = (Identifier *)Program_alloc(p, sizeof(Identifier));
    ma->member->member->name = member;
    ma->member->member->type = NULL;
    ma->range = NewRange(old.location, st->location);
    ma = Program_parse_suffix_expression(p, m, st, ma);
    return ma;
  }

  bool decrement = check_op(st, "--");
  if (decrement || check_op(st, "++")) {
    Expression *post = Program_new_Expression(p, UnaryPostfixE);
    post->unpost->o = e;
    post->unpost->op = decrement ? "--" : "++";
    post->range = NewRangeLength(old.location, 2);
    post = Program_parse_suffix_expression(p, m, st, post);
    return post;
  }

  if (check_op(st, "[")) {
    Expression *a = Program_parse_expression(p, m, st);
    if (!a)
      FATAL(&st->location, "missing '[]' content");
    Expression *acc = NULL;
    if (check_op(st, ":")) {
      acc = Program_new_Expression(p, SliceE);
      acc->slice->o = e;
      acc->slice->begin = a;
      acc->slice->end = Program_parse_expression(p, m, st);
      if (!acc->slice->end) {
        acc->slice->end = Program_new_Expression(p, BaseA);
        acc->slice->end->baseconst->text = "0";
        acc->slice->end->baseconst->type = &i32;
        acc->slice->end->range = NewRangeLength(st->location, 0);
      }
    } else {
      acc = Program_new_Expression(p, AccessE);
      acc->access->o = e;
      acc->access->p = a;
    }
    if (!check_op(st, "]"))
      FATAL(&st->location, "missing closing ']' for subscription '%s'", st->c);
    acc->range = NewRange(old.location, st->location);
    acc = Program_parse_suffix_expression(p, m, st, acc);
    return acc;
  }

  if (check_op(st, "(")) {
    Expression *call = Program_new_Expression(p, CallE);
    call->call->o = e;
    call->call->p = Program_parse_parameter_list(p, m, st);
    if (!check_op(st, ")"))
      FATAL(&st->location, "unfinished function call, missing ')'");
    call->range = NewRange(old.location, st->location);
    call = Program_parse_suffix_expression(p, m, st, call);
    return call;
  }

  if (check_word(st, "as")) {
    Expression *cast = Program_new_Expression(p, AsCast);
    cast->cast->o = e;
    cast->cast->type = Program_parse_declared_type(p, m, st, true);
    cast->range = NewRange(old.location, st->location);
    return cast;
  }
  return e;
}

Expression *Program_parse_auto_declaration_(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State old = *st;

  if (check_identifier(st)) {
    const char *ne = st->c;
    if (check_op(st, ":=")) {
      Expression *at = Program_new_Expression(p, AutoTypeE);
      at->autotype->e = Program_parse_expression(p, m, st);
      if (!at->autotype->e)
        FATAL(&st->location, "missing expression for auto assignment ");
      at->autotype->name = Program_copy_string(p, be_sv(old.c, ne));
      at->range = NewRange(old.location, back_to_none_whitespace(*st, old));
      return at;
    }
  }

  *st = old;
  return NULL;
}

Expression *Program_parse_cconst_declaration(Program *p, State *st) {
  skip_whitespace(st);
  State old = *st;

  if (check_identifier(st)) {
    const char *ne = st->c;
    if (check_op(st, ":=")) {
      skip_whitespace(st);
      if (check_word(st, "cconst")) {
        Expression *at = Program_new_Expression(p, AutoTypeE);
        at->autotype->name = Program_copy_string(p, be_sv(old.c, ne));
        at->autotype->e = NULL;
        at->range = NewRange(old.location, back_to_none_whitespace(*st, old));
        return at;
      }
    }
  }

  *st = old;
  return NULL;
}

Expression *Program_parse_unary_operand(Program *p, Module *m, State *st) {
  Expression *prefix = NULL;
  Location startL = st->location;
  const char *un_pre_ops[] = {"++", "--", "*", "~", "!", "-", "+", "&"};
  for (size_t i = 0; i < sizeof(un_pre_ops) / sizeof(const char *); ++i) {
    if (check_op(st, un_pre_ops[i])) {
      prefix = Program_new_Expression(p, UnaryPrefixE);
      prefix->unpre->op = un_pre_ops[i];
      break;
    }
  }

  Expression *e = NULL;
  State startB = *st;
  if (check_op(st, "(")) {
    e = Program_new_Expression(p, BraceE);
    e->brace->o = Program_parse_expression(p, m, st);
    if (!e->brace->o)
      FATAL(&st->location, "missing '(' content");
    if (!check_op(st, ")"))
      FATAL(&st->location, "missing closing ')'");
  } else if ((e = Program_parse_construction(p, m, st))) {
    ;
  } else if ((e = Program_parse_array_construction(p, m, st))) {
    ;
  } else if ((e = Program_parse_auto_declaration_(p, m, st))) {
    ;
  } else
    e = Program_parse_atom(p, st);

  if (!e && prefix)
    FATAL(&st->location, "prefix operation without expression '%s'", prefix->unpre->op);
  if (!e)
    return NULL;

  e->range = NewRange(startB.location, back_to_none_whitespace(*st, startB));

  e = Program_parse_suffix_expression(p, m, st, e);
  if (prefix) {
    prefix->unpre->o = e;
    prefix->range = NewRange(startL, st->location);
    return prefix;
  }
  return e;
}

Expression *Program_parse_bin_operator(Program *p, State *st) {
  if (check_whitespace_for_nl(st))
    return NULL;

  for (size_t i = 0; i < sizeof(ops) / sizeof(BinOp); ++i) {
    if (check_op(st, ops[i].op)) {
      Expression *bin = Program_new_Expression(p, BinaryOperationE);
      bin->binop->op = &ops[i];
      bin->range = NewRange(back(st, strlen(ops[i].op)), st->location);
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

Expression *Program_brace_expression(Program *p, Module *m, State *st) {
  skip_whitespace(st);
  State old = *st;

  if (check_op(st, "(")) {
    Expression *e = Program_parse_expression(p, m, st);
    if (!check_op(st, ")"))
      FATAL(&old.location, "missing closing ')'");
    return e;
  }

  *st = old;
  return NULL;
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
    eop = Program_parse_bin_operator(p, st);
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

  State old = *st;
  if (check_op(st, "?")) {
    Expression *e = Program_new_Expression(p, TernaryOperationE);
    e->ternop->condition = yard.val_stack[0];
    if (!(e->ternop->if_e = Program_parse_expression(p, m, st)))
      FATAL(&old.location, "expect 1st expression for ternary operation");
    if (!check_op(st, ":"))
      FATAL(&old.location, "expect ':' for ternary operation");
    if (!(e->ternop->else_e = Program_parse_expression(p, m, st)))
      FATAL(&old.location, "expect 2nd expression for ternary operation");
    if (e->ternop->condition->type == BinaryOperationE && e->ternop->condition->binop->op->prec < 100 - 13) {
      Expression *cond = e->ternop->condition->binop->o2;
      e->ternop->condition->binop->o2 = e;
      TernaryOperation *ternop = e->ternop;
      e = e->ternop->condition;
      ternop->condition = cond;
    }
    e->range = NewRange(old.location, st->location);
    return e;
  }

  return yard.val_stack[0];
}

Statement *Program_parse_statement(Program *p, Module *m, State *st, Statement *next);
Statement *Program_parse_case_body(Program *p, Module *m, State *st) {
  Statement *body = NULL;
  while (*st->c) {
    skip_whitespace(st);
    State old = *st;
    if (check_op(st, "}") || check_word(st, "case") || check_word(st, "default")) {
      *st = old;
      reverse_list(Statement, body);
      return body;
    }
    body = Program_parse_statement(p, m, st, body);
    if (!body)
      break;
  }
  FATAL(&st->location, "Missing closing '}' for scope");
  return NULL;
}

Statement *Program_parse_scope(Program *p, Module *m, State *st);
Statement *Program_parse_scope_block(Program *p, Module *m, State *st);
Statement *Program_parse_statement(Program *p, Module *m, State *st, Statement *next) {
  Statement *statement = NULL;
  Expression *temp_e = NULL;
  if (check_op(st, "{")) {
    statement = Program_new_Statement(p, Scope, next);
    statement->scope->body = Program_parse_scope(p, m, st);
  } else if (check_word(st, "return")) {
    statement = Program_new_Statement(p, Return, next);
    skip_whitespace_space(st);
    if (!check_whitespace_for_nl(st))
      statement->express->e = Program_parse_expression(p, m, st);
    else
      statement->express->e = NULL;
  } else if (check_word(st, "delete")) {
    statement = Program_new_Statement(p, Delete, next);
    skip_whitespace_space(st);
    if (!check_whitespace_for_nl(st))
      statement->deleteS->e = Program_parse_expression(p, m, st);
    else
      FATAL(&st->location, "Missing delete expression");
  } else if (check_word(st, "case")) {
    statement = Program_new_Statement(p, Case, next);
    if (!(statement->caseS->caseE = Program_parse_expression(p, m, st)))
      FATAL(&st->location, "Missing case expression");
    if (check_op(st, ":"))
      statement->caseS->body = Program_parse_case_body(p, m, st);
    else
      FATAL(&st->location, "Missing ':' in case statement");
  } else if (check_word(st, "default")) {
    statement = Program_new_Statement(p, Default, next);
    if (check_op(st, ":"))
      statement->defaultS->body = Program_parse_case_body(p, m, st);
    else
      FATAL(&st->location, "Missing ':' for default statement");
  } else if (check_word(st, "break"))
    statement = Program_new_Statement(p, Break, next);
  else if (check_word(st, "continue"))
    statement = Program_new_Statement(p, Continue, next);
  else if (check_word(st, "if")) {
    statement = Program_new_Statement(p, If, next);
    if ((temp_e = Program_brace_expression(p, m, st)))
      statement->ifS->condition = temp_e;
    else
      FATAL(&st->location, "Missing if conditon");
    statement->ifS->ifBody = Program_parse_scope_block(p, m, st);
    if (!statement->ifS->ifBody)
      FATAL(&st->location, "Missing if block");
    if (check_word(st, "else")) {
      statement->ifS->elseBody = Program_parse_scope_block(p, m, st);
      if (!statement->ifS->elseBody)
        FATAL(&st->location, "Missing else block");
    }
  } else if (check_word(st, "for")) {
    statement = Program_new_Statement(p, For, next);
    if (!check_op(st, "("))
      FATAL(&st->location, "Missing for loop description");
    statement->forS->init = Program_parse_expression(p, m, st);
    if (!check_op(st, ";"))
      FATAL(&st->location, "Missing for init");
    statement->forS->condition = Program_parse_expression(p, m, st);
    if (!check_op(st, ";"))
      FATAL(&st->location, "Missing for condition");
    statement->forS->incr = Program_parse_expression(p, m, st);
    if (!check_op(st, ")"))
      FATAL(&st->location, "Missing closing ')' of for condition");
    statement->forS->body = Program_parse_scope_block(p, m, st);
    if (!statement->forS->body)
      FATAL(&st->location, "Missing for block");
  } else if (check_word(st, "while")) {
    statement = Program_new_Statement(p, While, next);
    if ((temp_e = Program_brace_expression(p, m, st)))
      statement->whileS->condition = temp_e;
    else
      FATAL(&st->location, "Missing while conditon");
    statement->whileS->body = Program_parse_scope_block(p, m, st);
    if (!statement->whileS->body)
      FATAL(&st->location, "Missing while block");
  } else if (check_word(st, "do")) {
    statement = Program_new_Statement(p, DoWhile, next);
    statement->doWhileS->body = Program_parse_scope_block(p, m, st);
    if (!check_word(st, "while"))
      FATAL(&st->location, "Missing 'while' for do block");
    if ((temp_e = Program_brace_expression(p, m, st)))
      statement->doWhileS->condition = temp_e;
    else
      FATAL(&st->location, "Missing do while conditon");
  } else if (check_word(st, "switch")) {
    statement = Program_new_Statement(p, Switch, next);
    if ((temp_e = Program_brace_expression(p, m, st)))
      statement->switchS->condition = temp_e;
    else
      FATAL(&st->location, "Missing switch expression");
    statement->switchS->body = Program_parse_scope_block(p, m, st);
  } else {
    skip_whitespace(st);
    if ((temp_e = Program_parse_expression(p, m, st))) {
      statement = Program_new_Statement(p, ExpressionS, next);
      statement->express->e = temp_e;
    }
  }
  if (statement)
    check_op(st, ";");
  return statement;
}
Statement *Program_parse_scope(Program *p, Module *m, State *st) {
  Statement *body = NULL;
  while (*st->c) {
    skip_whitespace(st);
    if (check_op(st, "}")) {
      reverse_list(Statement, body);
      return body;
    }
    body = Program_parse_statement(p, m, st, body);
    if (!body)
      break;
  }
  FATAL(&st->location, "Missing closing '}' for scope");
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

void Program_parse_fn(Program *p, Module *m, State *st, bool extc) {
  Location start = back(st, extc ? 3 : 2);
  skip_whitespace(st);

  const char *b = st->c;
  if (check_identifier(st)) {
    const char *name = Program_copy_string(p, be_sv(b, st->c));
    FunctionDecl fnd = (FunctionDecl){};
    if (!Program_parse_fn_decl(p, m, &fnd, st))
      FATAL(&st->location, "Missing parameterlist");
    Function *fn = Program_add_type(p, FnT, name, m)->fnT;
    fn->d = fnd;
    fn->is_extern_c = extc;
    if (!extc) {
      if (check_op(st, "{"))
        fn->body = Program_parse_scope(p, m, st);
      else
        FATAL(&st->location, "Missing function body");
    }
    fn->location = NewRange(start, st->location);
  } else
    FATAL(&st->location, "Missing type name");
}

void Program_parse_ccode(Program *p, State *st, bool at_start) {
  State old = *st;
  if (!check_op(st, "{"))
    FATAL(&old.location, "missing block start for ccode");

  int open_brace = 1;
  old = *st;
  while (*st->c && open_brace > 0) {
    if (st->c[0] == '{')
      ++open_brace;
    if (st->c[0] == '}')
      --open_brace;

    if (*st->c == '\n') {
      st->location.line++;
      st->location.column = 1;
    } else
      ++st->location.column;
    ++st->c;
  }
  if (open_brace > 0)
    FATAL(&old.location, "missing close block for ccode");

  CBlock *cb = (CBlock *)Program_alloc(p, sizeof(CBlock));
  cb->next = p->cblocks;
  cb->block = Program_copy_string(p, be_sv(old.c, st->c - 1));
  cb->at_start = at_start;
  p->cblocks = cb;
}

void Program_parse_module(Program *p, Module *m, State *st) {
  while (st->c[0]) {
    skip_whitespace(st);
    if (st->c[0]) {
      if (check_word(st, "use"))
        Program_parse_use_path(p, m, st);
      else if (check_word(st, "type"))
        Program_parse_type(p, m, st);
      else if (check_word(st, "fn"))
        Program_parse_fn(p, m, st, false);
      else if (check_word(st, "cfn"))
        Program_parse_fn(p, m, st, true);
      else if (check_word(st, "ccode"))
        Program_parse_ccode(p, st, true);
      else if (check_word(st, "cmain"))
        Program_parse_ccode(p, st, false);
      else {
        Expression *autotype = Program_parse_cconst_declaration(p, st);
        if (autotype)
          Program_add_constant(p, m, autotype)->is_extern_c = true;
        else {
          autotype = Program_parse_auto_declaration_(p, m, st);
          if (!autotype) {
            FATAL(&st->location, "Unknown keyword >>'%s'\n", st->c);
            break;
          }
          Program_add_constant(p, m, autotype);
        }
      }
    }
  }
  reverse_list(TypeList, m->types)
}

Module *Program_parse_file(Program *p, const char *path) {
  Module *m = Program_find_module(p, path);
  if (m)
    return m;

  char tempPath[512] = {0};
  strcpy(tempPath, path);
  for (char *t = tempPath; *t; ++t)
    if (*t == '.')
      *t = '/';
  strcat(tempPath, ".jnq");
  char *code = readFile(tempPath);

  if (!code)
    return NULL;

  const char *st_path = Program_copy_string(p, c_sv(tempPath));
  State st = State_new(code, st_path);
  m = Program_add_module(p, path);
  Program_parse_module(p, m, &st);

  free(code);
  return m;
}

void c_Module_types(FILE *f, Module *m);

BuffString Type_special_cname(Type *t) {
  if (!t)
    return (BuffString){{0}};

  BuffString s;
  char *ss = s.s;
  int i = 0;
  while (t->child) {
    switch ((TypeKind)t->kind) {
    case DynArrayT:
      i += snprintf(ss + i, sizeof(s.s) - i, "_n_%d_", t->array_count);
      break;
    case SliceT:
      i += snprintf(ss + i, sizeof(s.s) - i, "_s_");
      break;
    case ArrayT:
      if (t->array_count > 0)
        i += snprintf(ss + i, sizeof(s.s) - i, "_%d_", t->array_count);
      else
        i += snprintf(ss + i, sizeof(s.s) - i, "__");
      break;
    case PointerT:
      i += snprintf(ss + i, sizeof(s.s) - i, "p");
      break;

    case ConstantWrapperT:
      return Type_special_cname(t->child);
    case UseT:
    case StructT:
    case CStructT:
    case InterfaceT:
    case UnionT:
    case EnumT:
    case CEnumT:
    case MacroT:
    case FnT:
    case PlaceHolder:
    case BaseT:
      i += snprintf(ss + i, sizeof(s.s) - i, "<>");
      break;
    }
    t = t->child;
  }
  i += snprintf(ss + i, sizeof(s.s) - i, "%s", t->name);
  return s;
}

bool c_type_declare_rec(FILE *f, Type *t, LocationRange l, const char *var, bool start) {
  if (!t || t == &Void)
    return false;

  if (t == &String && start) {
    fprintf(f, " char %s[]", var);
    return true;
  }

  bool hasVarWritten = false;
  Type *tt = t->child;
  if ((TypeKind)t->kind != SliceT && (TypeKind)t->kind != ConstantWrapperT) {
    while (tt && tt->kind == ArrayT)
      tt = tt->child;
    hasVarWritten = c_type_declare_rec(f, tt, l, var, false);
  }
  switch ((TypeKind)t->kind) {
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
  case SliceT:
    fprintf(f, "Slice_");
    break;

  case DynArrayT:
  case PointerT:
    fprintf(f, "*");
    break;
  case ConstantWrapperT:
    return c_type_declare_rec(f, t->child, l, var, true);
    break;
  case UseT:
    FATALR(l, "Can't use module '%s' as type!", Type_name(t).s);
    break;
  case CStructT:
  case CEnumT:
    fprintf(f, "%s", t->name);
    break;
  case StructT:
  case InterfaceT:
  case UnionT:
  case EnumT:
    fprintf(f, "%s%s", Type_defined_module(t)->c_name, t->name);
    break;
  case BaseT:
    fprintf(f, "%s", t->c_name);
    break;
  case MacroT:
    FATALR(l, "Cant handle macro '%s' as type!", t->macro_name);
    break;
  case FnT:
    if (c_type_declare_rec(f, t->fnT->d.returnType, l, "", true))
      FATALR(l, "I don't know right now how to handle array function pointer stuff!");
    fprintf(f, "(*())");
    break;
  case PlaceHolder:
    FATALR(l, "Unkown type '%s'!", t->name);
    break;
  }
  return hasVarWritten;
}

bool c_type_declare(FILE *f, Type *t, LocationRange l, const char *var) {
  return c_type_declare_rec(f, t, l, var, true);
}

void c_enum_entry_list(FILE *f, const char *module_name, EnumEntry *head) {
  for (EnumEntry *e = head; e; e = e->next) {
    fprintf(f, "%s%s", module_name, e->name);
    if (e->valueSet)
      fprintf(f, " = %d", e->value);
    if (e->next)
      fprintf(f, ",\n  ");
  }
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

void c_var_list(FILE *f, VariableList *v, const char *br) {
  for (int i = 0; i < v->len; ++i) {
    if (i > 0)
      fprintf(f, "%s", br);
    if (v->v[i].is_const)
      fprintf(f, "const ");
    if (!c_type_declare(f, v->v[i].type, v->v[i].location, v->v[i].name))
      fprintf(f, " %s", v->v[i].name);
  }
}

void c_struct(FILE *f, const char *module_name, const char *name, bool is_union, VariableList *vl) {
  fprintf(f, "typedef %s %s%s", (is_union ? "union" : "struct"), module_name, name);
  if (vl->len == 0) {
    fprintf(f, " {} %s%s;\n\n", module_name, name);
    return;
  }

  fprintf(f, " {\n  ");
  c_var_list(f, vl, ";\n  ");
  fprintf(f, "");
  fprintf(f, ";\n} %s%s;\n\n", module_name, name);
}

void c_fn_decl(FILE *f, const char *module_name, Function *fn, const char *fn_name);

void c_fn_pointer_decl(FILE *f, Type *tfn, bool named) {
  Function *fn = tfn->fnT;
  if (fn->d.returnType && fn->d.returnType != &Void) {
    if (c_type_declare(f, fn->d.returnType, fn->d.return_type_location, ""))
      FATALX("array return type not supported -> c backend!");
  } else
    fprintf(f, "void");
  fprintf(f, " (*%s)(", (named ? tfn->name : ""));
  for (int i = 0; i < fn->d.parameter.len; ++i) {
    Variable *v = &fn->d.parameter.v[i];
    if (i > 0) {
      fprintf(f, ", ");
      c_type_declare(f, v->type, v->location, "");
    } else
      fprintf(f, "void*");
  }
  if (fn->d.parameter.len == 0)
    fprintf(f, "void");
  fprintf(f, ")");
}

void c_interface_tables(FILE *f, const char *module_name, const char *name, Interface *intf) {
  fprintf(f, "typedef struct %s%sTable", module_name, name);
  fprintf(f, "{\n");
  for (int i = 0; i < intf->methods.len; ++i) {
    fprintf(f, "  ");
    Type *fnt = &intf->methods.fns[i];
    c_fn_pointer_decl(f, fnt, true);
    fprintf(f, ";\n");
  }
  fprintf(f, "} %s%sTable;\n\n", module_name, name);

  fprintf(f, "\ntypedef struct %s%s", module_name, name);
  fprintf(f, "{\n");
  fprintf(f, "  void *self;\n");
  fprintf(f, "  %s%sTable *tab;\n", module_name, name);
  fprintf(f, "} %s%s;\n\n", module_name, name);
}

Type *Module_find_member(Type *t, const char *name);

void c_interface(FILE *f, const char *module_name, const char *name, Interface *intf) {
  for (TypeList *tl = intf->used_types; tl; tl = tl->next) {
    Module *gotM = Type_defined_module(tl->type);
    if (!gotM)
      FATALX("could not find module for type '%s'", Type_name(tl->type).s);
    fprintf(f, "%s%sTable *%s%s_%s%s = &(%s%sTable){\n", module_name, name, module_name, name, gotM->c_name,
            tl->type->name, module_name, name);
    for (int i = 0; i < intf->methods.len; ++i) {
      const char *allName = intf->methods.fns[i].name;
      const char *fn_name = allName + strlen(name);
      Type *fnt = Module_find_member(tl->type, fn_name);
      if (!fnt || fnt->kind != FnT)
        FATALX("Type '%s' does not fit interface '%s'\n  missing method '%s'", Type_name(tl->type).s, name, fn_name);
      fprintf(f, "  (");
      c_fn_pointer_decl(f, fnt, false);
      fprintf(f, ")");
      fprintf(f, "&%s%s,\n", fnt->fnT->module->c_name, fnt->name);
    }
    fprintf(f, "};\n");
  }

  for (int i = 0; i < intf->methods.len; ++i) {
    Type *fnt = &intf->methods.fns[i];
    c_fn_decl(f, module_name, fnt->fnT, fnt->name);
    fprintf(f, " {\n  ");
    if (fnt->fnT->d.returnType && fnt->fnT->d.returnType != &Void)
      fprintf(f, "return ");

    fprintf(f, "%s->tab->%s(%s->self", fnt->fnT->d.parameter.v[0].name, fnt->name, fnt->fnT->d.parameter.v[0].name);
    for (int j = 1; j < fnt->fnT->d.parameter.len; ++j)
      fprintf(f, ", %s", fnt->fnT->d.parameter.v[j].name);
    fprintf(f, ");\n");
    fprintf(f, "}\n");
  }
}

void c_type(FILE *f, const char *module_name, Type *t) {
  switch ((TypeKind)t->kind) {
  case UseT:
    break;
  case CStructT:
    break;
  case BaseT:
    break;
  case UnionT:
  case StructT:
    c_struct(f, module_name, t->name, t->kind == UnionT, &t->structT->member);
    break;
  case EnumT:
  case CEnumT:
    break;
  case InterfaceT:
    break;
  case MacroT:
  case FnT:
    // todo!??
    break;
  case DynArrayT:
  case SliceT:
  case ArrayT:
  case PointerT:
  case ConstantWrapperT:
    break;
  case PlaceHolder:
    FATALX("Type declaration not implemented for that kind");
    break;
  }
}

void c_expression(FILE *f, Expression *e);

void c_parameter(FILE *f, ParameterList *pl, bool braces) {
  if (pl->len == 0)
    return;

  for (int i = 0; i < pl->len; ++i) {
    if (i > 0)
      fprintf(f, ", ");
    if (pl->p[i].name)
      fprintf(f, ".%s = ", pl->p[i].name);
    if (braces)
      fprintf(f, "(");
    c_expression(f, pl->p[i].p);
    if (braces)
      fprintf(f, ")");
  }
}

void jnq_expression(FILE *f, Expression *e) {
  if (!e)
    return;

  switch ((ExpressionType)e->type) {
  case BaseA:
    fprintf(f, "%s", e->baseconst->text);
    break;

  case IdentifierA:
    fprintf(f, "%s", e->id->name);
    break;

  case AutoTypeE:
    fprintf(f, "%s := ", e->autotype->name);
    jnq_expression(f, e->autotype->e);
    break;

  case BraceE:
    fprintf(f, "(");
    jnq_expression(f, e->brace->o);
    fprintf(f, ")");
    break;

  case CallE:
    jnq_expression(f, e->call->o);
    fprintf(f, "(");
    for (int i = 0; i < e->call->p.len; ++i) {
      if (i > 0)
        fprintf(f, ", ");
      jnq_expression(f, e->call->p.p[i].p);
    }
    fprintf(f, ")");
    break;
  case ConstructE:
    if (e->construct->pointer)
      fprintf(f, "&");
    fprintf(f, "%s{", e->construct->type->name);
    for (int i = 0; i < e->construct->p.len; ++i) {
      if (i > 0)
        fprintf(f, ", ");
      jnq_expression(f, e->construct->p.p[i].p);
    }
    fprintf(f, "}");
    break;
  case NewE:
    fprintf(f, " new ");
    jnq_expression(f, e->newE->o);
    break;
  case AccessE:
    jnq_expression(f, e->access->o);
    fprintf(f, "[");
    jnq_expression(f, e->access->p);
    fprintf(f, "]");
    break;
  case SliceE:
    jnq_expression(f, e->slice->o);
    fprintf(f, "[");
    jnq_expression(f, e->slice->begin);
    fprintf(f, ":");
    jnq_expression(f, e->slice->end);
    fprintf(f, "]");
    break;
  case MemberAccessE:
    jnq_expression(f, e->member->o);
    fprintf(f, ".%s", e->member->member->name);
    break;
  case AsCast:
    jnq_expression(f, e->cast->o);
    fprintf(f, " as %s", Type_name(e->cast->type).s);
    break;

  case UnaryPrefixE:
    fprintf(f, "(%s", e->unpre->op);
    jnq_expression(f, e->unpre->o);
    fprintf(f, ")");
    break;

  case UnaryPostfixE:
    jnq_expression(f, e->unpost->o);
    fprintf(f, "%s", e->unpost->op);
    break;

  case BinaryOperationE:
    jnq_expression(f, e->binop->o1);
    fprintf(f, " %s ", e->binop->op->op);
    jnq_expression(f, e->binop->o2);
    break;

  case TernaryOperationE:
    jnq_expression(f, e->ternop->condition);
    fprintf(f, " ? ");
    jnq_expression(f, e->ternop->if_e);
    fprintf(f, " : ");
    jnq_expression(f, e->ternop->else_e);
    break;

  case CDelegateE:
    if (e->cdelegate->o->type == BraceE)
      jnq_expression(f, e->cdelegate->o->brace->o);
    else
      jnq_expression(f, e->cdelegate->o);
    break;
  }
}

Type *c_expression_get_type(Module *m, Expression *e) {
  if (!e)
    return NULL;

  switch ((ExpressionType)e->type) {
  case BaseA:
    return e->baseconst->type;

  case IdentifierA: {
    if (!e->id->type)
      FATALR(e->range, "unknown type for '%s'", e->id->name);
    return e->id->type;
  }
  case AutoTypeE: {
    if (!e->autotype->type)
      FATALR(e->range, "unknown type for '%s'", e->autotype->name);
    return e->autotype->type;
  }
  case BraceE:
    return c_expression_get_type(m, e->brace->o);

  case MemberAccessE: {
    if (!e->member->member->type)
      FATALR(e->range, "unknown type for '%s'", e->member->member->name);
    return e->member->member->type;
  }

  case CallE: {
    Type *t = c_expression_get_type(m, e->call->o);
    if (!t || (t->kind != FnT && t->kind != MacroT))
      FATALR(e->range, "Need a function to be called!");
    if (t->kind == FnT)
      return t->fnT->d.returnType;
    if (t->kind == MacroT) {
      if (strcmp(t->macro_name, "len_s") == 0 || strcmp(t->macro_name, "offsetof") == 0 ||
          strcmp(t->macro_name, "cap_s") == 0 || strcmp(t->macro_name, "sizeof") == 0)
        return &u64;
      else if (strcmp(t->macro_name, "len") == 0 || strcmp(t->macro_name, "cap") == 0)
        return &i32;
      else if (strcmp(t->macro_name, "ASSERT") == 0)
        return &Bool;
      else if (strcmp(t->macro_name, "print") == 0)
        return &i32;
      else if (strcmp(t->macro_name, "resize") == 0 || strcmp(t->macro_name, "reserve") == 0)
        FATALR(e->range, "'%s' may be more complicated here!", t->macro_name);
      else
        FATALR(e->range, "Need a function to be called!");
    }
  }

  case ConstructE: {
    if (!e->construct->type)
      FATALR(e->range, "unknown construct type");
    return e->construct->type;
  }
  case AccessE: {
    Type *t = c_expression_get_type(m, e->access->o);
    if (t == &String)
      return &Char;
    if (!t->child)
      FATALR(e->range, "unknown access return type for '%s'", Type_name(t).s);
    return t->child;
  }
  case SliceE: {
    Type *t = c_expression_get_type(m, e->slice->o);
    if (t == &String) {
      Type *st = Module_find_slice_type(global_module(), &Char);
      if (!st)
        FATALR(e->range, "internal problem finding slice type for '%s'", Type_name(&Char).s);
      return st;
    }
    if (!t || !t->child)
      FATALR(e->range, "unknown slice return type for '%s'", Type_name(t).s);
    return t;
  }

  case AsCast:
    if (!e->cast->type)
      FATALR(e->range, "unknown access return type ");
    return e->cast->type;

  case NewE: {
    Type *st = c_expression_get_type(m, e->newE->o);
    Module *cm = Type_defined_module(st);
    if (!cm)
      FATALR(e->range, "internal problem finding module for type '%s'", Type_name(st).s);
    if ((TypeKind)st->kind == ArrayT) {
      Type *td = Module_find_dyn_array_type(cm, st->child);
      if (!td)
        FATALR(e->range, "internal problem finding dynamic array type for '%s'", Type_name(st).s);
      return td;
    }
    Type *td = Module_find_pointer_type(cm, st);
    if (!td)
      FATALR(e->range, "internal problem finding pointer type for '%s'", Type_name(st).s);
    return td;
  }

  case UnaryPrefixE: {
    Type *st = c_expression_get_type(m, e->unpost->o);
    if (strcmp(e->unpre->op, "&") == 0) {
      Module *cm = Type_defined_module(st);
      if (!cm)
        FATALX("internal problem finding module for type");
      Type *td = Module_find_pointer_type(cm, st);
      if (!td)
        FATALR(e->range, "internal problem finding pointer type for '%s'", Type_name(st).s);
      return td;
    } else if (strcmp(e->unpre->op, "*") == 0) {
      if (st->kind != PointerT)
        FATALR(e->range, "dereferenceing none pointer type '%s'!", Type_name(st).s);
      return st->child;
    } else if (strcmp(e->unpre->op, "!") == 0) {
      return &Bool;
    }
    return st;
  }

  case UnaryPostfixE:
    return c_expression_get_type(m, e->unpre->o);

  case BinaryOperationE: {
    Type *t1 = c_expression_get_type(m, e->binop->o1);
    Type *t2 = c_expression_get_type(m, e->binop->o2);
    if (!Type_convertable(t1, t2) && !Type_convertable(t2, t1)) {
      FATALR(e->range, "Expect equal types for binary operation '%s' (%s, %s) (%p, %p)", e->binop->op->op,
             Type_name(t1).s, Type_name(t2).s, t1, t2);
    }
    if (e->binop->op->returns_bool)
      return &Bool;
    // return  Type_more_common(t1, t2);
    return t1;
  }

  case TernaryOperationE: {
    Type *t1 = c_expression_get_type(m, e->ternop->if_e);
    Type *t2 = c_expression_get_type(m, e->ternop->else_e);
    if (!Type_equal(t1, t2))
      FATALR(e->range, "Expect equal types for ternary operation (%s, %s)", Type_name(t1).s, Type_name(t2).s);
    return t1;
  }

  case CDelegateE: {
    FATALR(e->range, "internal error: unexpected delegate");
    return c_expression_get_type(m, e->cdelegate->o);
  }
  }
  FATALR(e->range, "unknown type for expression!");
  return NULL;
}

bool c_check_macro(FILE *f, Call *ca, LocationRange l) {
  if (ca->o->type != IdentifierA)
    return false;

  if (strcmp(ca->o->id->name, "ASSERT") == 0) {
    if (ca->p.len != 1)
      FATALR(l, "'ASSERT' expects exact 1 parameter!");
    fprintf(f, "assert_imp_(\"%s\", %d, %d, ", l.file ? l.file : "", l.start_line, l.start_column);
    c_parameter(f, &ca->p, false);
    fprintf(f, ", \"'");
    jnq_expression(f, ca->p.p[0].p);
    fprintf(f, "'\")");
    return true;
  } else if (strcmp(ca->o->id->name, "print") == 0) {
    fprintf(f, "printf(\"");
    Type *param[128];
    int i = 0;
    for (; i < ca->p.len; ++i) {
      if (i >= 128)
        FATALR(ca->p.p[i].p->range, "Internal error to much parameter for 'print' macro.");
      param[i] = c_expression_get_type(NULL, ca->p.p[i].p);
      if (param[i] && param[i]->kind == ConstantWrapperT)
        param[i] = param[i]->child;
      if (param[i] == &String || param[i] == &Bool || (param[i]->kind == PointerT && param[i]->child == &Char))
        fprintf(f, "%%s");
      else if (param[i] == &Char)
        fprintf(f, "%%c");
      else if (param[i] == &i8)
        fprintf(f, "%%hhd");
      else if (param[i] == &i16)
        fprintf(f, "%%hd");
      else if (param[i] == &i32)
        fprintf(f, "%%d");
      else if (param[i] == &i64)
        fprintf(f, "%%ld");
      else if (param[i] == &u8)
        fprintf(f, "%%hhu");
      else if (param[i] == &u16)
        fprintf(f, "%%hu");
      else if (param[i] == &u32)
        fprintf(f, "%%u");
      else if (param[i] == &u64)
        fprintf(f, "%%lu");
      else if (param[i] == &f32)
        fprintf(f, "%%g");
      else if (param[i] == &f64)
        fprintf(f, "%%g");
      else if (param[i]->kind == SliceT && param[i]->child == &Char)
        fprintf(f, "%%s");
      else if (param[i] == &Any || param[i]->kind == PointerT || param[i]->kind == DynArrayT)
        fprintf(f, "%%p");
      else
        FATALR(ca->p.p[i].p->range, "Unhandled input type for 'print' macro '%s'", Type_name(param[i]).s);

      if (i + 1 < ca->p.len)
        fprintf(f, "\\t");
    }
    fprintf(f, "\\n\"");
    ParameterList *pl = &ca->p;
    for (int i = 0; i < pl->len; ++i) {
      fprintf(f, ", ");
      if (param[i] == &Bool) {
        fprintf(f, " ((");
        c_expression(f, pl->p[i].p);
        fprintf(f, " )?\"true\":\"false\")");
      } else if (param[i]->kind == SliceT && param[i]->child == &Char) {
        fprintf(f, " debug_tmp_string_(");
        c_expression(f, pl->p[i].p);
        fprintf(f, ")");
      } else
        c_expression(f, pl->p[i].p);
    }
    fprintf(f, ")");

    return true;
  } else if (strcmp(ca->o->id->name, "offsetof") == 0) {
    if (ca->p.len != 1)
      FATALR(l, "'offsetof' expects exact 1 parameter!");
    if (ca->p.p[0].p->type != MemberAccessE)
      FATALR(ca->p.p[0].p->range, "'offsetof' expects a member description!");
    MemberAccess *ma = ca->p.p[0].p->member;
    if (ma->o->type != IdentifierA)
      FATALR(ca->p.p[0].p->range, "'offsetof' expects a struct name!");
    Module *mam = Type_defined_module(ma->o_type);
    if (!mam)
      FATALR(ca->p.p[0].p->range, "Unknown type!");

    fprintf(f, "offsetof(%s%s, %s)", (ma->o_type->kind == CStructT ? "" : mam->c_name), ma->o->id->name,
            ma->member->name);
    return true;
  } else if (strcmp(ca->o->id->name, "resize") == 0 || strcmp(ca->o->id->name, "reserve") == 0 ||
             strcmp(ca->o->id->name, "push") == 0) {
    Type *arg_type = c_expression_get_type(NULL, ca->p.p[0].p);
    if (!arg_type || arg_type->kind != DynArrayT)
      FATALR(l, "Type '%s' not working as dynamic array!", Type_name(arg_type).s);
    fprintf(f, "__%s_ARRAY(", ca->o->id->name);
    if (c_type_declare(f, arg_type->child, l, "<<>>"))
      FATALR(l, "Type '%s' not working as dynamic array!", Type_name(arg_type).s);
    fprintf(f, ", ");
    c_parameter(f, &ca->p, true);
    fprintf(f, ")");
    return true;
  } else if (strcmp(ca->o->id->name, "pop") == 0 || strcmp(ca->o->id->name, "back") == 0 ||
             strcmp(ca->o->id->name, "clear") == 0) {
    Type *arg_type = c_expression_get_type(NULL, ca->p.p[0].p);
    if (!arg_type || arg_type->kind != DynArrayT)
      FATALR(l, "Type '%s' not working as dynamic array!", Type_name(arg_type).s);
    fprintf(f, "__%s_ARRAY(", ca->o->id->name);
    if (c_type_declare(f, arg_type->child, l, "<<>>"))
      FATALR(l, "Type '%s' not working as dynamic array!", Type_name(arg_type).s);
    fprintf(f, ", ");
    c_parameter(f, &ca->p, true);
    fprintf(f, ")");
    return true;
  } else if (strcmp(ca->o->id->name, "len_s") == 0 || strcmp(ca->o->id->name, "cap_s") == 0 ||
             strcmp(ca->o->id->name, "len") == 0 || strcmp(ca->o->id->name, "cap") == 0) {
    if (ca->p.len != 1)
      FATALR(l, "'%s' expects exact 1 parameter!", ca->o->id->name);
    Type *arg_type = c_expression_get_type(NULL, ca->p.p[0].p);

    if ((TypeKind)arg_type->kind == ConstantWrapperT)
      arg_type = arg_type->child;

    if (arg_type == &String) {
      if (strcmp(ca->o->id->name, "len") == 0 || strcmp(ca->o->id->name, "cap") == 0)
        fprintf(f, "(int32_t)");
      fprintf(f, "sizeof(");
      c_expression(f, ca->p.p[0].p);
      fprintf(f, ")");
      return true;
    }

    switch ((TypeKind)arg_type->kind) {
    case DynArrayT:
      if (strcmp(ca->o->id->name, "len") == 0 || strcmp(ca->o->id->name, "cap") == 0)
        fprintf(f, "(int32_t)");
      fprintf(f, "_%.3s_array((char*)", ca->o->id->name);
      c_expression(f, ca->p.p[0].p);
      fprintf(f, ")");
      return true;

    case ArrayT:
      fprintf(f, "%d", arg_type->array_count);
      return true;

    case SliceT:
      fprintf(f, "(");
      if (strcmp(ca->o->id->name, "len") == 0 || strcmp(ca->o->id->name, "cap") == 0)
        fprintf(f, "(int32_t)");
      c_expression(f, ca->p.p[0].p);
      fprintf(f, ".l)");
      return true;

    case UseT:
    case BaseT:
    case StructT:
    case CStructT:
    case UnionT:
    case EnumT:
    case CEnumT:
    case InterfaceT:
    case PointerT:
    case MacroT:
    case FnT:
    case ConstantWrapperT:
    case PlaceHolder:
      FATALR(ca->p.p[0].p->range, "Can't get 'len' for type '%s'!", Type_name(arg_type).s);
      break;
    }
  }

  return false;
}

void c_expression(FILE *f, Expression *e) {
  if (!e)
    return;

  switch ((ExpressionType)e->type) {
  case BaseA:
    if (e->baseconst->type == &String)
      fprintf(f, "\"%s\"", e->baseconst->text);
    else if (e->baseconst->type == &Char)
      fprintf(f, "'%s'", e->baseconst->text);
    else if (e->baseconst->type == &Null)
      fprintf(f, "NULL");
    else
      fprintf(f, "%s", e->baseconst->text);
    break;

  case IdentifierA:
    if (!e->id->type)
      FATALR(e->range, "unknown type for id '%s'", e->id->name);
    if (e->id->type->kind == FnT && !e->id->type->fnT->is_extern_c)
      fprintf(f, "%s", Type_defined_module(e->id->type)->c_name);
    else if (e->id->type->kind == ConstantWrapperT && e->id->type->constantModule)
      fprintf(f, "%s", e->id->type->constantModule->c_name);
    else if ((e->id->type->kind == StructT || e->id->type->kind == InterfaceT || e->id->type->kind == UnionT) &&
             e->id->type->name && strcmp(e->id->name, e->id->type->name) == 0)
      fprintf(f, "%s", Type_defined_module(e->id->type)->c_name);
    else if (e->id->type->kind == BaseT && e->id->type == Module_find_type(&global, c_sv(e->id->name))) {
      fprintf(f, "%s", e->id->type->c_name);
      break;
    }
    fprintf(f, "%s", e->id->name);
    break;

  case AutoTypeE: {
    if (!c_type_declare(f, e->autotype->type, e->range, e->autotype->name))
      fprintf(f, " %s", e->autotype->name);
    fprintf(f, " = ");
    c_expression(f, e->autotype->e);
    break;
  }

  case BraceE: {
    fprintf(f, "(");
    c_expression(f, e->brace->o);
    fprintf(f, ")");
    break;
  }

  case CallE: {
    if (c_check_macro(f, e->call, e->range))
      return;

    c_expression(f, e->call->o);
    fprintf(f, "(");
    c_parameter(f, &e->call->p, false);
    fprintf(f, ")");
    break;
  }

  case NewE: {
    if (e->newE->o->type != ConstructE)
      FATALR(e->range, "expect construction for 'new'");
    Type *t = e->newE->o->construct->type;
    if ((TypeKind)t->kind == ArrayT) {
      int count = t->array_count;
      t = t->child;
      fprintf(f, "__NEW_ARRAY(");
      if (c_type_declare(f, t, e->range, "<<>>"))
        FATALR(e->range, "Type '%s' not working as dynamic array!", Type_name(t).s);
      fprintf(f, ", %d)", count);
      Module_find_pointer_type(Type_defined_module(t), t);
    } else {
      fprintf(f, "__NEW_(%s%s, &(", Type_defined_module(t)->c_name, t->name);
      c_expression(f, e->newE->o);
      fprintf(f, "))");
      Module_find_pointer_type(Type_defined_module(t), t);
    }
    break;
  }

  case ConstructE: {
    if (e->construct->type->kind == InterfaceT) {
      if (e->construct->p.len == 0) {
        fprintf(f, "(%s%s){NULL, NULL}", Type_defined_module(e->construct->type)->c_name, e->construct->type->name);
      } else {
        if (e->construct->p.len != 2)
          FATALR(e->range, "Interface construction has wrong number of parameter!");
        if (e->construct->p.p[1].p->type != IdentifierA)
          FATALR(e->range, "Interface construction has missing table name!");
        if (e->construct->pointer)
          fprintf(f, "&");
        fprintf(f, "(%s%s){(void *)(", Type_defined_module(e->construct->type)->c_name, e->construct->type->name);
        c_expression(f, e->construct->p.p[0].p);
        fprintf(f, "), %s}", e->construct->p.p[1].p->id->name);
      }
    } else {
      if (e->construct->type->kind == StructT || e->construct->type->kind == UnionT)
        fprintf(f, "(%s%s){", Type_defined_module(e->construct->type)->c_name, e->construct->type->name);
      else if (e->construct->type->kind == CStructT)
        fprintf(f, "(%s){", e->construct->type->name);
      else if (e->construct->type->kind == ArrayT)
        fprintf(f, "{");
      else
        FATALR(e->range, "unexpect type for construction (or missing impementation)");
      c_parameter(f, &e->construct->p, false);
      fprintf(f, "}");
    }
    break;
  }

  case AccessE: {
    Type *type_to_access = c_expression_get_type(NULL, e->access->o);
    if (type_to_access && (TypeKind)type_to_access->kind == SliceT) {
      fprintf(f, "(*(");
      c_type_declare(f, type_to_access->child, e->range, ".:.");
      fprintf(f, "*)__slice_member(");
      c_expression(f, e->access->o);
      fprintf(f, ", sizeof(");
      c_type_declare(f, type_to_access->child, e->range, ".:.");
      fprintf(f, "), ");
      c_expression(f, e->access->p);
      fprintf(f, "))");

    } else {
      c_expression(f, e->access->o);
      fprintf(f, "[");
      c_expression(f, e->access->p);
      fprintf(f, "]");
    }
    break;
  }

  case SliceE: {
    Type *type_to_access = c_expression_get_type(NULL, e->slice->o);
    if (!type_to_access || (!type_to_access->child && type_to_access != &String))
      FATALR(e->range, "Error creating slice type");

    if (type_to_access->kind == DynArrayT) {
      fprintf(f, "__slice_from_dyn_array(");
    } else if (type_to_access->kind == ArrayT || type_to_access == &String) {
      fprintf(f, "__slice_from_array(");
    } else if (type_to_access->kind == SliceT) {
      fprintf(f, "__slice_from_slice(");
    } else if (type_to_access->kind == PointerT) {
      fprintf(f, "__slice_from_pointer(");
    } else
      FATALR(e->range, "Problem with slice creation for '%s'", Type_name(type_to_access).s);

    c_expression(f, e->slice->o);
    fprintf(f, ", sizeof(");
    if (type_to_access == &String)
      fprintf(f, "char");
    else
      c_type_declare(f, type_to_access->child, e->range, ".:.");
    fprintf(f, "), ");
    if ((TypeKind)type_to_access->kind == ArrayT || type_to_access == &String) {
      fprintf(f, "sizeof(");
      c_expression(f, e->slice->o);
      fprintf(f, "), ");
    }
    c_expression(f, e->slice->begin);
    fprintf(f, ", ");
    c_expression(f, e->slice->end);
    fprintf(f, ")");
    break;
  }

  case MemberAccessE: {
    if (!e->member->o_type)
      FATALR(e->range, "missing type for access");
    if (e->member->o_type->kind == EnumT || e->member->o_type->kind == CEnumT) {
      if (e->member->member->type->kind == EnumT)
        fprintf(f, "%s%s", Type_defined_module(e->member->member->type)->c_name, e->member->member->name);
      else if (e->member->member->type->kind == CEnumT)
        fprintf(f, "%s", e->member->member->name);
      else
        FATALR(e->range, "Missing implementation!");
      break;
    }
    if (!e->member->member->type)
      FATALR(e->range, "unknown type for member '%s'", e->member->member->name);
    TypeKind kk = (TypeKind)e->member->member->type->kind;
    if (kk == ConstantWrapperT)
      kk = (TypeKind)e->member->member->type->child->kind;
    switch (kk) {
    case ConstantWrapperT:
      FATALR(e->range, "Internal error for constant type '%s'", Type_name(e->member->member->type).s);
      break;
    case PlaceHolder:
      FATALR(e->range, "Use of unknow type '%s'", Type_name(e->member->member->type).s);
      break;
    case EnumT:
    case CEnumT:
    case UseT:
    case BaseT:
    case StructT:
    case CStructT:
    case InterfaceT:
    case UnionT:
    case PointerT:
    case DynArrayT:
    case ArrayT:
    case SliceT:
      c_expression(f, e->member->o);
      fprintf(f, "%s%s", (e->member->o_type->kind == PointerT ? "->" : "."), e->member->member->name);
      break;

      break;
    case MacroT:
    case FnT:
      FATALR(e->range, "internal error creating member function call!");
      break;
    }
    break;
  }

  case AsCast:
    fprintf(f, "((");
    if (c_type_declare(f, e->cast->type, e->range, ""))
      FATALR(e->range, "I don't know right now how to handle array cast  stuff!");
    fprintf(f, ")(");
    c_expression(f, e->cast->o);
    fprintf(f, "))");
    break;

  case UnaryPrefixE: {
    fprintf(f, "%s", e->unpre->op);
    c_expression(f, e->unpre->o);
    break;
  }

  case UnaryPostfixE: {
    c_expression(f, e->unpost->o);
    fprintf(f, "%s", e->unpost->op);
    break;
  }

  case BinaryOperationE: {
    c_expression(f, e->binop->o1);
    fprintf(f, " %s ", e->binop->op->op);
    c_expression(f, e->binop->o2);
    break;
  }

  case TernaryOperationE: {
    c_expression(f, e->ternop->condition);
    fprintf(f, " ? ");
    c_expression(f, e->ternop->if_e);
    fprintf(f, " : ");
    c_expression(f, e->ternop->else_e);
    break;
  }

  case CDelegateE: {
    c_expression(f, e->cdelegate->o);
    fprintf(f, "%s", e->cdelegate->delegate);
  }
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

void c_if_statement_base(FILE *f, Statement *s, int indent) {
  fprintf(f, "if (");
  c_expression(f, s->ifS->condition);
  fprintf(f, ") ");
  c_scope_as_body(f, s->ifS->ifBody, indent);
  if (s->ifS->elseBody) {
    fprintf(f, "%.*s", (s->ifS->ifBody->type == Scope ? 1 : indent), SPACE);
    fprintf(f, "else");
    c_scope_as_body(f, s->ifS->elseBody, indent);
    if (s->ifS->elseBody->type == Scope)
      fprintf(f, "\n");
  } else if (s->ifS->ifBody->type == Scope)
    fprintf(f, "\n");
}
void c_if_statement(FILE *f, Statement *s, int indent) {
  if (s->ifS->condition->type == AutoTypeE) {
    fprintf(f, "{\n");
    Expression *backup = s->ifS->condition;
    s->ifS->condition = &(Expression){
        .type = BraceE,
        .range = backup->range,
        .brace = &(Brace){
            .o = &(Expression){
                .binop = &(BinaryOperation){.o1 = &(Expression){.range = backup->range,
                                                                .type = IdentifierA,
                                                                .id = &(Identifier){.name = backup->autotype->name,
                                                                                    .type = backup->autotype->type}},
                                            .o2 = backup->autotype->e,
                                            .op = getop("=")},
                .type = BinaryOperationE,
                .range = backup->range,
            }}};

    fprintf(f, "%.*s", indent + 2, SPACE);
    if (!c_type_declare(f, backup->autotype->type, backup->range, backup->autotype->name))
      fprintf(f, " %s;", backup->autotype->name);
    c_if_statement_base(f, s, indent + 2);
    fprintf(f, "%.*s}", indent, SPACE);

    s->ifS->condition = backup;
  } else
    c_if_statement_base(f, s, indent);
}

void c_switch_statement_base(FILE *f, Statement *s, int indent) {
  fprintf(f, "switch (");
  c_expression(f, s->switchS->condition);
  fprintf(f, ") ");
  c_scope_as_body(f, s->doWhileS->body, indent);
}

void c_switch_statement(FILE *f, Statement *s, int indent) {
  if (s->switchS->condition->type == AutoTypeE) {
    fprintf(f, "{\n");
    Expression *backup = s->switchS->condition;
    s->switchS->condition = &(Expression){
        .type = BraceE,
        .range = backup->range,
        .brace = &(Brace){
            .o = &(Expression){
                .binop = &(BinaryOperation){.o1 = &(Expression){.range = backup->range,
                                                                .type = IdentifierA,
                                                                .id = &(Identifier){.name = backup->autotype->name,
                                                                                    .type = backup->autotype->type}},
                                            .o2 = backup->autotype->e,
                                            .op = getop("=")},
                .type = BinaryOperationE,
                .range = backup->range,
            }}};

    fprintf(f, "%.*s", indent + 2, SPACE);
    if (!c_type_declare(f, backup->autotype->type, backup->range, backup->autotype->name))
      fprintf(f, " %s;", backup->autotype->name);
    c_switch_statement_base(f, s, indent + 2);
    fprintf(f, "%.*s}", indent, SPACE);

    s->switchS->condition = backup;
  } else
    c_switch_statement_base(f, s, indent);
}

void c_statements(FILE *f, Statement *s, int indent) {
  if (!s)
    return;

  fprintf(f, "%.*s", indent, SPACE);
  switch ((StatementType)s->type) {
  case ExpressionS:
    c_expression(f, s->express->e);
    fprintf(f, ";\n");
    break;
  case Return:
    fprintf(f, "return ");
    c_expression(f, s->ret->e);
    fprintf(f, ";\n");
    break;
  case Delete: {
    Type *arg_type = c_expression_get_type(NULL, s->deleteS->e);
    switch ((TypeKind)arg_type->kind) {
    case DynArrayT:
      fprintf(f, "__FREE_ARRAY(");
      c_expression(f, s->deleteS->e);
      fprintf(f, ");");
      break;
    case PointerT:
      fprintf(f, "free(");
      c_expression(f, s->deleteS->e);
      fprintf(f, ");\n");
      break;
    case UseT:
    case BaseT:
    case StructT:
    case CStructT:
    case UnionT:
    case EnumT:
    case CEnumT:
    case InterfaceT:
    case ArrayT:
    case MacroT:
    case FnT:
    case ConstantWrapperT:
    case SliceT:
    case PlaceHolder:
      FATALR(s->deleteS->e->range, "Can't delete type '%s'!", Type_name(arg_type).s);
      break;
    }

    break;
  }

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
    c_if_statement(f, s, indent);
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
    fprintf(f, "while (");
    c_expression(f, s->whileS->condition);
    fprintf(f, ") ");
    c_scope_as_body(f, s->whileS->body, indent);
    if (s->whileS->body->type == Scope)
      fprintf(f, "\n");
    break;
  case DoWhile:
    fprintf(f, "do");
    c_scope_as_body(f, s->doWhileS->body, indent);
    fprintf(f, "%.*s", (s->doWhileS->body->type == Scope ? 1 : indent), SPACE);
    fprintf(f, "while (");
    c_expression(f, s->doWhileS->condition);
    fprintf(f, ");\n");
    break;
  case Switch:
    c_switch_statement(f, s, indent);
    break;
  }

  c_statements(f, s->next, indent);
}

void c_fn_decl(FILE *f, const char *module_name, Function *fn, const char *fn_name) {
  if (fn->d.returnType && fn->d.returnType != &Void) {
    if (c_type_declare(f, fn->d.returnType, fn->d.return_type_location, ""))
      FATALX("array return type not supported -> c backend!");
  } else
    fprintf(f, "void");
  fprintf(f, " %s%s(", module_name, fn_name);
  if (fn->d.parameter.len > 0)
    c_var_list(f, &fn->d.parameter, ", ");
  else
    fprintf(f, "void");
  fprintf(f, ")");
}

void c_fn(FILE *f, const char *module_name, Function *fn, const char *fn_name) {
  if (fn->is_extern_c)
    return;

  c_fn_decl(f, module_name, fn, fn_name);
  if (!fn->body)
    fprintf(f, " {}\n\n");
  else {
    fprintf(f, " {\n");
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
    for (TypeList *tl = m->types; tl; tl = tl->next)
      if (tl->type->kind == UseT)
        c_Module_types(f, tl->type->useT->module);
    fprintf(f, "\n");
    for (TypeList *tl = m->types; tl; tl = tl->next)
      c_type(f, m->c_name, tl->type);
  }
}

void c_Module_interface_tables(FILE *f, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  if (m->types) {
    for (TypeList *tl = m->types; tl; tl = tl->next)
      if (tl->type->kind == UseT)
        c_Module_interface_tables(f, tl->type->useT->module);

    for (TypeList *tl = m->types; tl; tl = tl->next)
      if (tl->type->kind == InterfaceT)
        c_interface_tables(f, m->c_name, tl->type->name, tl->type->interfaceT);
  }
}

void c_Module_interfaces(FILE *f, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  if (m->types) {
    for (TypeList *tl = m->types; tl; tl = tl->next)
      if (tl->type->kind == UseT)
        c_Module_interfaces(f, tl->type->useT->module);

    for (TypeList *tl = m->types; tl; tl = tl->next)
      if (tl->type->kind == InterfaceT)
        c_interface(f, m->c_name, tl->type->name, tl->type->interfaceT);
  }
}

void c_Module_constants(FILE *f, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  for (TypeList *tl = m->types; tl; tl = tl->next)
    if (tl->type->kind == UseT)
      c_Module_constants(f, tl->type->useT->module);

  for (ConstantList *cl = m->constants; cl; cl = cl->next) {
    if (cl->is_extern_c)
      continue;
    if ((ExpressionType)cl->autotype->type == AutoTypeE) {
      AutoTypeDeclaration *a = cl->autotype->autotype;
      char t_name[256];
      snprintf(t_name, sizeof(t_name), "%s%s", m->c_name, a->name);

      if (!c_type_declare(f, a->type, cl->autotype->range, t_name))
        fprintf(f, " %s", t_name);
      fprintf(f, " = ");
      c_expression(f, a->e);
    }
    fprintf(f, ";\n");
  }
}

void c_Module_fn(FILE *f, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  if (m->types) {
    fprintf(f, "\n");
    for (TypeList *tl = m->types; tl; tl = tl->next)
      if (tl->type->kind == UseT)
        c_Module_fn(f, tl->type->useT->module);
    fprintf(f, "\n");
    for (TypeList *tl = m->types; tl; tl = tl->next)
      if (tl->type->kind == FnT)
        c_fn(f, m->c_name, tl->type->fnT, tl->type->name);
  }
}

void c_fn_forward_fn(FILE *f, const char *module_name, const char *fn_name, Function *fn) {
  if (fn->d.returnType && fn->d.returnType != &Void) {
    if (c_type_declare(f, fn->d.returnType, fn->d.return_type_location, ""))
      FATALX("array return type not supported -> c backend!");
  } else
    fprintf(f, "void");
  fprintf(f, " %s%s(", module_name, fn_name);
  if (fn->d.parameter.len > 0)
    c_var_list(f, &fn->d.parameter, ", ");
  else
    fprintf(f, "void");
  fprintf(f, ");\n");
}

void c_type_forward(FILE *f, const char *module_name, Type *t) {
  switch ((TypeKind)t->kind) {
  case BaseT:
  case CStructT:
  case CEnumT:
    break;
  case StructT:
    fprintf(f, "typedef struct %s%s %s%s;\n", module_name, t->name, module_name, t->name);
    break;
  case UnionT:
    fprintf(f, "typedef union %s%s %s%s;\n", module_name, t->name, module_name, t->name);
    break;
  case EnumT:
    c_enum(f, module_name, t->name, t->enumT->entries);
    break;
  case InterfaceT: {
    fprintf(f, "typedef struct %s%sTable %s%sTable;\n", module_name, t->name, module_name, t->name);
    fprintf(f, "typedef struct %s%s %s%s;\n", module_name, t->name, module_name, t->name);
    break;
  }

  case ConstantWrapperT:
  case UseT:
    break;
  case PlaceHolder:
  case MacroT:
  case FnT:
    // todo?!?
    break;
  case ArrayT:
  case SliceT:
  case DynArrayT:
  case PointerT:
    break;
  }
}

void c_Module_forward_types(FILE *f, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  if (m->types) {
    fprintf(f, "\n");
    for (TypeList *tl = m->types; tl; tl = tl->next)
      if (tl->type->kind == UseT)
        c_Module_forward_types(f, tl->type->useT->module);
    fprintf(f, "\n");
    for (TypeList *t = m->types; t; t = t->next)
      c_type_forward(f, m->c_name, t->type);
  }
}

void c_fn_forward(FILE *f, const char *module_name, Type *t) {
  if (t->kind == FnT && !t->fnT->is_extern_c)
    c_fn_forward_fn(f, module_name, t->name, t->fnT);

  if (t->kind == InterfaceT) {
    for (int i = 0; i < t->interfaceT->methods.len; ++i)
      c_fn_forward_fn(f, module_name, t->interfaceT->methods.fns[i].name, t->interfaceT->methods.fns[i].fnT);
  }
}

void c_Module_forward_fn(FILE *f, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  if (m->types) {
    fprintf(f, "\n");
    for (TypeList *tl = m->types; tl; tl = tl->next)
      if (tl->type->kind == UseT)
        c_Module_forward_fn(f, tl->type->useT->module);
    fprintf(f, "\n");
    for (TypeList *tl = m->types; tl; tl = tl->next)
      c_fn_forward(f, m->c_name, tl->type);
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

bool is_member_fn_for(Type *ot, Type *ft, const char *name) {
  if (ft->kind != FnT)
    return false;
  Function *f = ft->fnT;

  if (f->d.parameter.len == 0)
    return false;

  const size_t ol = strlen(ot->name);
  const size_t nl = strlen(name);
  if (ol + nl == strlen(ft->name) && strncmp(ft->name, ot->name, ol) == 0) {
    if (strcmp(name, ft->name + ol) != 0)
      return false;
  } else if (strcmp(ft->name, name) != 0)
    return false;

  Variable *first = &f->d.parameter.v[0];
  if (Type_equal(first->type, ot))
    return true;

  if (first->type->kind == PointerT && Type_equal(first->type->child, ot))
    return true;

  return false;
}

Type *Module_find_member(Type *t, const char *name) {
  switch ((TypeKind)t->kind) {
  case ConstantWrapperT:
    return Module_find_member(t->child, name);
  case UnionT:
  case CStructT:
  case StructT: {
    for (Variable *v = t->structT->member.v; v < t->structT->member.v + t->structT->member.len; ++v)
      if (strcmp(v->name, name) == 0) {
        return v->type;
      }
    break;
  }
  case CEnumT:
  case EnumT: {
    for (EnumEntry *ee = t->enumT->entries; ee; ee = ee->next)
      if (strcmp(ee->name, name) == 0)
        return t;
    break;
  }
  case InterfaceT: {
    int offset = strlen(t->name);
    for (int i = 0; i < t->interfaceT->methods.len; ++i)
      if (strcmp(t->interfaceT->methods.fns[i].name + offset, name) == 0)
        return &t->interfaceT->methods.fns[i];
    break;
  }
  case UseT:
    FATALX("module name could not be used to access member '%s'!", Type_name(t).s);

  case BaseT:
  case ArrayT:
  case SliceT:
  case DynArrayT:
  case PointerT:
  case MacroT:
  case FnT:
  case PlaceHolder:
    FATALX("internal error, call of Module_find_member with wrong type");
    break;
  }
  Module *m = Type_defined_module(t);
  if (m) {
    for (TypeList *tl = m->types; tl; tl = tl->next) {
      if (is_member_fn_for(t, tl->type, name))
        return tl->type;
    }
  }

  return NULL;
}

void CheckInterface_for(Type *got, Type *expect, LocationRange l) {
  for (int i = 0; i < expect->interfaceT->methods.len; ++i) {
    const char *allName = expect->interfaceT->methods.fns[i].name;
    const char *name = allName + strlen(expect->name);
    Type *fnt = Module_find_member(got, name);
    if (!fnt || fnt->kind != FnT) {
      FATALR(l, "Type '%s' does not fit interface '%s'\n  missing method '%s'", Type_name(got).s, Type_name(expect).s,
             name);
    }
  }
}

Expression *Interface_construct(Program *p, Type *got, Type *expect, Expression *pr) {
  if (got == &Null) {
    Expression *cE = Program_new_Expression(p, ConstructE);
    cE->construct->type = expect;
    cE->construct->p = (ParameterList){NULL, 0};
    cE->range = pr->range;

    Expression *br = Program_new_Expression(p, BraceE);
    br->brace->o = cE;
    br->range = pr->range;
    return br;
  }

  bool is_pointer = got->kind == PointerT;
  if (!is_pointer) {
    fprintf(stderr, " %s <=> %s %p %p\n", Type_name(got).s, Type_name(expect).s, got, expect);
    FATALR(pr->range, "construct interface from none pointer type '%s'", Type_name(got).s);
  }

  if (is_pointer)
    got = got->child;
  CheckInterface_for(got, expect, pr->range);
  Expression *cE = Program_new_Expression(p, ConstructE);
  cE->range = pr->range;
  cE->construct->type = expect;
  cE->construct->p = (ParameterList){Program_alloc(p, sizeof(Parameter) * 2), 2};
  cE->construct->p.p[0].name = NULL;
  if (!is_pointer) {
    Expression *deref = Program_new_Expression(p, UnaryPrefixE);
    deref->range = pr->range;
    deref->unpre->o = pr;
    deref->unpre->op = "&";
    pr = deref;
  }
  cE->construct->p.p[0].p = pr;
  cE->construct->p.p[1].name = NULL;
  cE->construct->p.p[1].p = Program_new_Expression(p, IdentifierA);
  cE->construct->p.p[1].p->range = pr->range;
  Module *gotM = Type_defined_module(got);
  if (!gotM)
    FATALR(pr->range, "could not find module for type '%s'", Type_name(got).s);
  cE->construct->p.p[1].p->id->name =
      Program_copy_str(p, str("%s%s_%s%s", expect->interfaceT->module->c_name, expect->name, gotM->c_name, got->name));
  cE->construct->p.p[1].p->id->type = NULL;
  pr = cE;

  bool allready_used = false;
  for (TypeList *tl = expect->interfaceT->used_types; !allready_used && tl; tl = tl->next)
    allready_used = (tl->type == got);
  if (!allready_used) {
    TypeList *tl = (TypeList *)Program_alloc(p, sizeof(TypeList));
    tl->type = got;
    tl->next = expect->interfaceT->used_types;
    expect->interfaceT->used_types = tl;
  }

  Expression *br = Program_new_Expression(p, BraceE);
  br->range = pr->range;
  br->brace->o = pr;
  return br;
}

Type *AdaptParameter_for(Program *p, Type *got, Type *expect, Parameter *param) {
  if (expect->kind == InterfaceT && got != expect) {
    param->p = Interface_construct(p, got, expect, param->p);
    return expect;
  } else if (expect->kind == PointerT && expect->child->kind == InterfaceT && got != expect->child) {
    Type *x = AdaptParameter_for(p, got, expect->child, param);
    if (x != expect->child)
      FATALR(param->p->range, "internal error constructing interface '%s'", Type_name(expect).s);
    if (param->p->type != BraceE || param->p->brace->o->type != ConstructE)
      FATALR(param->p->range, "internal error constructing interface '%s'", Type_name(expect).s);
    param->p->brace->o->construct->pointer = true;
    Type *xP = Module_find_pointer_type(expect->child->interfaceT->module, expect->child);
    if (!xP) {
      xP = Program_add_type(p, PointerT, "", expect->child->interfaceT->module);
      xP->child = expect;
    }
    return xP;
  } else if (expect->kind == PointerT && expect->child == got) {
    Type *xP = Module_find_pointer_type(Type_defined_module(expect->child), expect->child);
    if (!xP) {
      xP = Program_add_type(p, PointerT, "", Type_defined_module(expect->child));
      xP->child = expect->child;
    }
    Expression *prefix = Program_new_Expression(p, UnaryPrefixE);
    prefix->range = param->p->range;
    prefix->unpre->op = "&";
    prefix->unpre->o = param->p;
    param->p = prefix;
    return xP;
  } else if (expect->kind == PointerT && got->kind == SliceT && expect->child == got->child) {
    Type *xP = Module_find_pointer_type(Type_defined_module(expect->child), expect->child);
    if (!xP) {
      xP = Program_add_type(p, PointerT, "", Type_defined_module(expect->child));
      xP->child = expect->child;
    }
    Expression *mem = Program_new_Expression(p, MemberAccessE);
    mem->range = param->p->range;
    mem->member->o = param->p;
    mem->member->o_type = &Any;
    mem->member->member = (Identifier *)Program_alloc(p, sizeof(Identifier));
    mem->member->member->name = "d";
    mem->member->member->type = &Any;
    Expression *cast = Program_new_Expression(p, AsCast);
    cast->range = param->p->range;
    cast->cast->o = mem;
    cast->cast->type = xP;
    param->p = cast;
    return xP;

  } else if (got->kind == PointerT && expect == got->child && expect != &Bool) {
    Expression *prefix = Program_new_Expression(p, UnaryPrefixE);
    prefix->range = param->p->range;
    prefix->unpre->op = "*";
    prefix->unpre->o = param->p;
    param->p = prefix;
    return expect;

  } else if ((expect->kind == SliceT && (got->kind == ArrayT || got->kind == DynArrayT) &&
              expect->child == got->child) ||
             (expect->kind == SliceT && expect->child == &Char && got == &String)) {
    Expression *sa = Program_new_Expression(p, SliceE);
    sa->range = param->p->range;
    sa->slice->o = param->p;
    sa->slice->begin = Program_new_Expression(p, BaseA);
    sa->slice->begin->range = param->p->range;
    sa->slice->begin->baseconst->text = "0";
    sa->slice->begin->baseconst->type = &i32;
    sa->slice->end = Program_new_Expression(p, BaseA);
    sa->slice->end->range = param->p->range;
    sa->slice->end->baseconst->text = got == &String ? "-1" : "0";
    sa->slice->end->baseconst->type = &i32;
    param->p = sa;
    return expect;

  } else if (got->kind == SliceT && expect == &Any) {
    Expression *mem = Program_new_Expression(p, MemberAccessE);
    mem->range = param->p->range;
    mem->member->o = param->p;
    mem->member->o_type = &Any;
    mem->member->member = (Identifier *)Program_alloc(p, sizeof(Identifier));
    mem->member->member->name = "d";
    mem->member->member->type = &Any;
    param->p = mem;
    return &Any;

  } else if (got->kind == InterfaceT && expect == &Bool) {
    Expression *mem = Program_new_Expression(p, MemberAccessE);
    mem->range = param->p->range;
    mem->member->o = param->p;
    mem->member->o_type = &Any;
    mem->member->member = (Identifier *)Program_alloc(p, sizeof(Identifier));
    mem->member->member->name = "self";
    mem->member->member->type = &Any;
    param->p = mem;
    return &Bool;
  }

  return got;
}

Type *c_Expression_make_variables_typed(VariableStack *s, Program *p, Module *m, Expression *e);

Type *c_Macro_make_variables_typed(VariableStack *s, Program *p, Module *m, const char *macro_name, Expression *e) {
  Type *param[32];
  int nb_param = 0;
  ParameterList pl = e->call->p;
  for (; nb_param < pl.len; ++nb_param) {
    if (nb_param >= 32)
      FATALR(e->range, "internal error: too much parameter for makro call");
    param[nb_param] = c_Expression_make_variables_typed(s, p, m, pl.p[nb_param].p);
  }

  if (strcmp("resize", macro_name) == 0 || strcmp("reserve", macro_name) == 0) {
    if (nb_param < 2)
      FATALR(e->range, "missing parameter for macro '%s'!", macro_name);
    if (nb_param > 2)
      FATALR(e->range, "too much parameter for macro '%s'!", macro_name);
    if (param[0]->kind != DynArrayT)
      FATALR(pl.p[0].p->range, "expect dynamic array for macro '%s' got '%s'!", macro_name, Type_name(param[0]).s);
    if (!Type_convertable(&u64, param[1]) && !Type_convertable(&i64, param[1]))
      FATALR(pl.p[1].p->range, "expect length unit for macro '%s'!", macro_name);
    return param[0];
  } else if (strcmp("push", macro_name) == 0) {
    if (nb_param < 2)
      FATALR(e->range, "missing parameter for macro '%s'!", macro_name);
    if (nb_param > 2)
      FATALR(e->range, "too much parameter for macro '%s'!", macro_name);
    if (param[0]->kind != DynArrayT)
      FATALR(pl.p[0].p->range, "expect dynamic array for macro '%s' got '%s'!", macro_name, Type_name(param[0]).s);
    param[1] = AdaptParameter_for(p, param[1], param[0]->child, &pl.p[1]);
    if (!Type_convertable(param[0]->child, param[1]))
      FATALR(pl.p[1].p->range, "can not '%s' type '%s'!", macro_name, Type_name(param[1]).s);
    if ((ExpressionType)pl.p[0].p->type == CallE)
      FATALR(pl.p[0].p->range, "'%s' does not work for r-values!", macro_name);
    return param[0];
  } else if (strcmp("pop", macro_name) == 0 || strcmp("back", macro_name) == 0) {
    if (nb_param < 1)
      FATALR(e->range, "missing parameter for macro '%s'!", macro_name);
    if (nb_param > 1)
      FATALR(e->range, "too much parameter for macro '%s'!", macro_name);
    if (param[0]->kind != DynArrayT)
      FATALR(pl.p[0].p->range, "expect dynamic array for macro '%s' got '%s'!", macro_name, Type_name(param[0]).s);
    return param[0]->child;
  } else if (strcmp("clear", macro_name) == 0) {
    if (nb_param < 1)
      FATALR(e->range, "missing parameter for macro '%s'!", macro_name);
    if (nb_param > 1)
      FATALR(e->range, "too much parameter for macro '%s'!", macro_name);
    if (param[0]->kind != DynArrayT)
      FATALR(pl.p[0].p->range, "expect dynamic array for macro '%s' got '%s'!", macro_name, Type_name(param[0]).s);
    return param[0];
  } else if (strcmp("sizeof", macro_name) == 0 || strcmp("offsetof", macro_name) == 0 ||
             strcmp("len_s", macro_name) == 0 || strcmp("cap_s", macro_name) == 0) {
    if (nb_param < 1)
      FATALR(e->range, "missing parameter for macro '%s'!", macro_name);
    if (nb_param > 1)
      FATALR(e->range, "too much parameter for macro '%s'!", macro_name);
    return &u64;
  } else if (strcmp("len", macro_name) == 0 || strcmp("cap", macro_name) == 0) {
    if (nb_param < 1)
      FATALR(e->range, "missing parameter for macro '%s'!", macro_name);
    if (nb_param > 1)
      FATALR(e->range, "too much parameter for macro '%s'!", macro_name);
    return &i32;
  } else if (strcmp("print", macro_name) == 0) {
    return &i32;
  } else if (strcmp("ASSERT", macro_name) == 0) {
    if (nb_param < 1)
      FATALR(e->range, "missing parameter for macro '%s'!", macro_name);
    if (nb_param > 1)
      FATALR(e->range, "too much parameter for macro '%s'!", macro_name);
    if (!param[0])
      FATALR(pl.p[0].p->range, "void type in macro '%s'!", macro_name);
    if (!Type_convertable(&Bool, param[0]))
      FATALR(pl.p[0].p->range, "expect boolean condition for macro '%s' got '%s'!", macro_name, Type_name(param[0]).s);
    param[0] = AdaptParameter_for(p, param[0], &Bool, &pl.p[0]);

    return &Bool;
  }

  FATALR(e->range, "macro '%s' not implemented!", macro_name);
  return NULL;
}

Type *c_Expression_make_variables_typed(VariableStack *s, Program *p, Module *m, Expression *e) {
  if (!e)
    return NULL;

  switch ((ExpressionType)e->type) {
  case BaseA:
    // if (e->baseconst->type == &String)
    //   FATALR(e->range, "need convert to Slice");
    return e->baseconst->type;

  case IdentifierA: {
    if ((e->id->type = VariableStack_find(s, e->id->name))) {
      if ((TypeKind)e->id->type->kind == ConstantWrapperT)
        return e->id->type->child;
      return e->id->type;
    }
    if ((e->id->type = Module_constant_type(m, c_sv(e->id->name))))
      return e->id->type->child;
    if ((e->id->type = Module_find_type(m, c_sv(e->id->name))))
      return e->id->type;
    FATALR(e->range, "unknown type for '%s'", e->id->name);
    return NULL;
  }
  case AutoTypeE: {
    e->autotype->type = c_Expression_make_variables_typed(s, p, m, e->autotype->e);
    VariableStack_push(s, e->autotype->name, e->autotype->type);
    return e->autotype->type;
  }
  case BraceE:
    return c_Expression_make_variables_typed(s, p, m, e->brace->o);

  case MemberAccessE: {
    Type *t = c_Expression_make_variables_typed(s, p, m, e->member->o);
    if (!t)
      FATALR(e->range, "member '%s' access to 'void' type", e->member->member->name);
    if ((TypeKind)t->kind == ConstantWrapperT)
      t = t->child;
    e->member->o_type = t;
    if (t->kind == PointerT)
      t = t->child;
    if (t->kind != StructT && t->kind != CStructT && t->kind != UnionT && t->kind != InterfaceT && t->kind != EnumT &&
        t->kind != CEnumT)
      FATALR(e->range, "Expect non pointer type for member access got '%s'", Type_name(t).s);
    if (!(e->member->member->type = Module_find_member(t, e->member->member->name)))
      FATALR(e->range, "unknown member '%s' for '%s'", e->member->member->name, Type_name(t).s);
    if (e->member->member->type->kind == FnT)
      e->member->member->name = e->member->member->type->name;
    return e->member->member->type;
  }
  case CallE: {
    Type *t = c_Expression_make_variables_typed(s, p, m, e->call->o);
    if (!t || (t->kind != FnT && t->kind != MacroT))
      FATALR(e->range, "Need a function to be called!");

    if (t->kind == MacroT)
      return c_Macro_make_variables_typed(s, p, m, t->macro_name, e);

    Type *first_param_type = NULL;
    if (e->call->o->type == MemberAccessE) {
      first_param_type = e->call->o->member->o_type;
      for (int i = e->call->p.len - 1; i >= 0; --i)
        e->call->p.p[i + 1] = e->call->p.p[i];
      e->call->p.len++;
      e->call->p.p[0].name = NULL;
      e->call->p.p[0].p = e->call->o->member->o;
      e->call->o->type = IdentifierA;
      e->call->o->id = e->call->o->member->member;
    }

    int p_len = e->call->p.len;
    if (p_len > t->fnT->d.parameter.len && t->fnT->d.parameter.v[t->fnT->d.parameter.len - 1].type != &Ellipsis)
      FATALR(e->range, "To much parameter for function call");
    if (p_len < t->fnT->d.parameter.len && t->fnT->d.parameter.v[t->fnT->d.parameter.len - 1].type != &Ellipsis)
      FATALR(e->range, "Missing parameter for function call");

    int i = 0;
    for (int j = 0; i < e->call->p.len && j < t->fnT->d.parameter.len; ++i, ++j) {
      Type *pt = j == 0 ? first_param_type : NULL;
      if (!pt)
        pt = c_Expression_make_variables_typed(s, p, m, e->call->p.p[i].p);
      if (!pt)
        FATALR(e->call->p.p[i].p->range, "Type missmatch got 'void', expect '%s'",
               Type_name(t->fnT->d.parameter.v[j].type).s);
      pt = AdaptParameter_for(p, pt, t->fnT->d.parameter.v[j].type, &e->call->p.p[i]);
      if (!Type_convertable(t->fnT->d.parameter.v[j].type, pt))
        FATALR(e->call->p.p[i].p->range, "Type missmatch got '%s', expect '%s'", Type_name(pt).s,
               Type_name(t->fnT->d.parameter.v[j].type).s);
    }
    for (; i < e->call->p.len; ++i)
      c_Expression_make_variables_typed(s, p, m, e->call->p.p[i].p);

    return t->fnT->d.returnType;
  }
  case ConstructE: {
    if (!e->construct->type) {
      Type *first = NULL;
      for (Parameter *pa = e->construct->p.p; pa < e->construct->p.p + e->construct->p.len; ++pa) {
        Type *pt = c_Expression_make_variables_typed(s, p, m, pa->p);
        if (first && !Type_equal(pt, first))
          FATALR(e->range, "Type missmatch for array element of '%s'!", Type_name(first).s);
        if (!first)
          first = pt;
      }
      if (e->construct->p.len == 0 || !first)
        FATALR(e->range, "empty array construction");

      Module *first_m = Type_defined_module(first);
      if (!first_m)
        FATALR(e->range, "unknown module for type '%s'", Type_name(first).s);
      e->construct->type = Module_find_array_type(first_m, e->construct->p.len, first);
      if (!e->construct->type) {
        e->construct->type = Program_add_type(p, ArrayT, "", first_m);
        e->construct->type->array_count = e->construct->p.len;
        e->construct->type->child = first;
      }
    }
    if (e->construct->type->kind != StructT && e->construct->type->kind != CStructT &&
        e->construct->type->kind != UnionT)
      for (int i = 0; i < e->construct->p.len; ++i) {
        Parameter *pa = &e->construct->p.p[i];
        if (pa->name)
          FATALR(pa->p->range, "Named construction '%s' for none struct type '%s'!", pa->name,
                 Type_name(e->construct->type).s);
      }
    if (e->construct->type->kind == ArrayT) {
      if (e->construct->type->array_count > 0 && e->construct->p.len > e->construct->type->array_count)
        FATALR(e->range, "Too many initializer for array of '%s'!", Type_name(e->construct->type).s);
      for (Parameter *pa = e->construct->p.p; pa < e->construct->p.p + e->construct->p.len; ++pa) {
        Type *pt = c_Expression_make_variables_typed(s, p, m, pa->p);
        Type *et = e->construct->type->child;
        if (!Type_equal(pt, et))
          FATALR(e->range, "Type missmatch for array element of '%s'!", Type_name(e->construct->type->child).s);
      }
    } else if (e->construct->type->kind == StructT || e->construct->type->kind == CStructT) {
      for (int i = 0; i < e->construct->p.len; ++i) {
        Parameter *pa = &e->construct->p.p[i];
        Type *pt = c_Expression_make_variables_typed(s, p, m, pa->p);
        if (pa->name) {
          Type *vt = Module_find_member(e->construct->type, pa->name);
          if (!vt)
            FATALR(pa->p->range, "Type '%s' has no member '%s'!", Type_name(e->construct->type).s, pa->name);
          pt = AdaptParameter_for(p, pt, vt, &e->construct->p.p[i]);
          if (!Type_convertable(vt, pt))
            FATALR(pa->p->range, "Type missmatch for member '%s' of '%s'!\n  expect '%s', got '%s' ", pa->name,
                   Type_name(e->construct->type).s, Type_name(vt).s, Type_name(pt).s);
        } else {
          if (i >= e->construct->type->structT->member.len)
            FATALR(e->range, "Too many initializer for struct '%s'", Type_name(e->construct->type).s);
          Variable *ma = &e->construct->type->structT->member.v[i];
          pt = AdaptParameter_for(p, pt, ma->type, &e->construct->p.p[i]);
          if (!Type_convertable(ma->type, pt))
            FATALR(pa->p->range, "Type missmatch for member '%s' of '%s'!\n  expect '%s', got '%s' ", ma->name,
                   Type_name(e->construct->type).s, Type_name(ma->type).s, Type_name(pt).s);
        }
      }
    } else if (e->construct->type->kind == InterfaceT) {
      if (e->construct->p.len == 0)
        ; // null construct
      else if (e->construct->p.len != 2)
        FATALR(e->range, "interfaces could only be constructed with corresponding struct");
    } else if (e->construct->type->kind == UnionT) {
      if (e->construct->p.len > 1)
        FATALR(e->range, "Too many initializer for union '%s'!", Type_name(e->construct->type).s);
      if (e->construct->p.len > 0) {
        Parameter *pa = &e->construct->p.p[0];
        Type *pt = c_Expression_make_variables_typed(s, p, m, pa->p);
        if (pa->name) {
          Type *vt = Module_find_member(e->construct->type, pa->name);
          if (!vt)
            FATALR(pa->p->range, "Type '%s' has no member '%s'!", Type_name(e->construct->type).s, pa->name);
          if (!Type_equal(pt, vt))
            FATALR(pa->p->range, "Type missmatch for member '%s' of '%s'!\n  expect '%s', got '%s' ", pa->name,
                   Type_name(e->construct->type).s, Type_name(vt).s, Type_name(pt).s);
        } else {
          bool any_type_fit = false;
          for (int i = 0; !any_type_fit && i < e->construct->type->structT->member.len; ++i) {
            Variable *v = &e->construct->type->structT->member.v[i];
            any_type_fit = Type_equal(pt, v->type);
            if (any_type_fit)
              pa->name = v->name;
          }
          if (!any_type_fit)
            FATALR(pa->p->range, "Type missmatch for union '%s'!\n  '%s' is not a valid member type",
                   Type_name(e->construct->type).s, Type_name(pt).s);
        }
      }
    } else {
      FATALR(e->range, "construction not possible (or not implemented) for '%s'", Type_name(e->construct->type).s);
    }
    return e->construct->type;
  }
  case AccessE: {
    Type *subt = c_Expression_make_variables_typed(s, p, m, e->access->p);
    if (!Type_convertable(&u64, subt) && !Type_convertable(&i64, subt))
      FATALR(e->access->p->range, "Expect integral type for array subscription, got '%s'", Type_name(subt).s);
    Type *t = c_Expression_make_variables_typed(s, p, m, e->access->o);
    if (t == &String)
      return &Char;
    if ((TypeKind)t->kind != ArrayT && t->kind != DynArrayT && t->kind != PointerT && t->kind != SliceT)
      FATALR(e->range, "Expect array/pointer type for access got '%s'", Type_name(t).s);
    return t->child;
  }
  case SliceE: {
    Type *begT = c_Expression_make_variables_typed(s, p, m, e->slice->begin);
    if (!Type_convertable(&u64, begT) && !Type_convertable(&i64, begT))
      FATALR(e->slice->begin->range, "Expect integral type for slice subscription, got '%s'", Type_name(begT).s);
    Type *endT = c_Expression_make_variables_typed(s, p, m, e->slice->end);
    if (!Type_convertable(&u64, endT) && !Type_convertable(&i64, endT))
      FATALR(e->slice->end->range, "Expect integral type for slice subscription, got '%s'", Type_name(endT).s);
    Type *t = c_Expression_make_variables_typed(s, p, m, e->slice->o);
    if ((TypeKind)t->kind != ArrayT && t->kind != DynArrayT && t->kind != PointerT && t->kind != SliceT && t != &String)
      FATALR(e->range, "Expect array/pointer type for slice creation got '%s'", Type_name(t).s);
    Type *child = t == &String ? &Char : t->child;
    Module *cm = Type_defined_module(child);
    Type *st = Module_find_slice_type(cm, child);
    if (!st) {
      st = Program_add_type(p, SliceT, "", cm);
      st->child = child;
    }
    return st;
  }
  case AsCast:
    c_Expression_make_variables_typed(s, p, m, e->cast->o);
    // check if cast is valid!
    return e->cast->type;
  case NewE: {
    Type *st = c_Expression_make_variables_typed(s, p, m, e->newE->o);
    Module *cm = Type_defined_module(st);
    if (!cm)
      FATALX("internal problem finding module for type");
    if ((TypeKind)st->kind == ArrayT) {
      Type *td = Module_find_dyn_array_type(cm, st->child);
      if (!td) {
        td = Program_add_type(p, DynArrayT, "", cm);
        td->child = st->child;
      }
      return td;
    }
    Type *td = Module_find_pointer_type(cm, st);
    if (!td) {
      td = Program_add_type(p, PointerT, "", cm);
      td->child = st;
    }
    return td;
  }
  case UnaryPrefixE: {
    Type *st = c_Expression_make_variables_typed(s, p, m, e->unpost->o);
    if (strcmp(e->unpre->op, "&") == 0) {
      Module *cm = Type_defined_module(st);
      if (!cm)
        FATALX("internal problem finding module for type");
      Type *td = Module_find_pointer_type(cm, st);
      if (!td) {
        td = Program_add_type(p, PointerT, "", cm);
        td->child = st;
      }
      return td;
    } else if (strcmp(e->unpre->op, "*") == 0) {
      if (st->kind != PointerT)
        FATALR(e->range, "dereferenceing none pointer type '%s'!", Type_name(st).s);
      return st->child;
    } else if (strcmp(e->unpre->op, "!") == 0) {
      if (!Type_convertable(&Bool, st))
        FATALR(e->range, "wrong type for '!' operator '%s'", Type_name(st).s);
      if (st->kind == InterfaceT) {
        Expression *cd = Program_new_Expression(p, CDelegateE);
        cd->range = e->unpre->o->range;
        Expression *br = Program_new_Expression(p, BraceE);
        br->range = e->unpre->o->range;
        br->brace->o = e->unpre->o;
        cd->cdelegate->o = br;
        cd->cdelegate->delegate = ".self";
        e->unpre->o = cd;
      }
      return &Bool;
    }
    return st;
  }
  case UnaryPostfixE:
    return c_Expression_make_variables_typed(s, p, m, e->unpre->o);
  case BinaryOperationE: {
    Type *t1 = c_Expression_make_variables_typed(s, p, m, e->binop->o1);
    if (!t1)
      FATALR(e->range, "void left side in binary expression '%s'", e->binop->op->op);
    Type *t2 = c_Expression_make_variables_typed(s, p, m, e->binop->o2);
    if (!t2)
      FATALR(e->range, "void right side in binary expression '%s'", e->binop->op->op);
    if (t1->kind == InterfaceT && (strcmp(e->binop->op->op, "==") == 0 || strcmp(e->binop->op->op, "!=") == 0)) {
      Expression *cd = Program_new_Expression(p, CDelegateE);
      cd->range = e->binop->o1->range;
      Expression *br = Program_new_Expression(p, BraceE);
      br->range = e->binop->o1->range;
      br->brace->o = e->binop->o1;
      cd->cdelegate->o = br;
      cd->cdelegate->delegate = ".self";
      e->binop->o1 = cd;
      // return &Bool;
    }
    if (t2->kind == InterfaceT && (strcmp(e->binop->op->op, "==") == 0 || strcmp(e->binop->op->op, "!=") == 0)) {
      Expression *cd = Program_new_Expression(p, CDelegateE);
      cd->range = e->binop->o2->range;
      Expression *br = Program_new_Expression(p, BraceE);
      br->range = e->binop->o2->range;
      br->brace->o = e->binop->o2;
      cd->cdelegate->o = br;
      cd->cdelegate->delegate = ".self";
      e->binop->o2 = cd;
      // return &Bool;
    }
    if (t1 != t2 && t1->kind == InterfaceT && strcmp(e->binop->op->op, "=") == 0) {
      e->binop->o2 = Interface_construct(p, t2, t1, e->binop->o2);
      return t1;
    }
    if (t1 != t2 && t2->kind == InterfaceT && t1 == &Bool && strcmp(e->binop->op->op, "=") == 0) {
      Expression *cd = Program_new_Expression(p, CDelegateE);
      cd->range = e->binop->o2->range;
      Expression *br = Program_new_Expression(p, BraceE);
      br->range = e->binop->o2->range;
      br->brace->o = e->binop->o2;
      cd->cdelegate->o = br;
      cd->cdelegate->delegate = ".self";
      e->binop->o2 = cd;
      return t1;
    }
    if (!Type_convertable(t1, t2) && !Type_convertable(t2, t1)) {
      FATALR(e->range, "Expect equal types for binary operation '%s' (%s, %s) (%p, %p)", e->binop->op->op,
             Type_name(t1).s, Type_name(t2).s, t1, t2);
    }
    if (e->binop->op->returns_bool)
      return &Bool;
    return t1;
  }
  case TernaryOperationE: {
    Type *tc = c_Expression_make_variables_typed(s, p, m, e->ternop->condition);
    if (tc != &Bool && tc != &Null && tc->kind != PointerT && tc->kind != InterfaceT)
      FATALR(e->range, "Expect 'bool' or pointer as contition got '%s'", Type_name(tc).s);
    if (tc->kind == InterfaceT) {
      Expression *cd = Program_new_Expression(p, CDelegateE);
      cd->range = e->range;
      cd->cdelegate->o = e->ternop->condition;
      cd->cdelegate->delegate = ".self";
      e->ternop->condition = cd;
    }
    Type *t1 = c_Expression_make_variables_typed(s, p, m, e->ternop->if_e);
    Type *t2 = c_Expression_make_variables_typed(s, p, m, e->ternop->else_e);
    if (!Type_equal(t1, t2))
      FATALR(e->range, "Expect equal types for ternary operation (%s, %s)", Type_name(t1).s, Type_name(t2).s);
    return t1;
  }
  case CDelegateE: {
    FATALX("nooo");
    return c_Expression_make_variables_typed(s, p, m, e->cdelegate->o);
  }
  }
  FATALR(e->range, "unknown type for expression!");
  return NULL;
}

void c_Statement_make_variables_typed(VariableStack *s, Program *p, Module *m, Statement *st) {
  if (!st)
    return;

  switch ((StatementType)st->type) {
  case ExpressionS:
    c_Expression_make_variables_typed(s, p, m, st->express->e);
    break;
  case Return:
    if (st->ret->e)
      c_Expression_make_variables_typed(s, p, m, st->ret->e);
    break;
  case Delete:
    c_Expression_make_variables_typed(s, p, m, st->deleteS->e);
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
  c_Statement_make_variables_typed(s, p, m, st->next);
}

void c_Function_make_variables_typed(VariableStack *s, Program *p, Module *m, Function *f) {
  int size = s->stackSize;
  for (Variable *p = f->d.parameter.v; p < f->d.parameter.v + f->d.parameter.len; ++p)
    VariableStack_push(s, p->name, p->type);

  c_Statement_make_variables_typed(s, p, m, f->body);

  s->stackSize = size;
}

void c_Module_make_variables_typed(Program *p, Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  for (TypeList *tl = m->types; tl; tl = tl->next)
    if (tl->type->kind == UseT)
      c_Module_make_variables_typed(p, tl->type->useT->module);

  VariableStack stack = (VariableStack){};
  for (ConstantList *cl = m->constants; cl; cl = cl->next) {
    Expression *e = cl->autotype;
    e->autotype->type = Program_add_type(p, ConstantWrapperT, "", m);
    if (cl->is_extern_c) {
      e->autotype->type->child = &i32;
      e->autotype->type->constantModule = NULL;
    } else
      e->autotype->type->child = c_Expression_make_variables_typed(&stack, p, m, e->autotype->e);
    if (!e->autotype->type->child)
      FATALX("internal problem finding type for constant '%s'", e->autotype->name);
    VariableStack_push(&stack, e->autotype->name, e->autotype->type);
  }
  for (TypeList *tl = m->types; tl; tl = tl->next)
    if (tl->type->kind == FnT)
      c_Function_make_variables_typed(&stack, p, m, tl->type->fnT);
}

void c_check_types(Module *m) {
  if (m->finished)
    return;
  m->finished = true;

  for (TypeList *tl = m->types; tl; tl = tl->next) {
    if (tl->type->kind == PlaceHolder)
      FATALX("undefined type '%s' in module '%s'", Type_name(tl->type).s, m->path);
    else if (tl->type->kind == UseT)
      c_check_types(tl->type->useT->module);
  }
  for (TypeList *tl = m->types; tl; tl = tl->next) {
    if (tl->type->kind != UseT)
      continue;

    for (Type **t = tl->type->useT->type; t < (tl->type->useT->type + tl->type->useT->type_len); ++t) {
      if ((*t)->kind == PlaceHolder) {
        LocationRange *r = &tl->type->useT->location;
        Location l = (Location){r->file, r->start_line, r->start_column};
        FATAL(&l, "include unkown type '%s' in module '%s'", Type_name(*t).s, tl->type->useT->module->path);
      }
    }
  }
}

void c_Program(FILE *f, Program *p, Module *m) {
  Program_reset_module_finished(p);
  c_check_types(&global);
  c_check_types(m);

  fputs("#include <ctype.h>\n", f);
  fputs("#include <stdint.h>\n", f);
  fputs("#include <stdarg.h>\n", f);
  fputs("#include <stdbool.h>\n", f);
  fputs("#include <stddef.h>\n", f);
  fputs("#include <stdio.h>\n", f);
  fputs("#include <stdlib.h>\n", f);
  fputs("#include <string.h>\n", f);
  fputs("#include <math.h>\n", f);
  fputs("\n", f);

  fputs("bool assert_imp_(const char *f, int l, int c, bool condition, const char *code) {\n", f);
  fputs("  if (!condition) {\n", f);
  fputs("    fprintf(stderr, \"%s:%d:%d: failed: %s\\n\", f, l, c, code);\n", f);
  fputs("    abort();\n", f);
  fputs("  }\n", f);
  fputs("  return !condition;\n", f);
  fputs("}\n", f);

  fputs("char *new_array_imp_(size_t nb, size_t st) {\n", f);
  fputs("  char *d = (char *)malloc(nb * st + 2 * sizeof(size_t));\n", f);
  fputs("  ((size_t *)d)[0] = nb;\n", f);
  fputs("  ((size_t *)d)[1] = nb;\n", f);
  fputs("  return (d + 2 * sizeof(size_t));\n", f);
  fputs("}\n", f);
  fputs("size_t _len_array(char *a) { return a!=NULL ? ((size_t *)(a - 2 * sizeof(size_t)))[0] : 0; }\n", f);
  fputs("size_t _cap_array(char *a) { return a!=NULL ? ((size_t *)(a - 2 * sizeof(size_t)))[1] : 0; }\n", f);
  fputs("char *resize_array_imp_(char *a, size_t nb, size_t st) {\n", f);
  fputs("  char *d = (char *)realloc((a - 2*sizeof(size_t)), nb * st + 2 * sizeof(size_t));\n", f);
  fputs("  ((size_t *)d)[0] = nb;\n", f);
  fputs("  ((size_t *)d)[1] = nb;\n", f);
  fputs("  return (d + 2 * sizeof(size_t));\n", f);
  fputs("}\n", f);
  fputs("char *reserve_array_imp_(char *a, size_t nb, size_t st) {\n", f);
  fputs("  if (_cap_array(a) >= nb)\n", f);
  fputs("    return a;\n", f);
  fputs("  char *d = (char *)realloc((a - 2*sizeof(size_t)), nb * st + 2 * sizeof(size_t));\n", f);
  fputs("  ((size_t *)d)[1] = nb;\n", f);
  fputs("  return (d + 2 * sizeof(size_t));\n", f);
  fputs("}\n", f);
  fputs("char *prepare_push_array_imp_(char *a, size_t st) {\n", f);
  fputs("  if (_cap_array(a) <= _len_array(a))\n", f);
  fputs("    a = reserve_array_imp_(a, _cap_array(a) + 512, st);\n", f);
  fputs("  char *d = (char *)(a - 2*sizeof(size_t));\n", f);
  fputs("  ((size_t *)d)[0]++;\n", f);
  fputs("  return (d + 2 * sizeof(size_t));\n", f);
  fputs("}\n", f);
  fputs("char *pop_array_imp_(char **a, size_t st) {\n", f);
  fputs("  if (_len_array(*a) == 0)\n", f);
  fputs("    return *a;\n", f);
  fputs("  char *d = (char *)((*a) - 2*sizeof(size_t));\n", f);
  fputs("  ((size_t *)d)[0]--;\n", f);
  fputs("  d = (d + 2 * sizeof(size_t));\n", f);
  fputs("  return &d[st * _len_array(d)];\n", f);
  fputs("}\n", f);

  fputs("typedef struct Slice_ {void *d; int64_t l;} Slice_; \n", f);
  fputs("void __slice_check_bounds(int64_t *b, int64_t *e, int64_t l) {\n", f);
  fputs("  *b = *b >= 0 ? *b : l + *b;\n", f);
  fputs("  *e = *e > 0 ? *e : l + *e;\n", f);
  fputs("  *e = *e >= l ? l : *e;\n", f);
  fputs("  *b = *b >= *e ? *e : *b;\n", f);
  fputs("  *b = *b < 0 ? 0 : *b;\n", f);
  fputs("}\n", f);
  fputs("Slice_ __slice_from_dyn_array(void *d, size_t st, int64_t b, int64_t e) {\n", f);
  fputs("  __slice_check_bounds(&b, &e, _len_array((char*)d));\n", f);
  fputs("  return (Slice_){d+st*b, e-b};\n", f);
  fputs("}\n", f);
  fputs("Slice_ __slice_from_array(void *d, size_t st, int64_t max, int64_t b, int64_t e) {\n", f);
  fputs("  __slice_check_bounds(&b, &e, (max/st));\n", f);
  fputs("  return (Slice_){d+st*b, e-b};\n", f);
  fputs("}\n", f);
  fputs("Slice_ __slice_from_pointer(void *d, size_t st, int64_t b, int64_t e) {\n", f);
  fputs("  e = e<0 ? 0 : e;\n", f);
  fputs("  __slice_check_bounds(&b, &e, e);\n", f);
  fputs("  return (Slice_){d+st*b, e-b};\n", f);
  fputs("}\n", f);
  fputs("Slice_ __slice_from_slice(Slice_ s, size_t st, int64_t b, int64_t e) {\n", f);
  fputs("  __slice_check_bounds(&b, &e, s.l);\n", f);
  fputs("  return (Slice_){s.d + st*b, e-b};\n", f);
  fputs("}\n", f);
  fputs("void* __slice_member(Slice_ s, size_t st, size_t o) {\n", f);
  fputs("  return (s.d + st * o);\n", f);
  fputs("}\n", f);
  fputs("char *debug_tmp_string_(Slice_ s) {\n", f);
  fputs("  static char b[512] = {0};\n", f);
  fputs("  snprintf(b, sizeof(b), \"%.*s\", (int)s.l, (const char*)s.d);\n", f);
  fputs("  return b;\n", f);
  fputs("}\n", f);

  fputs("#define __NEW_ARRAY(T, count) ((T *)new_array_imp_((count), sizeof(T)))\n", f);
  fputs("#define __resize_ARRAY(T, a, count) ((T *)resize_array_imp_((char*)a, (count), sizeof(T)))\n", f);
  fputs("#define __reserve_ARRAY(T, a, count) ((T *)reserve_array_imp_((char*)a, (count), sizeof(T)))\n", f);
  fputs("#define __clear_ARRAY(T, a) ((T *)resize_array_imp_((char*)a, 0, sizeof(T)))\n", f);
  fputs("#define __push_ARRAY(T, a, val) (a = (T *)prepare_push_array_imp_((char*)a, sizeof(T)), "
        "a[_len_array((char*)a)-1] = val, a)\n",
        f);
  fputs("#define __pop_ARRAY(T, a) *((T*)pop_array_imp_((char**)&a, sizeof(T)))\n", f);
  fputs("#define __back_ARRAY(T, a) (a[_len_array((char*)a)-1])\n", f);
  fputs("#define __FREE_ARRAY(a) (free(((char*)a) - 2 * sizeof(size_t)))\n", f);
  fputs("#define __NEW_(T, src) ((T *)memcpy(malloc(sizeof(T)), src, sizeof(T)))\n", f);

  for (CBlock *cb = p->cblocks; cb; cb = cb->next)
    if (cb->at_start)
      fputs(cb->block, f);

  Program_reset_module_finished(p);
  c_Module_make_variables_typed(p, &global);
  c_Module_make_variables_typed(p, m);

  Program_reset_module_finished(p);
  c_Module_forward_types(f, &global);
  c_Module_forward_types(f, m);

  Program_reset_module_finished(p);
  c_Module_interface_tables(f, &global);
  c_Module_interface_tables(f, m);

  Program_reset_module_finished(p);
  c_Module_types(f, &global);
  c_Module_types(f, m);

  Program_reset_module_finished(p);
  c_Module_forward_fn(f, &global);
  c_Module_forward_fn(f, m);

  Program_reset_module_finished(p);
  c_Module_interfaces(f, &global);
  c_Module_interfaces(f, m);

  Program_reset_module_finished(p);
  c_Module_constants(f, &global);
  c_Module_constants(f, m);

  Program_reset_module_finished(p);
  c_Module_fn(f, &global);
  c_Module_fn(f, m);

  bool custom_main = false;
  for (CBlock *cb = p->cblocks; cb; cb = cb->next)
    if (!cb->at_start) {
      fputs(cb->block, f);
      custom_main = true;
    }

  if (!custom_main) {

    int nb_main_args = 0;
    for (TypeList *tl = m->types; tl; tl = tl->next) {
      if (tl->type->kind != FnT || strcmp(tl->type->name, "main") != 0)
        continue;
      nb_main_args = tl->type->fnT->d.parameter.len;
    }

    fputs("\n", f);
    if (nb_main_args == 0) {
      fputs("int main() {\n", f);
      fprintf(f, "  return %smain();\n", m->c_name);
    } else if (nb_main_args == 2) {
      fputs("int main(int argc, char **argv) {\n", f);
      fprintf(f, "  return %smain(argc, argv);\n", m->c_name);
    } else {
      FATALX("incompatible main function");
    }
    fputs("}\n", f);
  }
}

#define fn_location ((Location){.file = "", .line = 1, .column = 3})

void Program_declare_macro(Program *p, const char *name) {
  Type *m = Program_add_type(p, MacroT, name, &global);
  m->macro_name = name;
}

void Program_add_defaults(Program *p) {
  Program_declare_macro(p, "ASSERT");
  Program_declare_macro(p, "len");
  Program_declare_macro(p, "len_s");
  Program_declare_macro(p, "cap");
  Program_declare_macro(p, "cap_s");
  Program_declare_macro(p, "offsetof");
  Program_declare_macro(p, "sizeof");
  Program_declare_macro(p, "resize");
  Program_declare_macro(p, "reserve");
  Program_declare_macro(p, "push");
  Program_declare_macro(p, "clear");
  Program_declare_macro(p, "pop");
  Program_declare_macro(p, "back");
  Program_declare_macro(p, "print");
}

#ifndef WIN32
extern ssize_t readlink(const char *, char *, size_t);
#endif

void init_lib_path() {
#ifdef WIN32
  const char sep = '\\';
  size_t len = GetModuleFileNameA(NULL, lib_path, sizeof(lib_path));
#else
  const char sep = '/';
  size_t len = readlink("/proc/self/exe", lib_path, sizeof(lib_path));
#endif

  // printf("%s\n", lib_path);
  if (len + 5 > sizeof(lib_path))
    FATALX("not enough memory for lib_path");

  char *last = NULL;
  for (char *c = lib_path; *c; ++c) {
    if (*c == sep) {
      *c = '.';
      last = c;
    }
  }
  if (last)
    last[1] = '\0';
  strcat(lib_path, "lib.");
}

void write_lsp_Location(FILE *f, LocationRange re, const char *uri) {

  char p[1024] = {0};
  if (uri[0] != '/') {
    getcwd(p, sizeof(p));
    strcat(p, "/");
  }
  strcat(p, uri);

  fprintf(f, "{");
  fprintf(f, "\"uri\":{\"$mid\":1,\"scheme\":\"file\",\"fsPath\":\"%s\",\"path\":\"%s\",\"external\":\"file://%s\"},",
          p, p, p);
  fprintf(f, "\"range\":{");
  fprintf(f, "\"start\":{");
  fprintf(f, "\"line\":%d,", re.start_line - 1);
  fprintf(f, "\"character\":%d},", re.start_column - 1);
  fprintf(f, "\"end\":{");
  fprintf(f, "\"line\":%d,", re.end_line - 1);
  fprintf(f, "\"character\":%d}}", re.end_column - 1);
  fprintf(f, "}");
}

// taken from vscode extension code
typedef enum Lsp_SymbolKind {
  Lsp_File = 0,
  Lsp_Module = 1,
  Lsp_Namespace = 2,
  Lsp_Package = 3,
  Lsp_Class = 4,
  Lsp_Method = 5,
  Lsp_Property = 6,
  Lsp_Field = 7,
  Lsp_Constructor = 8,
  Lsp_Enum = 9,
  Lsp_Interface = 10,
  Lsp_Function = 11,
  Lsp_Variable = 12,
  Lsp_Constant = 13,
  Lsp_String = 14,
  Lsp_Number = 15,
  Lsp_Boolean = 16,
  Lsp_Array = 17,
  Lsp_Object = 18,
  Lsp_Key = 19,
  Lsp_Null = 20,
  Lsp_EnumMember = 21,
  Lsp_Struct = 22,
  Lsp_Event = 23,
  Lsp_Operator = 24,
  Lsp_TypeParameter = 25
} Lsp_SymbolKind;

Lsp_SymbolKind Lsp_SymbolKind_for(TypeKind kind) {
  switch (kind) {
  case StructT:
  case CStructT:
  case UnionT:
    return Lsp_Struct;

  case MacroT:
  case FnT:
    return Lsp_Function;

  case EnumT:
  case CEnumT:
    return Lsp_Enum;

  case InterfaceT:
    return Lsp_Interface;

  case UseT:
    return Lsp_Package;

  case BaseT:
  case ArrayT:
  case DynArrayT:
  case SliceT:
  case PointerT:
  case PlaceHolder:
  case ConstantWrapperT:
    break;
  }

  return Lsp_Variable;
}

void write_lsp_SymbolInformation(FILE *f, const char *name, Lsp_SymbolKind k, const char *container, LocationRange ll) {
  fprintf(f, "{");
  fprintf(f, "\"name\":\"%s\",", name);
  if (container)
    fprintf(f, "\"containerName\":\"%s\",", container);
  fprintf(f, "\"kind\":%d,", k);
  fprintf(f, "\"location\":");
  write_lsp_Location(f, ll, ll.file);
  fprintf(f, "}");
}

void write_symbols(Module *m) {
  FILE *f = stdout;

  fprintf(f, "[\n");
  bool first = true;
  for (ConstantList *cl = m->constants; cl; cl = cl->next) {
    if ((ExpressionType)cl->autotype->type != AutoTypeE)
      continue;
    if (!first)
      fprintf(f, ",\n");
    first = false;
    write_lsp_SymbolInformation(f, cl->autotype->autotype->name, Lsp_Constant, NULL, cl->autotype->range);
  }
  for (TypeList *l = m->types; l; l = l->next) {
    LocationRange *ll = Type_location(l->type);
    if (!ll)
      continue;
    if (!first)
      fprintf(f, ",\n");
    first = false;

    write_lsp_SymbolInformation(f, l->type->name, Lsp_SymbolKind_for((TypeKind)l->type->kind), NULL, *ll);
    if ((TypeKind)l->type->kind == EnumT || (TypeKind)l->type->kind == CEnumT) {
      EnumEntry *ee = l->type->enumT->entries;
      while (ee) {
        fprintf(f, ",\n");
        write_lsp_SymbolInformation(f, ee->name, Lsp_EnumMember, l->type->name, NewRangeWord(ee->location, ee->name));
        ee = ee->next;
      }
    } else if ((TypeKind)l->type->kind == StructT || (TypeKind)l->type->kind == UnionT ||
               (TypeKind)l->type->kind == CStructT) {
      VariableList vl = l->type->structT->member;
      for (int i = 0; i < vl.len; ++i) {
        fprintf(f, ",\n");
        write_lsp_SymbolInformation(f, vl.v[i].name, Lsp_Property, l->type->name, vl.v[i].location);
      }
    } else if ((TypeKind)l->type->kind == InterfaceT) {
      FnVec fl = l->type->interfaceT->methods;
      for (int i = 0; i < fl.len; ++i) {
        fprintf(f, ",\n");
        write_lsp_SymbolInformation(f, fl.fns[i].name, Lsp_Method, l->type->name, *ll);
      }
    }
  }
  fprintf(f, "\n]\n");
}

#ifdef WIN32
#define STDIN_FILENO _fileno(stdin)
#define read(a, b, c) _read(a, b, c)
#endif

char *read_stdin() {
  size_t size = 0;
  size_t size_read = 1024;
  char *code = NULL;
  while (size_read == 1024) {
    code = (char *)realloc(code, size + 1024);
    size_read = read(STDIN_FILENO, code + size, 1024);
    size += 1024;
  }
  return code;
}

int symbols(Program *p, const char *file) {
  char *code = file ? readFile(file) : read_stdin();
  if (!code)
    return EXIT_FAILURE;
  State st = State_new(code, "dummy");
  Module *m = Program_add_module(p, "dummy");
  Program_parse_module(p, m, &st);
  write_symbols(m);
  free(code);

  return EXIT_SUCCESS;
}

typedef struct DeclarationVar {
  const char *name;
  Type *type;
  LocationRange range;
} DeclarationVar;

typedef struct DeclarationStack {
  DeclarationVar stack[256];
  int stackSize;
} DeclarationStack;

void DeclarationStack_push(DeclarationStack *s, const char *n, Type *t, LocationRange r) {
  // printf("push %s\n", n);
  if (s->stackSize >= 256)
    FATALX("Variable stack limit reached");
  s->stack[s->stackSize] = (DeclarationVar){n, t, r};
  s->stackSize++;
}

LocationRange DeclarationStack_find(DeclarationStack *s, const char *n) {
  for (int i = s->stackSize - 1; i >= 0; i--)
    if (strcmp(s->stack[i].name, n) == 0)
      return s->stack[i].range;
  return (LocationRange){};
}

Type *DeclarationStack_find_type(DeclarationStack *s, const char *n) {
  for (int i = s->stackSize - 1; i >= 0; i--)
    if (strcmp(s->stack[i].name, n) == 0)
      return s->stack[i].type;
  return NULL;
}

Type *Struct_member_fn_for(Type *t, const char *name) {
  if (!t || (t->kind != StructT && t->kind != UnionT && t->kind != CStructT))
    return NULL;

  Module *m = Type_defined_module(t);
  if (m) {
    for (TypeList *tl = m->types; tl; tl = tl->next) {
      if ((TypeKind)tl->type->kind != FnT)
        continue;
      if (tl->type->fnT->d.parameter.len < 1 || strcmp(tl->type->name, name) != 0)
        continue;
      if (tl->type->fnT->d.parameter.v[0].type == t ||
          (tl->type->fnT->d.parameter.v[0].type->kind == PointerT && tl->type->fnT->d.parameter.v[0].type->child == t))
        return tl->type;
    }
  }
  return NULL;
}

LocationRange declaration_member_location_of_type(Type *t, const char *name) {
  switch ((TypeKind)t->kind) {
  case StructT:
  case CStructT:
  case UnionT:
    for (int i = 0; i < t->structT->member.len; ++i) {
      if (strcmp(t->structT->member.v[i].name, name) == 0)
        return t->structT->member.v[i].location;
    }
    Type *method = Struct_member_fn_for(t, name);
    LocationRange *lr = Type_location(method);
    return lr ? *lr : (LocationRange){};

  case PointerT:
    return declaration_member_location_of_type(t->child, name);

  case EnumT:
  case CEnumT:
    for (EnumEntry *ee = t->enumT->entries; ee; ee = ee->next) {
      if (strcmp(ee->name, name) == 0)
        return NewRangeWord(ee->location, ee->name);
    }
    break;

  case UseT:
  case BaseT:
  case InterfaceT:
  case ArrayT:
  case DynArrayT:
  case SliceT:
  case FnT:
  case MacroT:
  case ConstantWrapperT:
  case PlaceHolder:
    break;
  }
  return (LocationRange){};
}

Type *declaration_type_of(Expression *e, DeclarationStack *ds) {
  switch ((ExpressionType)e->type) {
  case BaseA:
    return e->baseconst->type;

  case IdentifierA:
    return DeclarationStack_find_type(ds, e->id->name);

  case BraceE:
    return declaration_type_of(e->brace->o, ds);

  case AccessE: {
    Type *at = declaration_type_of(e->access->o, ds);
    return at ? at->child : NULL;
  }

  case SliceE:
    return declaration_type_of(e->access->o, ds);

  case CallE: {
    Type *ct = declaration_type_of(e->call->o, ds);
    return (ct && ct->kind == FnT) ? ct->fnT->d.returnType : NULL;
  }

  case AutoTypeE:
  case NewE:
  case AsCast:
  case UnaryPrefixE:
  case UnaryPostfixE:
  case BinaryOperationE:
  case TernaryOperationE:
  case CDelegateE:
    break;

  case ConstructE: {
    return e->construct->type;
    break;
  }

  case MemberAccessE: {
    Type *t_of = declaration_type_of(e->member->o, ds);
    if (t_of->kind == PointerT)
      t_of = t_of->child;
    if (t_of && (t_of->kind == StructT || t_of->kind == UnionT || t_of->kind == CStructT)) {
      for (int i = 0; i < t_of->structT->member.len; ++i)
        if (strcmp(t_of->structT->member.v[i].name, e->member->member->name) == 0)
          return t_of->structT->member.v[i].type;
      Type *method = Struct_member_fn_for(t_of, e->member->member->name);
      return method ? method : t_of;
    }
    break;
  }
  }
  return NULL;
}

LocationRange declaration_at_expression(Expression *e, DeclarationStack *ds, int line, int column) {
  if (!e)
    return (LocationRange){};

  switch ((ExpressionType)e->type) {
  case BaseA:
  case IdentifierA:
    if (inRange(e->range, line, column))
      return DeclarationStack_find(ds, e->id->name);
    break;

  case AutoTypeE: {
    if (inRange(NewRangeWord(RangeStart(e->range), e->autotype->name), line, column))
      return e->range;
    LocationRange re = declaration_at_expression(e->autotype->e, ds, line, column);
    Type *t_of = declaration_type_of(e->autotype->e, ds);
    DeclarationStack_push(ds, e->autotype->name, t_of, e->range);
    return re;
  }

  case BraceE:
    return declaration_at_expression(e->brace->o, ds, line, column);

  case CallE: {
    LocationRange re = declaration_at_expression(e->call->o, ds, line, column);
    if (RangeOk(re))
      return re;
    for (int i = 0; i < e->call->p.len; ++i) {
      if (RangeOk(re = declaration_at_expression(e->call->p.p[i].p, ds, line, column)))
        return re;
    }
    break;
  }

  case ConstructE: {
    if (e->construct->type && inRange(NewRangeWord(RangeStart(e->range), e->construct->type->name), line, column)) {
      LocationRange *lr = Type_location(e->construct->type);
      return lr ? *lr : (LocationRange){};
    }

    LocationRange re = (LocationRange){};
    for (int i = 0; i < e->construct->p.len; ++i) {
      if (RangeOk(re = declaration_at_expression(e->construct->p.p[i].p, ds, line, column)))
        return re;
    }
    break;
  }

  case NewE:
    return declaration_at_expression(e->newE->o, ds, line, column);

  case AccessE: {
    LocationRange re = declaration_at_expression(e->access->o, ds, line, column);
    if (RangeOk(re))
      return re;
    return declaration_at_expression(e->access->p, ds, line, column);
  }

  case SliceE: {
    LocationRange re = declaration_at_expression(e->slice->o, ds, line, column);
    if (RangeOk(re))
      return re;
    re = declaration_at_expression(e->slice->begin, ds, line, column);
    if (RangeOk(re))
      return re;
    return declaration_at_expression(e->slice->end, ds, line, column);
  }

  case MemberAccessE:
    if (inRange(e->range, line, column)) {
      Type *t_of = declaration_type_of(e->member->o, ds);
      if (t_of) {
        LocationRange lrm = declaration_member_location_of_type(t_of, e->member->member->name);
        if (RangeOk(lrm))
          return lrm;
        LocationRange *lr = Type_location(t_of);
        if (lr)
          return *lr;
      }
    }
    return declaration_at_expression(e->member->o, ds, line, column);

  case AsCast:
    return declaration_at_expression(e->cast->o, ds, line, column);

  case UnaryPrefixE:
    return declaration_at_expression(e->unpre->o, ds, line, column);

  case UnaryPostfixE:
    return declaration_at_expression(e->unpost->o, ds, line, column);

  case BinaryOperationE: {
    LocationRange re = declaration_at_expression(e->binop->o1, ds, line, column);
    if (RangeOk(re))
      return re;
    return declaration_at_expression(e->binop->o2, ds, line, column);
  }

  case TernaryOperationE: {
    LocationRange re = declaration_at_expression(e->ternop->condition, ds, line, column);
    if (RangeOk(re))
      return re;
    re = declaration_at_expression(e->ternop->if_e, ds, line, column);
    if (RangeOk(re))
      return re;
    return declaration_at_expression(e->ternop->else_e, ds, line, column);
  }

  case CDelegateE:
    return declaration_at_expression(e->cdelegate->o, ds, line, column);
  }

  return (LocationRange){};
}

LocationRange declaration_at_statement(Statement *s, DeclarationStack *ds, int line, int column);
LocationRange declaration_at_scope_like(Statement *s, DeclarationStack *ds, int line, int column) {
  int ds_state = ds->stackSize;
  LocationRange re = (LocationRange){};
  for (Statement *sub = s; sub; sub = sub->next) {
    if (RangeOk(re = declaration_at_statement(sub, ds, line, column)))
      break;
  }
  ds->stackSize = ds_state;
  return re;
}

LocationRange declaration_at_statement(Statement *s, DeclarationStack *ds, int line, int column) {
  switch ((StatementType)s->type) {

  case ExpressionS:
    return declaration_at_expression(s->express->e, ds, line, column);
  case Return:
    return declaration_at_expression(s->ret->e, ds, line, column);

  case Default:
  case Break:
  case Continue:
    break;

  case Case: {
    int ds_state = ds->stackSize;
    LocationRange re = declaration_at_expression(s->caseS->caseE, ds, line, column);
    if (!RangeOk(re))
      re = declaration_at_scope_like(s->caseS->body, ds, line, column);
    ds->stackSize = ds_state;
    return re;
  }

  case Delete:
    return declaration_at_expression(s->deleteS->e, ds, line, column);

  case Scope:
    return declaration_at_scope_like(s->scope->body, ds, line, column);

  case If: {
    int ds_state = ds->stackSize;
    LocationRange re = declaration_at_expression(s->ifS->condition, ds, line, column);
    if (!RangeOk(re)) {
      re = declaration_at_statement(s->ifS->ifBody, ds, line, column);
      if (!RangeOk(re) && s->ifS->elseBody)
        re = declaration_at_statement(s->ifS->elseBody, ds, line, column);
    }
    ds->stackSize = ds_state;
    return re;
  }

  case For: {
    int ds_state = ds->stackSize;
    LocationRange re = declaration_at_expression(s->forS->init, ds, line, column);
    if (!RangeOk(re)) {
      re = declaration_at_expression(s->forS->condition, ds, line, column);
      if (!RangeOk(re)) {
        re = declaration_at_expression(s->forS->incr, ds, line, column);
        if (!RangeOk(re))
          re = declaration_at_statement(s->forS->body, ds, line, column);
      }
    }
    ds->stackSize = ds_state;
    return re;
  }

  case While: {
    int ds_state = ds->stackSize;
    LocationRange re = declaration_at_expression(s->whileS->condition, ds, line, column);
    if (!RangeOk(re))
      re = declaration_at_statement(s->whileS->body, ds, line, column);
    ds->stackSize = ds_state;
    return re;
  }

  case DoWhile: {
    int ds_state = ds->stackSize;
    LocationRange re = declaration_at_expression(s->doWhileS->condition, ds, line, column);
    if (!RangeOk(re))
      re = declaration_at_statement(s->doWhileS->body, ds, line, column);
    ds->stackSize = ds_state;
    return re;
  }

  case Switch: {
    int ds_state = ds->stackSize;
    LocationRange re = declaration_at_expression(s->switchS->condition, ds, line, column);
    if (!RangeOk(re))
      re = declaration_at_statement(s->switchS->body, ds, line, column);
    ds->stackSize = ds_state;
    return re;
  }
  }

  return (LocationRange){};
}

int declaration(Program *p, const char *file, int line, int column, const char *uri) {
  char *code = file ? readFile(file) : read_stdin();
  if (!code)
    return EXIT_FAILURE;

  uri = !uri ? file : uri;

  State st = State_new(code, uri);

  char mod_path[512];
  strncpy(mod_path, uri, sizeof(mod_path));
  char *last = mod_path;
  for (char *c = mod_path; *c; ++c) {
    if (*c == '.')
      last = c;
    if (*c == '/' || *c == '\\')
      *c = '.';
  }
  *last = '\0';
  Module *m = Program_add_module(p, mod_path);
  Program_parse_module(p, m, &st);

  FILE *f = stdout;

  DeclarationStack ds = (DeclarationStack){};
  for (TypeList *l = m->types; l; l = l->next) {
    if ((TypeKind)l->type->kind == UseT) {
      if (l->type->useT->take_all) {
        for (TypeList *ll = l->type->useT->module->types; ll; ll = ll->next) {
          if (!Type_is_import_type(ll->type))
            continue;
          LocationRange *range = Type_location(ll->type);
          if (range)
            DeclarationStack_push(&ds, ll->type->name, ll->type, *range);
        }
      } else {
        for (int i = 0; i < l->type->useT->type_len; ++i) {
          LocationRange *range = Type_location(l->type->useT->type[i]);
          if (range)
            DeclarationStack_push(&ds, l->type->useT->type[i]->name, l->type->useT->type[i], *range);
        }
      }
    } else {
      LocationRange *range = Type_location(l->type);
      if (range)
        DeclarationStack_push(&ds, l->type->name, l->type, *range);
    }
  }
  for (ConstantList *l = m->constants; l; l = l->next)
    DeclarationStack_push(&ds, l->autotype->autotype->name, l->autotype->autotype->type, l->autotype->range);

  LocationRange re = (LocationRange){0};
  for (TypeList *l = m->types; l; l = l->next) {
    LocationRange *ll = Type_location(l->type);
    if (!ll || !inRange(*ll, line, column))
      continue;

    if ((TypeKind)l->type->kind != FnT)
      continue;

    // check paramteter location
    for (int i = 0; i < l->type->fnT->d.parameter.len; ++i) {
      Variable *p = &l->type->fnT->d.parameter.v[i];
      LocationRange p_re = NewRangeWord(RangeStart(p->location), p->name);
      if (inRange(p_re, line, column)) {
        re = p_re;
        goto done;
      }
      p_re = p->location;
      p_re.start_column += strlen(p->name) + 1;
      if (inRange(p_re, line, column)) {
        LocationRange *tre = Type_location(Type_remove_referening(p->type));
        if (tre)
          re = *tre;
        goto done;
      }
    }
    // check return type location
    if (l->type->fnT->d.returnType) {
      if (inRange(l->type->fnT->d.return_type_location, line, column)) {
        LocationRange *tre = Type_location(Type_remove_referening(l->type->fnT->d.returnType));
        if (tre)
          re = *tre;
        goto done;
      }
    }

    // push parameter location
    int ds_state = ds.stackSize;
    for (int i = 0; i < l->type->fnT->d.parameter.len; ++i) {
      Variable *p = &l->type->fnT->d.parameter.v[i];
      DeclarationStack_push(&ds, p->name, p->type, p->location);
    }

    // check fn body
    re = declaration_at_scope_like(l->type->fnT->body, &ds, line, column);
    if (RangeOk(re))
      goto done;

    ds.stackSize = ds_state;
  }

done:
  if (RangeOk(re)) {
    write_lsp_Location(f, re, re.file);
    fprintf(f, "\n");
  }

  free(code);
  return RangeOk(re) ? EXIT_SUCCESS : EXIT_FAILURE;
}

int references(Program *p, const char *file, int line, int column, const char *uri) {
  char *code = file ? readFile(file) : read_stdin();
  if (!code)
    return EXIT_FAILURE;

  uri = !uri ? file : uri;

  State st = State_new(code, "dummy");
  Module *m = Program_add_module(p, "dummy");
  Program_parse_module(p, m, &st);

  FILE *f = stdout;

  fprintf(f, "[");
  write_lsp_Location(f, (LocationRange){uri, line, column, line, column + 10}, uri);
  fprintf(f, ",");
  write_lsp_Location(f, (LocationRange){uri, line + 1, column, line + 1, column + 10}, uri);
  fprintf(f, "]\n");

  free(code);
  return EXIT_SUCCESS;
}

typedef struct CommandLineArgs {
  ProgramMode mode;
  const char *output;
  const char *main_file;
  const char *uri;
  int line, column;
} CommandLineArgs;

CommandLineArgs parse_command_line(int argc, char *argv[]) {
  if (argc <= 1)
    FATALX("missing command line arguments\n");

  CommandLineArgs args = (CommandLineArgs){Run, JNQ_BIN, NULL, NULL, 0, 0};
  int start = 2;
  if (strcmp(argv[1], "symbols") == 0)
    args.mode = Symbols;
  else if (strcmp(argv[1], "declaration") == 0)
    args.mode = Declaration;
  else if (strcmp(argv[1], "references") == 0)
    args.mode = References;
  else if (strcmp(argv[1], "build") == 0)
    args.mode = Build;
  else if (strcmp(argv[1], "transpile") == 0)
    args.mode = Transpile;
  else if (strcmp(argv[1], "run") == 0)
    args.mode = Run;
  else
    start = 1;

  int main_file = -1;
  for (int i = start; i < argc; ++i) {
    if (strcmp(argv[i], "--") == 0)
      break;
    if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
      args.output = argv[i + 1];
      i++;
    } else if (strcmp(argv[i], "--line") == 0 && i + 1 < argc) {
      args.line = atoi(argv[i + 1]);
      i++;
    } else if (strcmp(argv[i], "--column") == 0 && i + 1 < argc) {
      args.column = atoi(argv[i + 1]);
      i++;
    } else if (strcmp(argv[i], "--uri") == 0 && i + 1 < argc) {
      args.uri = argv[i + 1];
      i++;
    } else {
      int len = strlen(argv[i]);
      if (len > 4 && strcmp(argv[i] + (len - 4), ".jnq") == 0)
        main_file = i;
    }
  }

  if (main_file < 0 && args.mode != Symbols && args.mode != Declaration && args.mode != References)
    FATALX("missing input file\n");
  if (main_file >= 0)
    args.main_file = argv[main_file];

  return args;
}

Module *parse_main(Program *p) {
  int jnq_len = strlen(p->main_file);
  if (jnq_len < 4 || strcmp(p->main_file + (jnq_len - 4), ".jnq") != 0)
    FATALX("invalid input file '%s'\n", p->main_file);

  BuffString main_mod = (BuffString){};
  if (jnq_len > 255)
    FATALX("input path too long '%s' (sorry)\n", p->main_file);
  strncpy(main_mod.s, p->main_file, jnq_len - 4);

  return Program_parse_file(p, main_mod.s);
}

BuffString write_c_file(Program *p, Module *m) {
  int jnq_len = strlen(p->main_file);
  BuffString main_c = p->mode == Transpile ? str("%s.c", p->output) : str("%.*s_.c", jnq_len - 4, p->main_file);

  int error = 0;
  error = setjmp(long_jump_end);
  if (error == 0 && access(main_c.s, F_OK) == 0)
    FATALX("temp file already exisits '%s'", main_c.s);

  if (error == 0) {
    FILE *c_tmp_file = fopen(main_c.s, "w");
    if (!c_tmp_file)
      FATALX("could not create temp file '%s'", main_c);
    error = setjmp(long_jump_end);
    if (error == 0)
      c_Program(c_tmp_file, p, m);
    fclose(c_tmp_file);
    if (error != 0)
      remove(main_c.s);
  }
  if (error != 0)
    main_c.s[0] = '\0';

  return main_c;
}

int compile(Program *p, const char *main_c, int argc, char *argv[]) {
  int error = 0;
  error = setjmp(long_jump_end);
  if (error == 0 && access(p->output, F_OK) == 0)
    FATALX("temp bin already exisits '%s'", p->output);

  if (error == 0)
    error = setjmp(long_jump_end);
  if (error == 0) {
    char clang_call[1024] = {0};
    strcat(clang_call, "clang  -Werror -g "
#ifndef _WIN32
                       "-lm "
#else
                       "-D_CRT_SECURE_NO_WARNINGS "
#endif
    );
    const int start = 2; // p->main_file == argv[1] ? 2 : 3;
    bool output_defined = false;
    for (int i = start; i < argc; ++i) {
      if (strcmp(argv[i], "--") == 0)
        break;
      if (strcmp(argv[i], "-o") == 0)
        output_defined = true;

      int len = strlen(argv[i]);
      if (len > 4 && strcmp(argv[i] + len - 4, ".jnq") == 0)
        continue;

      strcat(clang_call, argv[i]);
      strcat(clang_call, " ");
    }
    if (!output_defined) {
      strcat(clang_call, "-o ");
      strcat(clang_call, p->output);
      strcat(clang_call, " ");
    }
    strcat(clang_call, main_c);
    error = system(clang_call);
    if (error != 0)
      FATALX("failed to compile c '%s'", main_c);
  }
  remove(main_c);
  return error;
}

int run(Program *p, const char *exec, int argc, char *argv[]) {
  char exec_call[1024] = {0};
  if (strlen(exec) < 2 || exec[0] != '.' || exec[1] != '/')
    strcat(exec_call, "./");
  strcat(exec_call, exec);
  strcat(exec_call, " ");
  bool add = false;
  for (int i = 1; i < argc; ++i) {
    if (add) {
      strcat(exec_call, argv[i]);
      strcat(exec_call, " ");
    } else
      add = strcmp(argv[i], "--") == 0;
  }

  int error = system(exec_call);
  remove(exec);
#ifdef _WIN32
  remove("jnq_bin.ilk");
  remove("jnq_bin.pdb");
#endif

  int percent = (int)(100.0 * (double)p->arena.len / (double)p->arena.cap);
  if (percent > 90) {
    printf("-------------------------------------\n");
    printf("       memory usage %3d%% (%zu/%zu)\n", percent, p->arena.len, p->arena.cap);
  }
  return error;
}

int main(int argc, char *argv[]) {
  char buffer[1024 * 1024 * 4];

  init_lib_path();

  CommandLineArgs args = parse_command_line(argc, argv);

  Program p = Program_new(buffer, sizeof(buffer));
  Program_add_defaults(&p);

  p.mode = args.mode;
  p.main_file = args.main_file;
  p.output = args.output;

  if (p.mode == Symbols)
    return symbols(&p, p.main_file);
  else if (p.mode == Declaration)
    return declaration(&p, p.main_file, args.line, args.column, args.uri);
  else if (p.mode == References)
    return references(&p, p.main_file, args.line, args.column, args.uri);

  Module *m = parse_main(&p);
  if (!m)
    FATALX("Invalid input file! '%s'", p.main_file);

  BuffString main_c = write_c_file(&p, m);

  int error = *main_c.s == '\0' ? 1 : 0;

  if (error == 0 && p.mode != Transpile)
    error = compile(&p, main_c.s, argc, argv);

  if (error == 0 && p.mode == Run)
    error = run(&p, p.output, argc, argv);

  return error == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
