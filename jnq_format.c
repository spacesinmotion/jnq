
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef _WIN32
#include <unistd.h>
#endif

typedef struct Location {
  const char *file;
  uint16_t line;
  uint16_t column;
} Location;

typedef struct State {
  const char *c;
  Location location;
} State;

State State_new(const char *c, const char *file) {
  return (State){.c = c, .location = (Location){.file = file, .line = 1, .column = 1}};
}

bool skip_whitespace_space(State *st, bool with_newline) {
  State old = *st;
  while (*st->c && isspace(*st->c) && (with_newline || *st->c != '\n')) {
    if (*st->c == '\n') {
      st->location.line++;
      st->location.column = 1;
    } else
      ++st->location.column;
    ++st->c;
  }
  return old.c > st->c;
}

void State_skip(State *st) {
  if (*st->c == '\n') {
    st->location.line++;
    st->location.column = 1;
  } else
    ++st->location.column;
  ++st->c;
}

void State_skip_n(State *s, int c) {
  for (int i = 0; i < c; ++i)
    State_skip(s);
}

int count_nl_in_whitespace_space(State st) {
  int c = 0;
  while (*st.c && isspace(*st.c)) {
    if (*st.c == '\n')
      c++;
    State_skip(&st);
  }
  return c;
}

bool check_identifier(State *st) {
  State old = *st;
  if (st->c[0] && (isalpha(st->c[0]) || st->c[0] == '_')) {
    while (st->c[0] && (isalnum(st->c[0]) || st->c[0] == '_'))
      State_skip(st);
  }
  if (old.c < st->c)
    return true;
  *st = old;
  return false;
}

bool check_word(State *st, const char *word) {
  State old = *st;
  while (st->c[0] && *word && *word == st->c[0]) {
    State_skip(st);
    ++word;
  }
  if (*word == 0 && !isalnum(st->c[0]) && st->c[0] != '_')
    return true;
  *st = old;
  return false;
}

bool check_word_s(State st, const char *word) {
  skip_whitespace_space(&st, true);
  while (st.c[0] && *word && *word == st.c[0]) {
    State_skip(&st);
    ++word;
  }
  return *word == 0 && !isalnum(st.c[0]) && st.c[0] != '_';
}

bool is_op(State *st, const char *op) {
  while (st->c[0] && op[0] && op[0] == st->c[0]) {
    State_skip(st);
    ++op;
  }
  return op[0] == 0;
}

bool check_op(State *st, const char *op) { return is_op(st, op); }

void FATAL(Location *l, const char *format, ...) {
  va_list args;
  fprintf(stderr, "%s:%hu:%hu: error: ", l->file, l->line, l->column);
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);
  exit(-1);
}

void FATALX(const char *format, ...) {
  va_list args;
  fprintf(stderr, "%s:%zu:%zu error: ", "unknown.jnq", (size_t)0, (size_t)0);
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);
  exit(-1);
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

char *readStdin() {
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

bool look_ahead(const char *test, const char *c, bool end_nonspace) {
  while (*c && *test) {
    if (*test != *c)
      return false;
    ++test;
    ++c;
  }

  return end_nonspace ? !isspace(*c) : isspace(*c);
}

typedef struct Formatter {
  bool check_only;
  bool json;
  bool first;
} Formatter;

void handle_ok(Formatter *f, State *old, State *st) {
  if (f->check_only)
    return;
  if (f->json) {
  } else
    fprintf(stdout, "%.*s", (int)(st->c - old->c), old->c);
}

void handle_replace(Formatter *f, State *old, State *st, const char *n) {
  if (f->check_only)
    return;
  (void)old;
  (void)st;
  if (f->json) {
    if (!f->first)
      fprintf(stdout, ",");
    fprintf(stdout, "{");
    fprintf(stdout, "\"type\":\"replace\",");
    fprintf(stdout, "\"text\":\"");
    for (; *n; ++n) {
      if (*n == '\n')
        fprintf(stdout, "\\n");
      else if (*n == '\t')
        fprintf(stdout, "\\t");
      else
        fprintf(stdout, "%c", *n);
    }
    fprintf(stdout, "\",");
    fprintf(stdout, "\"start\":{\"line\":%d,\"character\":%d},", old->location.line, old->location.column);
    fprintf(stdout, "\"end\":{\"line\":%d,\"character\":%d}", st->location.line, st->location.column);
    fprintf(stdout, "}\n");
    f->first = false;
  } else
    fprintf(stdout, "%s", n);
}

bool check_comment(Formatter *f, State *st) {
  State old = *st;
  skip_whitespace_space(st, true);
  if (strncmp(st->c, "//", 2) == 0) {
    while (strncmp(st->c, "//", 2) == 0) {
      while (*st->c != '\n' && *st->c != '\0')
        State_skip(st);
      State_skip(st);
    }
    handle_ok(f, &old, st);
    check_comment(f, st);
    return true;
  }
  *st = old;
  return false;
}

void expect_space(Formatter *f, State *st, const char *space) {
  State old = *st;
  skip_whitespace_space(st, true);
  int len = strlen(space);
  if (len == st->c - old.c && strncmp(old.c, space, len) == 0)
    handle_ok(f, &old, st);
  else
    handle_replace(f, &old, st, space);
}

void expect_l_space(Formatter *f, State *st, const char *space) {
  State old = *st;
  skip_whitespace_space(st, false);
  int len = strlen(space);
  if (len == st->c - old.c && strncmp(old.c, space, len) == 0)
    handle_ok(f, &old, st);
  else
    handle_replace(f, &old, st, space);
}

void expect_indent(Formatter *f, State *st, int ind) {
  check_comment(f, st);
  State old = *st;

  if (check_word_s(*st, "case") || check_word_s(*st, "default")) {
    ind -= 2;
    *st = old;
  }

  skip_whitespace_space(st, true);
  int len = st->c - old.c;
  if (len == ind + 1 && old.c[0] == '\n' && strncmp(old.c + 1, "                                        ", ind) == 0) {
    handle_ok(f, &old, st);
  } else if (len == ind + 2 && old.c[0] == '\n' && old.c[1] == '\n' &&
             strncmp(old.c + 2, "                                        ", ind) == 0) {
    handle_ok(f, &old, st);
  } else {
    int nl = 0;
    for (const char *c = old.c; c < st->c; ++c)
      if (*c == '\n')
        nl++;

    char s[] = "\n\n                                        ";
    s[ind + 2] = '\0';
    handle_replace(f, &old, st, nl > 1 ? s : (s + 1));
  }
}

void expect_indent_line(Formatter *f, State *st, int ind) {
  State old = *st;
  while (*st->c && isspace(*st->c) && *st->c != '\n')
    State_skip(st);
  if ((st->c - old.c) == ind && strncmp(old.c, "                                        ", ind) == 0) {
    handle_ok(f, &old, st);
  } else {
    char s[] = "                                              ";
    s[ind] = '\0';
    handle_replace(f, &old, st, s);
  }
}

bool expect_identifier(Formatter *f, State *st) {
  State old = *st;
  if (check_word(st, "for") || check_word(st, "while") || check_word(st, "do") || check_word(st, "if") ||
      !check_identifier(st)) {
    *st = old;
    return false;
  }
  handle_ok(f, &old, st);
  return true;
}

bool expect_word(Formatter *f, State *st, const char *word) {
  State old = *st;
  if (!check_word(st, word)) {
    *st = old;
    return false;
  }
  handle_ok(f, &old, st);
  return true;
}

bool expect_number(Formatter *f, State *st) {
  char *end;
  strtod(st->c, &end);
  if (end == st->c)
    return false;
  State old = *st;
  st->location.column += end - st->c;
  st->c = end;
  if (st->c[0] == 'f')
    State_skip(st);
  else if (st->c[0] == 'l' && st->c[1] == 'u')
    State_skip_n(st, 2);
  else if (st->c[0] == 'u' && st->c[1] == 'l')
    State_skip_n(st, 2);
  else if (st->c[0] == 'l')
    State_skip(st);
  else if (st->c[0] == 'u')
    State_skip(st);
  handle_ok(f, &old, st);
  return true;
}

bool expect_string(Formatter *f, State *st) {
  char t = st->c[0];
  if (t != '"' && t != '\'')
    return false;

  State old = *st;
  State_skip(st);
  bool slash_escaped = false;
  while (st->c[0] != t || (st->c[-1] == '\\' && !slash_escaped)) {
    if (!st->c[0])
      return true;
    slash_escaped = st->c[-1] == '\\';
    State_skip(st);
  }
  State_skip(st);
  handle_ok(f, &old, st);
  return true;
}

bool expect_op(Formatter *f, State *st, const char *op) {
  State old = *st;
  if (!check_op(st, op)) {
    *st = old;
    return false;
  }
  handle_ok(f, &old, st);
  return true;
}

bool expect_l_op(Formatter *f, State *st, const char *op) {
  State old = *st;
  if (!is_op(st, op)) {
    *st = old;
    return false;
  }
  handle_ok(f, &old, st);
  return true;
}

void format_use(Formatter *f, State *st) {
  expect_space(f, st, " ");

  if (!expect_identifier(f, st) && !expect_op(f, st, "*"))
    return;
  while (check_op(st, ",")) {
    expect_space(f, st, "");
    State old = *st;
    State_skip(st);
    handle_ok(f, &old, st);
    old = *st;
    expect_space(f, st, " ");

    if (!expect_identifier(f, st))
      return;
  }

  expect_space(f, st, " ");
  if (!expect_word(f, st, "from"))
    return;

  expect_space(f, st, " ");
  while (expect_identifier(f, st)) {
    if (!check_op(st, "."))
      break;
    expect_space(f, st, "");
    expect_op(f, st, ".");
    expect_space(f, st, "");
  }
}

bool expect_array_decl(Formatter *f, State *st) {
  State old = *st;

  if (expect_op(f, st, "[")) {
    expect_space(f, st, "");
    if (expect_number(f, st))
      expect_space(f, st, "");
    if (expect_op(f, st, "]"))
      return true;
  }

  *st = old;
  return false;
}

bool expect_type_decl(Formatter *f, State *st) {
  State old = *st;

  if (expect_word(f, st, "new")) {
    expect_space(f, st, " ");
    return expect_type_decl(f, st);
  }

  if (expect_word(f, st, "buf") || expect_word(f, st, "pool") || expect_word(f, st, "vec")) {
    expect_space(f, st, "");
    return expect_type_decl(f, st);
  }
  if (expect_op(f, st, "*")) {
    expect_space(f, st, "");
    return expect_type_decl(f, st);
  }
  if (expect_array_decl(f, st)) {
    expect_space(f, st, "");
    return expect_type_decl(f, st);
  }
  if (expect_identifier(f, st))
    return true;

  *st = old;
  return false;
}

bool check_construct(Formatter *f, State *st) {
  State old = *st;
  bool old_check = f->check_only;
  f->check_only = true;
  bool ok = expect_type_decl(f, st) && check_op(st, "{");
  f->check_only = old_check;
  *st = old;
  return ok;
}

void format_member_list(Formatter *f, State *st) {
  if (!expect_op(f, st, "{"))
    return;
  if (check_op(st, "}")) {
    expect_space(f, st, "");
    expect_op(f, st, "}");
    return;
  } else
    expect_indent_line(f, st, 2);

  while (expect_identifier(f, st)) {
    expect_space(f, st, " ");
    if (!expect_type_decl(f, st))
      return;
    if (check_op(st, ",")) {
      expect_space(f, st, "");
      expect_op(f, st, ",");
    } else {
      handle_replace(f, st, st, ",");
    }
    check_comment(f, st);
    if (check_op(st, "}")) {
      expect_space(f, st, "\n");
      expect_op(f, st, "}");
      break;
    }
    expect_indent(f, st, 2);
  }
}

void format_enum_member_list(Formatter *f, State *st) {
  if (!expect_op(f, st, "{"))
    return;
  if (check_op(st, "}")) {
    expect_space(f, st, "");
    expect_op(f, st, "}");
    return;
  } else
    expect_indent_line(f, st, 2);

  while (expect_identifier(f, st)) {
    if (check_op(st, "=")) {
      expect_space(f, st, " ");
      expect_op(f, st, "=");
      expect_space(f, st, " ");
      if (!expect_number(f, st))
        return;
    }
    if (check_op(st, ",")) {
      expect_space(f, st, "");
      expect_op(f, st, ",");
    } else {
      handle_replace(f, st, st, ",");
    }
    check_comment(f, st);
    if (check_op(st, "}")) {
      expect_indent_line(f, st, 0);
      expect_op(f, st, "}");
      break;
    }
    expect_indent(f, st, 2);
  }
}

bool expect_param_def(Formatter *f, State *st) {
  State old = *st;
  if (expect_op(f, st, "..."))
    return true;
  if (expect_word(f, st, "const"))
    expect_space(f, st, " ");
  if (expect_identifier(f, st)) {
    expect_space(f, st, " ");
    if (expect_type_decl(f, st))
      return true;
  }

  *st = old;
  return false;
}

bool fn_decl_finished(State st) {
  skip_whitespace_space(&st, true);
  return check_word(&st, "type") || check_word(&st, "fn") || check_word(&st, "cfn") || check_word(&st, "use");
}

void format_interface_fn_list(Formatter *f, State *st) {
  if (!expect_op(f, st, "{"))
    return;
  if (check_op(st, "}")) {
    expect_space(f, st, "");
    expect_op(f, st, "}");
    return;
  } else
    expect_indent_line(f, st, 2);

  while (expect_word(f, st, "fn")) {
    expect_space(f, st, " ");
    if (!expect_identifier(f, st))
      break;
    expect_space(f, st, "");
    if (expect_op(f, st, "(")) {
      expect_space(f, st, "");
      while (expect_param_def(f, st)) {
        expect_space(f, st, "");
        if (expect_op(f, st, ")"))
          break;
        if (!expect_op(f, st, ","))
          return;
        expect_space(f, st, " ");
      }
    }
    if (check_op(st, "}")) {
      expect_indent_line(f, st, 0);
      expect_op(f, st, "}");
      break;
    }
    if (!fn_decl_finished(*st)) {
      expect_space(f, st, " ");
      if (!expect_type_decl(f, st))
        return;
      check_comment(f, st);
      if (check_op(st, "}")) {
        expect_indent_line(f, st, 0);
        expect_op(f, st, "}");
        break;
      }
    }
    expect_indent(f, st, 2);
  }
}

void format_type(Formatter *f, State *st) {
  expect_space(f, st, " ");
  if (!expect_identifier(f, st))
    return;

  expect_space(f, st, " ");
  if (expect_word(f, st, "struct") || expect_word(f, st, "cstruct") || expect_word(f, st, "union")) {
    expect_space(f, st, " ");
    format_member_list(f, st);
  } else if (expect_word(f, st, "enum") || expect_word(f, st, "cenum")) {
    expect_space(f, st, " ");
    format_enum_member_list(f, st);
  } else if (expect_word(f, st, "interface")) {
    expect_space(f, st, " ");
    format_interface_fn_list(f, st);
  } else if (expect_word(f, st, "uniontype")) {
    expect_space(f, st, " ");
    format_enum_member_list(f, st);
  }
}

bool expect_unary_pre(Formatter *f, State *st) {
  return expect_l_op(f, st, "++") || expect_l_op(f, st, "--") || expect_l_op(f, st, "*") || expect_l_op(f, st, "!") ||
         expect_l_op(f, st, "&") || expect_l_op(f, st, "~") || expect_l_op(f, st, "-");
}

bool check_l_op(State st, const char *op) {
  skip_whitespace_space(&st, false);
  return is_op(&st, op);
}

const char *check_bin_op(State st) {
  const char *ops[] = {"<<=", ">>=", "&&", "||", "<<", ">>", ":=", "+=", "-=", "*=", "/=", "%=",
                       "&=",  "|=",  "^=", "==", "!=", "<=", ">=", "->", "=",  "<",  ">",  "+",
                       "-",   "*",   "/",  "%",  "&",  "|",  "^",  ".",  ":",  "?",  NULL};

  State old = st;
  for (const char **c = ops; *c; c++)
    if (is_op(&st, *c))
      return *c;
    else
      st = old;
  return NULL;
}

bool format_expression(Formatter *f, State *st, int ind, bool with_colon);

bool contains_new_line(State st, char a, char b) {
  int open = 1;
  while (*st.c) {
    if (*st.c == '\n')
      return true;
    if (*st.c == a)
      ++open;
    if (*st.c == b)
      --open;
    State_skip(&st);
    if (open == 0)
      break;
  }
  return false;
}

bool expect_expression_list(Formatter *f, State *st, int ind, const char *sep, char s, char e) {
  const bool nl = contains_new_line(*st, s, e);

  char end[] = {e, '\0'};
  if (nl)
    expect_indent(f, st, ind);
  else
    expect_space(f, st, "");
  if (!expect_l_op(f, st, end)) {
    while (format_expression(f, st, ind, true) || check_op(st, sep)) {
      if (check_op(st, end)) {
        if (nl)
          expect_indent(f, st, ind - 2);
        else
          expect_space(f, st, "");
        expect_l_op(f, st, end);
        break;
      }
      expect_space(f, st, "");
      if (!expect_l_op(f, st, sep))
        return false;
      check_comment(f, st);
      if (check_op(st, end)) {
        if (nl)
          expect_indent(f, st, ind - 2);
        else
          expect_space(f, st, "");
        expect_l_op(f, st, end);
        break;
      }
      if (nl)
        expect_indent(f, st, ind);
      else
        expect_space(f, st, " ");
    }
  }
  return true;
}

bool format_scope(Formatter *f, State *st, int ind);

bool check_post_fix(Formatter *f, State *st, int ind) {
  if (check_l_op(*st, "as")) {
    expect_l_space(f, st, " ");
    expect_l_op(f, st, "as");
    expect_l_space(f, st, " ");
    if (!expect_type_decl(f, st))
      return false;
  } else if (check_l_op(*st, "(")) {
    expect_l_space(f, st, "");
    expect_l_op(f, st, "(");
    expect_l_space(f, st, "");
    if (expect_op(f, st, ")"))
      return true;
    expect_expression_list(f, st, ind + 2, ",", '(', ')');
  } else if (check_l_op(*st, "[")) {
    expect_l_space(f, st, "");
    expect_l_op(f, st, "[");
    format_expression(f, st, ind, true);
    if (!expect_l_op(f, st, "]"))
      return false;
  } else if (check_l_op(*st, "++")) {
    expect_l_space(f, st, "");
    expect_l_op(f, st, "++");
  } else if (check_l_op(*st, "--")) {
    expect_l_space(f, st, "");
    expect_l_op(f, st, "--");
  } else
    return false;
  return true;
}

bool format_expression(Formatter *f, State *st, int ind, bool with_colon) {
  if (expect_unary_pre(f, st)) {
    expect_l_space(f, st, "");
    return format_expression(f, st, ind, with_colon);
  }

  if (check_construct(f, st)) {
    expect_type_decl(f, st);
    expect_space(f, st, "");
    expect_op(f, st, "{");
    expect_expression_list(f, st, ind + 2, ",", '{', '}');
  } else if (expect_l_op(f, st, "(")) {
    expect_space(f, st, "");
    if (format_expression(f, st, ind, with_colon)) {
      expect_space(f, st, "");
      if (!expect_l_op(f, st, ")"))
        return false;
    }
  } else if (expect_l_op(f, st, "[")) {
    expect_l_space(f, st, "");
    return expect_expression_list(f, st, ind + 2, ",", '[', ']');
  } else if (!expect_identifier(f, st) && !expect_number(f, st) && !expect_string(f, st)) {
    return false;
  }

  while (check_post_fix(f, st, ind))
    ;

  check_comment(f, st);
  const char *bin_op = check_bin_op(*st);
  if (bin_op && (with_colon || strcmp(bin_op, ":") != 0)) {
    if (*bin_op == '.' || *bin_op == ',')
      expect_l_space(f, st, "");
    else
      expect_l_space(f, st, " ");
    expect_l_op(f, st, bin_op);
    if (*bin_op == '.')
      expect_space(f, st, "");
    else if (count_nl_in_whitespace_space(*st) == 1)
      expect_indent_line(f, st, ind + 6);
    else
      expect_space(f, st, " ");

    return format_expression(f, st, ind, with_colon);
  }

  return true;
}

bool format_statement(Formatter *f, State *st, int ind);

bool format_scope_or_single(Formatter *f, State *st, int ind) {
  if (check_op(st, "{")) {
    expect_space(f, st, " ");
    expect_op(f, st, "{");
    format_scope(f, st, ind + 2);
    return true;
  } else {
    expect_indent_line(f, st, ind + 2);
    format_statement(f, st, ind + 2);
    return true;
  }
  return false;
}

bool optional_semicolon(Formatter *f, State *st) {
  if (check_l_op(*st, ";")) {
    expect_l_space(f, st, "");
    expect_l_op(f, st, ";");
    return true;
  }
  return false;
}

bool format_statement(Formatter *f, State *st, int ind) {
  if (expect_word(f, st, "break") || expect_word(f, st, "continue")) {
    optional_semicolon(f, st);
    return true;
  } else if (expect_word(f, st, "return") || expect_word(f, st, "delete")) {
    if (optional_semicolon(f, st))
      return true;
    if (count_nl_in_whitespace_space(*st) == 0) {
      expect_space(f, st, " ");
      format_expression(f, st, ind, true);
      optional_semicolon(f, st);
    }
    return true;
  } else if (expect_word(f, st, "if")) {
    expect_space(f, st, " ");
    if (format_expression(f, st, ind, true) && format_scope_or_single(f, st, ind)) {
      if (check_word_s(*st, "else")) {
        if (st->c[-1] == '}')
          expect_space(f, st, " ");
        else
          expect_indent_line(f, st, ind);
        expect_word(f, st, "else");
        if (check_word_s(*st, "if")) {
          expect_space(f, st, " ");
          return format_statement(f, st, ind);
        }
        return format_scope_or_single(f, st, ind);
      } else
        return true;
    }
  } else if (expect_word(f, st, "for") || expect_word(f, st, "while")) {
    expect_space(f, st, " ");
    expect_op(f, st, "(");
    expect_expression_list(f, st, ind, ";", '(', ')');
    return format_scope_or_single(f, st, ind);
  } else if (expect_op(f, st, "{")) {
    return format_scope(f, st, ind + 2);
  } else if (expect_word(f, st, "do")) {
    if (format_scope_or_single(f, st, ind)) {
      if (st->c[-1] == '}')
        expect_space(f, st, " ");
      else
        expect_indent_line(f, st, ind);
      expect_word(f, st, "while");
      expect_space(f, st, " ");
      expect_op(f, st, "(");
      return expect_expression_list(f, st, ind, ";", '(', ')');
    }
  } else if (expect_word(f, st, "switch")) {
    expect_space(f, st, " ");
    expect_op(f, st, "(");
    expect_expression_list(f, st, ind, ";", '(', ')');
    return format_scope_or_single(f, st, ind);
  } else if (expect_word(f, st, "case")) {
    expect_space(f, st, " ");
    format_expression(f, st, ind, false);
    expect_space(f, st, "");
    if (expect_op(f, st, ":"))
      return true;
  } else if (expect_word(f, st, "default")) {
    expect_space(f, st, "");
    if (expect_op(f, st, ":"))
      return true;
  } else if (expect_op(f, st, "//")) {
    State old = *st;
    while (*st->c && *st->c != '\n')
      State_skip(st);
    handle_ok(f, &old, st);
    return true;
  } else if (format_expression(f, st, ind, true)) {
    optional_semicolon(f, st);
    return true;
  }

  return false;
}

bool format_scope(Formatter *f, State *st, int ind) {
  if (check_op(st, "}")) {
    expect_space(f, st, "");
    return expect_op(f, st, "}");
  }
  expect_indent_line(f, st, ind);

  while (format_statement(f, st, ind)) {
    if (check_op(st, "}"))
      break;
    expect_indent(f, st, ind);
  }

  expect_indent_line(f, st, ind - 2);
  return expect_op(f, st, "}");
}

void format_fn(Formatter *f, State *st) {
  expect_space(f, st, " ");
  if (!expect_identifier(f, st))
    return;

  expect_space(f, st, "");
  expect_op(f, st, "(");
  expect_space(f, st, "");
  if (!expect_op(f, st, ")")) {
    while (expect_param_def(f, st)) {
      expect_space(f, st, "");
      if (check_op(st, ",")) {
        expect_space(f, st, "");
        expect_op(f, st, ",");
        expect_space(f, st, " ");
      } else if (check_op(st, ")")) {
        expect_space(f, st, "");
        expect_op(f, st, ")");
        break;
      }
    }
  }

  if (fn_decl_finished(*st))
    return;

  expect_space(f, st, " ");
  if (!expect_op(f, st, "{")) {
    if (!expect_type_decl(f, st))
      return;
    expect_space(f, st, " ");
    if (!expect_op(f, st, "{"))
      return;
  }
  format_scope(f, st, 2);
}

void format_c_block(Formatter *f, State *st) {
  expect_space(f, st, " ");
  if (!expect_op(f, st, "{"))
    return;

  State old = *st;
  int open = 1;
  while (*st->c) {
    if (*st->c == '{')
      ++open;
    if (*st->c == '}')
      --open;
    State_skip(st);
    if (open == 0)
      break;
  }
  handle_ok(f, &old, st);
}

void State_skip_line(State *st) {
  while (*st->c && *st->c != '\n')
    State_skip(st);
  State_skip(st);
}

char after_space_line(State st) {
  while (*st.c && *st.c != '\n' && isspace(*st.c))
    State_skip(&st);
  return *st.c;
}

char after_space(State st) {
  while (*st.c && isspace(*st.c))
    State_skip(&st);
  return *st.c;
}

bool word_after_space(State st, const char *word) {
  while (*st.c && isspace(*st.c))
    State_skip(&st);
  return check_word(&st, word);
}

bool op_after_space(State st, const char *op) {
  while (*st.c && isspace(*st.c))
    State_skip(&st);
  return check_op(&st, op);
}

void skip_lines(Formatter *f, State *st, int l) {
  State old = *st;
  while (*st->c) {
    if (*st->c == '\n') {
      l--;
      if (l == 0) {
        State_skip(st);
        handle_replace(f, &old, st, "");
        return;
      }
    }
    State_skip(st);
  }
}

void format_scopex(Formatter *f, State *st, int indent, char end, bool skip_end);

void format_if_like(Formatter *f, State *st, int indent) {
  expect_l_space(f, st, " ");
  if (*st->c == '(') {
    State_skip(st);
    format_scopex(f, st, indent + 2, ')', true);

    if (after_space(*st) == '{') {
      expect_space(f, st, " ");
    } else if (after_space_line(*st) != '\n') {
      expect_space(f, st, " ");
    } else {
      expect_indent_line(f, st, 0);
      State_skip(st);
      const int nl = count_nl_in_whitespace_space(*st);
      if (nl > 0)
        skip_lines(f, st, nl);
      expect_indent_line(f, st, indent + 2);
      format_scopex(f, st, indent + 2, '\n', false);
    }
  }
}

void format_else_like(Formatter *f, State *st, int indent) {
  if (after_space(*st) == '{') {
    expect_space(f, st, " ");
  } else if (after_space_line(*st) != '\n') {
    expect_space(f, st, " ");
  } else {
    expect_indent_line(f, st, 0);
    State_skip(st);
    const int nl = count_nl_in_whitespace_space(*st);
    if (nl > 0)
      skip_lines(f, st, nl);
    expect_indent_line(f, st, indent + 2);
  }
}

void format_dowhile_like(Formatter *f, State *st, int indent) {
  if (after_space(*st) == '{') {
    expect_space(f, st, " ");
    State_skip(st);
    format_scopex(f, st, indent + 2, '}', true);
    if (word_after_space(*st, "while")) {
      expect_space(f, st, " ");
      expect_word(f, st, "while");
      if (after_space(*st) == '(')
        expect_space(f, st, " ");
    }
  }
}

void format_scopex(Formatter *f, State *st, int indent, char end, bool skip_end) {
  const int line_at_start = st->location.line;
  if (after_space(*st) == end) {
    expect_space(f, st, "");
  } else if (after_space_line(*st) == '\n') {
    expect_space(f, st, "\n");
    expect_indent_line(f, st, indent);
  } else
    expect_space(f, st, "");
  const char *t = NULL;
  while (*st->c) {
    if (*st->c == end) {
      if (skip_end) {
        State_skip(st);
        const char next = after_space_line(*st);
        if (next == ')' || next == '}' || next == ']' || next == ',' || next == ';')
          expect_l_space(f, st, "");
      }
      return;
    } else if (*st->c == '\n') {
      int nl = count_nl_in_whitespace_space(*st);
      if (nl > 2)
        skip_lines(f, st, nl - 2);
      else
        State_skip(st);
      const char n = after_space_line(*st);
      if (n == end && end != '\0') {
        expect_indent_line(f, st, indent < 2 ? 0 : indent - 2);
        State_skip(st);
        return;
      }
      if (n != '\n' && (word_after_space(*st, "case") || word_after_space(*st, "default")))
        expect_indent_line(f, st, indent < 2 ? 0 : indent - 2);
      else
        expect_indent_line(f, st, n == '\n' ? 0 : indent);
    } else if (isspace(*st->c)) {
      if (after_space_line(*st) == '\n') {
        expect_indent_line(f, st, 0);
      }
      State_skip(st);
    } else if (*st->c == '{') {
      State_skip(st);
      format_scopex(f, st, indent + 2, '}', true);
      if (word_after_space(*st, "else"))
        expect_space(f, st, " ");
      else {
        const char next = after_space_line(*st);
        if (end != '\n' && end != '\0' && next == end && line_at_start < st->location.line)
          expect_indent(f, st, indent < 2 ? 0 : indent - 2);
        else if (next != '\n')
          expect_l_space(f, st, "");
      }
    } else if (*st->c == '[') {
      State_skip(st);
      format_scopex(f, st, indent + 2, ']', true);
      if (end != '\n' && end != '\0' && after_space_line(*st) == end && line_at_start < st->location.line)
        expect_indent(f, st, indent < 2 ? 0 : indent - 2);
    } else if (*st->c == '(') {
      State_skip(st);
      format_scopex(f, st, indent + 2, ')', true);
      if (end != '\n' && end != '\0' && after_space_line(*st) == end && line_at_start < st->location.line)
        expect_indent(f, st, indent < 2 ? 0 : indent - 2);
    } else if (expect_op(f, st, "//")) {
      while (*st->c && *st->c != '\n')
        State_skip(st);
    } else if (expect_word(f, st, "if") || expect_word(f, st, "while") || expect_word(f, st, "for") ||
               expect_word(f, st, "switch")) {
      format_if_like(f, st, indent);
    } else if (expect_word(f, st, "else")) {
      format_else_like(f, st, indent);
    } else if (expect_word(f, st, "return")) {
      if (after_space_line(*st) != '\n')
        expect_l_space(f, st, " ");
    } else if (expect_word(f, st, "do")) {
      format_dowhile_like(f, st, indent);
    } else if (expect_word(f, st, "ccode") || expect_word(f, st, "cmain")) {
      expect_l_space(f, st, " ");
      if (*st->c == '{') {
        State_skip(st);
        int c = 1;
        while (*st->c) {
          if (*st->c == '{')
            ++c;
          if (*st->c == '}')
            --c;
          State_skip(st);
          if (c == 0)
            break;
        }
      }
    } else if (expect_unary_pre(f, st)) {
      if (after_space_line(*st) == '\n')
        expect_indent_line(f, st, 0);
      else if (isspace(*st->c))
        expect_indent_line(f, st, 1);
    } else if ((t = check_bin_op(*st))) {
      State_skip_n(st, strlen(t));
      const char n = after_space_line(*st);
      if (end != '\n' && end != '\0' && n == end && line_at_start < st->location.line) {
        expect_indent(f, st, indent < 2 ? 0 : indent - 2);
      } else if (strcmp(".", t) == 0 && n != '\n')
        expect_l_space(f, st, "");
      else if (n != '\n')
        expect_l_space(f, st, " ");
    } else if (*st->c == ',' || *st->c == ';') {
      State_skip(st);
      if (after_space_line(*st) != '\n')
        expect_l_space(f, st, " ");
    } else if (expect_string(f, st) || expect_number(f, st) || expect_identifier(f, st)) {
      if (op_after_space(*st, "++") || op_after_space(*st, "--"))
        expect_l_space(f, st, "");
      else if (op_after_space(*st, ":="))
        expect_l_space(f, st, " ");
      else {
        const char n = after_space_line(*st);
        if (n == end && end != '\n' && end != '\0' && line_at_start < st->location.line) {
          expect_indent(f, st, indent < 2 ? 0 : indent - 2);
        } else if (n == '{' && end != '\0')
          expect_l_space(f, st, "");
        else if (n == '(' || n == '[' || n == ')' || n == ']' || n == '}' || n == ';' || n == ',' || n == '.' ||
                 n == '\n')
          expect_l_space(f, st, "");
        else if (n == ':') {
          if (isspace(*st->c))
            expect_l_space(f, st, " ");
          else
            expect_l_space(f, st, "");
        } else
          expect_l_space(f, st, " ");
      }
    } else
      State_skip(st);
  }
}

void format_file(Formatter *f, State *st) { format_scopex(f, st, 0, '\0', false); }

#ifdef WIN32
#define STDIN_FILENO _fileno(stdin)
#define read(a, b, c) _read(a, b, c)
#endif

int main(int argc, char *argv[]) {
  char *code = argc > 1 ? readFile(argv[1]) : readStdin();

  Formatter f = (Formatter){false, true, true};
  State st = State_new(code, argc > 1 ? argv[1] : "main_file");
  fprintf(stdout, "[\n");
  format_file(&f, &st);
  fprintf(stdout, "]\n");

  if (st.c[0] != '\0')
    FATAL(&st.location, "format fail");

  free(code);
}
