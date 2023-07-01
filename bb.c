#include <ctype.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

void assert_imp_(const char *f, int l, int c, bool condition, const char *code) {
  if (!condition) {
    fprintf(stderr, "%s:%d:%d: failed: %s\n", f, l, c, code);
    abort();
  }
}
#define __NEW_(T, src) ((T *)memcpy(malloc(sizeof(T)), src, sizeof(T)))




typedef enum main_TestE {
  main_A = 3,
  main_B,
  main_C
} main_TestE;









int32_t main_dummy(int32_t a);
int32_t main_main(int32_t argc, char* argv[]);




int32_t main_dummy(int32_t a) {
  return a;
}

int32_t main_main(int32_t argc, char* argv[]) {
  for (int32_t i = 0; i < argc; ++i)
    printf("%s ", argv[i]);
  printf("\n");
  int32_t v[] = {3, 1, 2};
  printf("%d %d %d\n", v[0], v[1], v[2]);
  {
    int32_t val;switch ((val = main_dummy(3)))  {
      case 3: {
        printf("Yes %d\n", val);
        break;
      }
      default: {
        assert_imp_("main.jnq", 23, 18, false, "'false'");
        break;
      }
    }  }  assert_imp_("main.jnq", 26, 9, ((int32_t)(main_A)) == 3, "'TestE.A as i32 == 3'");
  assert_imp_("main.jnq", 27, 9, ((int32_t)(main_B)) == 4, "'TestE.B as i32 == 4'");
  assert_imp_("main.jnq", 28, 9, ((int32_t)(main_C)) == 5, "'TestE.C as i32 == 5'");
  printf("ok\n");
  return 0;
}


int main(int argc, char **argv) {
  return main_main(argc, argv);
}
