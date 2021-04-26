#include <string.h>
#include <stdlib.h>

char mid[20] =  "a3.1";

int main() {
  const char* lo = "a3";
  const char* hi = "a5";
  mid[5] = '\0';
  if (strverscmp(lo, mid)<0 && strverscmp(mid, hi)<0) {
    exit(1);
  }
  return 0;
}
