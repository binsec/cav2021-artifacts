#include <fnmatch.h>
#include <stdlib.h>

char input[20] = "ajh";

int main() {
  input[5] = '\0';
  if (fnmatch("c*l", input, 0)) {
    /* no match */
    exit(1);
  }
  /* match ! */
  return 0;
}
