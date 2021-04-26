#include <stdlib.h>

char input[20] = "+7@";

int main() {
  input[4]='\0';
  if (strtol(input, NULL, 10)==42L) {
    exit(1);
  }
  return 0;
}
