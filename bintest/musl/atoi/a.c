#include <fnmatch.h>

char input[20] = "42";

int main() {
  input[4]='\0';
  if (atoi(input)==42) {
    exit(1);
  }
  return 0;
}
