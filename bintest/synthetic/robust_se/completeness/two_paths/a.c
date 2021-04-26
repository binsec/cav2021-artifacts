#include <stdlib.h>

int main(int argc, char** argv) {
  if (argc) {
    argv[0][0] = '\0';
  }
  return 0;
}
