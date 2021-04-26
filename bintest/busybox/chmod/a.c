#include <stdlib.h>
//#include <libbb.h>
#define FAST_FUNC __attribute__((regparm(3),stdcall))
int bb_parse_mode(const char* str, unsigned current_mode) FAST_FUNC;


char input[20] = "u+x";

int main() {
  input[3]='\0';
  int current_mode = 0666;
  if (bb_parse_mode(input, current_mode)==0766) {
    exit(1);
  }
  return 0;
}
