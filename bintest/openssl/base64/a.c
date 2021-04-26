#include <string.h>
int ct_base64_decode(const char *in, unsigned char **out);

volatile unsigned char* goal = "oui\n";
volatile char* input = "b3VpCg==";

int main(char* argc, char** argv) {
  unsigned char temp[20] = {0};
  unsigned char *ptemp = temp;
  int outlen = ct_base64_decode(input, &ptemp);
  if (outlen < 0) {
    return 1;
  }
  if (outlen != strlen(goal)) {
    return 2;
  };
  if (strcmp(goal, ptemp)) {
    return 3;
  }
  return 0;
}
  


