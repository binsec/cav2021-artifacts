#include <string.h>

#define SIZE 7

char symbolic_input[2*SIZE] = "foobar";

void get_secret(char secr[]) {
  strncpy(secr, "secret", SIZE);
}

void read_input(char src[], char dst[]) {
  int i = 0;
  while (src[i]) {
    dst[i] = src[i];
    i++;
  };
}

int validate(char secr[], char input[]) {
  int b = 1;
  for (int i =0; i < SIZE; i++) {
    b &= secr[i] == input[i];
  }
  return b;
}

int main () {
  char secr[SIZE];
  char input[SIZE];

  get_secret(secr);
  read_input(symbolic_input, input);

  return validate(secr, input);
}

