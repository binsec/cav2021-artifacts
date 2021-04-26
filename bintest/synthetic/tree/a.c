#include <stdio.h>
int main() {
  int a = printf("Compiler ! dont optimize this !");
  int b = 1;
// expected : a&4 != 0 a!=7 a&2 = 0
label:
  if (a==7) b = 3;
  if (a&2) b = 4;
  else {
    if (a&4) b += 4;
    else {
      if (a&8) b= 9;
    if (a&16){
      a+=1;
      goto label;
    }
    }
  }
  return b;
}

