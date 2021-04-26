#include <stdio.h>
int main() {
  int a = printf("Compiler ! don't try to optimize this !");
  //a = 1;
  char* b = "01";
  switch (a) {
    case 1: b = "10";
            break;
    case 2: b = "42";
            break;
    case 3: b = "93";
            break;
    case 4: b = "16";
            break;
    case 5: b = "25";
            break;
    case 6: b = "36";
            break;
    case 7: b = "49";
            break;
    case 8: b = "j'ai perdu";
            break;
    case 9: b = "37";
            break;
  }
  int result = b[0]-b[1];
  result = result>0?result:-result;
  if (result > 10) {
    return 0;
  }
  return 1;
}

