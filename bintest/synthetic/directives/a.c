volatile int choice = 0;

int main() {
  if (choice < 0) {
    choice = - choice;
  }
  int a;
start:
  switch (choice) {
    case 1: a = 2;
            break;;
    case 2: a = 3;
            break;;
    case 4: a = 6;
            break;;
    default:
            choice--;
            goto start;
  }
  long long res = a*a + choice*choice + 2;
  if (res < 0) {
    choice = -1;
  }
  return a+choice;
}

