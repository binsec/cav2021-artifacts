volatile int a = 0;
volatile int b = 9;
volatile int x = 4; // chosen with a fair dice roll
int main() {
  if (a*x + b > 0) {
    return 0;
  } else {
    x = 42;
    return 1;
  }
}
