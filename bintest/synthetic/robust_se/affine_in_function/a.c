volatile int a = 0;
volatile int b = 9;
volatile int x = 4; // chosen with a fair dice roll

__attribute__ ((noinline)) int test(int a, int b, int x)
{
  return a*x + b > 0;
}

int main() {
  if (test(a, b, x)) {
    return 0;
  } else {
    x = 42;
    return 1;
  }
}
