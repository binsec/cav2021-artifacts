volatile int a = 0;
volatile int b = 9;
volatile int x = 4; // chosen with a fair dice roll

int main() {
  // write on the stack
  int z[20] = {0};
  // ensure the line above writes before the test
  asm volatile ("" ::: "memory");
  // load a, x, b from .data
  if (a*x + b >= 0) {
    // for simplification to work, binsec must know that esp is notably
    // different from the location of a, b, x
    return 0;
  } else {
    z[1] = 42;
    z[0] = 1;
    // ensure z is used.
    return z[!!a];
  }
}
