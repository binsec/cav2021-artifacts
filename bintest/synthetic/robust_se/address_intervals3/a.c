volatile int a = 0;

int main() {
  // write on the stack
  volatile int j = 2;
  // ensure the line above writes before the test
  asm volatile ("" ::: "memory");
  // load a, x, b from .data
  if (a == 0) {
    // for simplification to work, binsec must know that esp is notably
    // different from the location of a, b, x
    return 0;
  } else {
    return a+j;
  }
}
