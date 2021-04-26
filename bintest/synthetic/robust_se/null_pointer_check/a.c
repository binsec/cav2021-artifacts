volatile int a = 0;

__attribute__ ((noinline)) int null_ptr_check(int *arg) {
  // this tests whether the stack pointer is 0
  if (arg) {
    *arg = 42;
    return 1;
  }
  return 0;
}


int main() {
  // write on the stack
  int j = a;
  // ensure the line above writes before the test
  asm volatile ("" ::: "memory");
  // the argument is an address on the stack
  null_ptr_check(&j);
  if (j < 0) {
    return 0;
  } else {
    return j + a;
  }
}
