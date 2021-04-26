void init(int *);
void assign_one(int *);


int body(int input) {
  int result;
  int nondet[100];

  // this one is unlikely to be initialized
  init(&nondet[0]);
  if (nondet[0]) {
    result = 1;
  } else {
    assign_one(&result);
  }
  result += input;
  return result;
}

volatile int input = 0;
int main() {
  return body(input);
}


