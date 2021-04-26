void init(int *);
void assign_one(int *);


int body(int input) {
  int result;
  int nondet[100];

  // this one is unlikely to be initialized
  init(nondet);
  if (nondet[0]) {
    result = 2;
  } else {
    if (nondet[1]) {
      assign_one(&result);
    } else {
      assign_one(&nondet[2]);
      result = nondet[2];
    }
  }
  result += input;
  init(&result);
  if (nondet[3]) {
    init(&result);
  }
  return result;
}

volatile int input = 0;
int main() {
  return body(input);
}


