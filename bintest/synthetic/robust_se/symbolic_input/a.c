static char minuscules[26] = {
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'i', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'u', 'u', 'x', 'y', 'z'
};
static char majuscules[26] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'I', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'V', 'V', 'V', 'X', 'Y', 'Z'
};
char input = 'a';
int main() {
  volatile char output = input;

  if (input >= 'a' && input <= 'z') {
    // read memory on a symbolic address
    output = minuscules[input - 'a'];
  }
  if (input >= 'A' && input <= 'Z') {
    // read memory on a symbolic address
    output = majuscules[input - 'A'];
  }
  if (input != output) {
    return 42;
  }
  return 0;
}


