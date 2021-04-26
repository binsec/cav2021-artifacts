#include <stdio.h>
//#include <iostream>
//#include <iomanip>


//using namespace std;
#define size 16
extern unsigned int a[size/16];
unsigned int b[size/16];
extern unsigned int expected[size/16];

int main()
{
  //int size;

  //cin >> size;
  

  //unsigned int* a = new unsigned int[size / 16]; // <- input tab to encrypt
  //unsigned int* b = new unsigned int[size / 16]; // <- output tab
 
  //for (int i = 0; i < size / 16; i++) {   // Read size / 16 integers to a
  //  cin >> hex >> a[i];
  //}

  for (int i = 0; i < size / 16; i++) {   // Write size / 16 zeros to b
    b[i] = 0;
  }	

  for (int i = 0; i < size; i++)
    for (int j = 0; j < size; j++) {
      b[(i + j) / 16] ^= ( (a[i / 16] >> (i % 16)) &
		       (a[j / 16 + size / 16] >> (j % 16)) & 1 ) << ((i + j) % 16);   // Magic centaurian operation
  }


  int blob = 1;
  for (int i = 0; i<size/16; i++) {
    blob &= expected[i]==b[i];
  }
 
  //for(int i = 0; i < size / 16; i++) {
  //  if (i > 0) {
  //    cout << ' ';
  //  }
  //  cout << setfill('0') << setw(8) << hex << b[i];       // print result
  //}
  //cout << endl;

 /* 
    Good luck humans     
 */
  return blob;

}
