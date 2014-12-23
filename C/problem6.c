#include "stdio.h"


int main(int argv, char ** argc){
  (void) argv;
  (void) argc;

  long n = 100;
  long result = n * (n + 1) * (n - 1) * ( 3 * n + 2) / 12;

  printf("Solution : %lu\n", result);


  return 0;
}
