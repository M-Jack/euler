#include "stdio.h"

int main (int argc, char ** argv){
  (void) argc;
  (void) argv;

  int f1 = 0;
  int f2 = 1;

  int result = 0;

  while( f2 < 4000000){
    if(f2 % 2 == 0)
      result += f2;
    f2 = f2 + f1;
    f1 = f2 - f1;

  }

  printf("Result : %d \n", result);
  return 0;
}
