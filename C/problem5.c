#include "stdio.h"

long lcm(long, long);
long gcd(long, long);

int main(int argc, char ** argv){
  (void) argc;
  (void) argv;

  long result = 1;

  long i = 2;

  for(i = 2; i < 21; i ++){
    if(result % i != 0) result = lcm(result, i);
  }

  printf("Solution : %lu\n", result);

  return 0;
}

long gcd(long x, long y){
  long temp;
  if( x < y ){
    temp = x;
    x = y;
    y = temp;
  }


  while ( y != 0 ){
    temp = x % y;
    x = y;
    y = temp;
  }

  return x;
}


long lcm(long x, long y){
  return x * y / gcd(x, y);
}
