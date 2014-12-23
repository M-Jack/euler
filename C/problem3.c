#include "stdio.h"



int main(int argv, char ** argc){
  (void) argv;
  (void) argc;

  unsigned long target = 600851475143;

  unsigned long prime;

  for(prime = 3l; prime * prime < target; prime += 2){
    if (target % prime == 0){
      target = target / prime;
      prime -=2;
    }
  }
  
  printf("Result : %lu\n", target);
  

  return 0;
}
