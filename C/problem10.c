#include "stdio.h"

int isPrime(int x);

int main(int argc, char *argv[])
{
  (void) argc;
  (void) argv;

  long sum, i;
  sum = 2;
  for(i = 3; i < 2000000; i +=2){
    if(isPrime(i)) sum += i;
  }

  printf("Solution : %lu", sum);
  
  return 0;
}


int isPrime(int x){
  if(x == 2) return 1;
  if(x % 2 == 0) return 0;
  int i;
  for(i = 3; i * i <= x; i += 2){
    if(x % i == 0){
      return 0;
    }
  }
  return 1;
}
