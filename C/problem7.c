#include "stdio.h"

int main(int argc, char *argv[])
{
  (void) argc;
  (void) argv;
  int i, j, result;
  result = 1;
  for(i = 2; i < 10002; i ++){
    result += 2;
    for(j = 3; j * j <= result; j += 2){
      if(result % j == 0){
	result +=2;
	j = 1;
      }
    }
  }
  printf("Solution : %d\n", result);
  
  return 0;
}
