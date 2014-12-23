#include "stdio.h"
#include "string.h"
#include "assert.h"

int isPalin(int);

int main (int argv, char ** argc){
  (void) argv;
  (void) argc;

  int x, y;
  
  int result = 0;

  assert(isPalin(1001));
  assert(! isPalin(1737));
  
  for(x = 100; x < 1000; x ++){
    for(y = x; y < 1000; y ++){
      int temp = x * y;
      if(temp > result && isPalin(temp)){
	result = temp;
      }
    }

  }

  printf("Result : %d\n", result);

  
  return 0;
}


int isPalin(int number){
  char str[10];
  sprintf(str, "%d", number);

  int length = strlen(str);

  int i;

  for(i = 0; i < length / 2; i ++){
    if(str[i] != str[length - i - 1])
      return 0;
  }
  return 1;

}
