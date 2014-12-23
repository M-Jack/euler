#include "stdio.h"



int main(int argc, char *argv[])
{
  (void) argc;
  (void) argv;


  int a,b,c;
  for(a = 1; a < 334; a ++){
    for(b = a, c = 1000 - 2 * a; b < c; b ++, c --){
      if( c * c == a * a + b * b){
	printf("Solution : %d", a * b * c);	
	return 0;
      }
    }
  }
  return 0;
}
