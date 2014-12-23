#include "stdio.h"

#define CHAR_TO_INT(x) (int)((x) - '0')
#define LENGTH 4
int main(int argc, char *argv[])
{
  (void) argc;
  (void) argv;

  int grid [20][20];
  FILE * file = fopen("../grid.txt", "r");
  
  
  int x = 0;
  int y = 0;
  char c;
  
  for(c = getc(file); c != EOF; c = getc(file)){
    if(c == ' '){
      x ++;
    } else if ( c =='\n'){
      x = 0;
      y ++;
    } else {
      char d = getc(file);
      grid[x][y] = CHAR_TO_INT(c) * 10 + CHAR_TO_INT(d);
    }
  }

  int best= 0;
  //vertical
  for(x = 0; x < 20; x ++){
    for(y = 0; y < 20 - LENGTH; y ++){
      int i;
      int current = 1;
      for(i = 0; i < LENGTH; i ++){
	current *= grid[x][y + i];	
      }
      if(current > best) best = current;      
    }
  }

  //horizontal
  for(x = 0; x < 20 - LENGTH; x ++){
    for(y = 0; y < 20; y ++){
      int i;
      int current = 1;
      for(i = 0; i < LENGTH; i ++){
	current *= grid[x + i][y];
      }
      if(current > best) best = current;
    }
  }

  //top left to bottom right
  for(x = 0; x < 20 - LENGTH; x ++){
    for(y = 0; y < 20 - LENGTH; y ++){
      int i;
      int current = 1;
      for(i = 0; i < LENGTH; i ++){
	current *= grid[x + i][y + i];
      }
      if(current > best) best = current;
    }
  }

  //top right to bottom left
  for(x = LENGTH - 1; x < 20; x ++){
    for(y = 0; y < 20 - LENGTH; y ++){
      int i;
      int current = 1;
      for(i = 0; i < LENGTH; i ++){
	current *= grid[x - i][y + i];
      }
      if(current > best) best = current;
    }
  }

  printf("Solution = %d\n", best);
  
  return 0;
}
