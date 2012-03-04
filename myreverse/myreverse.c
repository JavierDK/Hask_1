#include <stdio.h>
#include <stdlib.h>

#define BUF_SZ 10

void print_reversed(char* arr, int first, int last)
{
  int i;
  for (i = last; i >= first; i--)
    printf("%c", arr[i]);
  printf("\n");
}

void flush()
{
}

int main()
{
  int sz = 0, over = 0;
  char buf[BUF_SZ+1];
  for (;;)
  {
    fread(buf+sz, 1, BUF_SZ-sz + 1, stdin);
    sz = BUF_SZ+1;
    int pos=0;
    for (int i = 0; i < sz; i++)
      if (buf[i]=='\n')
      {
        if (! over)
          print_reversed(buf, pos, i-1);
        else
          over = 0;
        pos = i+1;
        
      }
    if (pos == 0)
    {
      sz = 0;
      over = 1;
    }
    else
    {
       for (int j= pos; j < BUF_SZ+1; j++)
          buf[j - pos] = buf[j];
        sz = BUF_SZ - pos;        
        over = 0;
    }
  }
  return 0;
}
