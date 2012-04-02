#include <stdio.h>

#define BUF_SZ 5

int print_reversed(char* arr, int first, int last)
{
  int i, sz = 0;
  char ans[BUF_SZ+1];
  for (i = last-1; i >= first; i--)
  {
    ans[sz] = arr[i];
    sz++;
  }
  ans[sz] = arr[last];
  sz++;
  if (ans[sz-1] == 0)
	sz--;	
  return write(1, ans, sz);
}

int main()
{
  int sz = 0, over = 0;
  char buf[BUF_SZ+1];
  for (int i = 0; i <= BUF_SZ; i++)
	buf[i] = 0;
  int read_status; 
  while (1)
  {
    read_status = read(0, buf+sz, BUF_SZ - sz + 1);
    if (read_status < 0)
      return -1;
	if (read_status == 0)
	{
	  if (! over)
        return print_reversed(buf, 0, sz);
	}
    sz += read_status;
    int pos = 0;
    for (int i = 0; i < sz; i++)
    {
      if (buf[i] == '\n')
      {
        if (! over)
 		{
//		  printf("%d %d\n",pos,i);
          if (print_reversed(buf, pos, i) <= 0)
            return -1;
        }    
        else
          over = 0;
        pos = i+1;
      }
	}

	if (sz == BUF_SZ + 1 && pos == 0)
	{
	  over = 1;
	  sz = 0;
	  for (int i = 0; i <= BUF_SZ; i++)
		buf[i] = 0;
	}
	else
	{
		for (int i = pos; i < sz; i++)
			buf[i - pos] = buf[i];
		sz = sz - pos;
		for (int i = sz; i < BUF_SZ + 1; i++)
			buf[i] = 0;
	}
    
  }
  return 0;
}
