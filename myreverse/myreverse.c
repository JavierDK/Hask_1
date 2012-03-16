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
  return write(1, ans, sz);
}

int main()
{
  int sz = 0, over = 0;
  char buf[BUF_SZ+1];
  int read_status; 
  while (1)
  {
    read_status = read(0, buf+sz, BUF_SZ-sz + 1);
    if (read_status < 0)
      return -1;
    sz = BUF_SZ+1;
    int pos=0;
    for (int i = 0; i < sz; i++)
      if (buf[i]=='\n')
      {
        if (! over)
 		{
          if (print_reversed(buf, pos, i) <= 0)
            return -1;
        }    
        else
          over = 0;
        pos = i+1;
      }
    if (read_status == 0)
      return 0;
    if (pos == 0)
    {
      sz = 0;
      over = 1;
    }
    else
    {
       for (int j= pos; j < BUF_SZ+1; j++)
          buf[j - pos] = buf[j];
        sz = BUF_SZ + 1 - pos;        
        over = 0;
    }
  }
  return 0;
}
