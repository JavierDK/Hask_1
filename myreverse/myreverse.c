int print_reversed(char* arr, int first, int last, int size)
{
  int i, sz = 0;
  char* ans = (char*)calloc(size + 1, sizeof(char));
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

int procBuffer(int read_status, int *sz, int *over, char *buf, int size)
{
	*sz += read_status;
	int pos = 0;
	for (int i = 0; i < *sz; i++)
    {
      if (buf[i] == '\n')
      {
        if (! *over)
 		{
          if (print_reversed(buf, pos, i, size) <= 0)
            return -1;
        }    
        else
          *over = 0;
        pos = i+1;
      }
	}
	if (*sz == size + 1 && pos == 0)
	{
	  *over = 1;
	  *sz = 0;
	  for (int i = 0; i <= size; i++)
		buf[i] = 0;
	}
	else
	{
		for (int i = pos; i < *sz; i++)
			buf[i - pos] = buf[i];
		*sz = *sz - pos;
		for (int i = *sz; i < size + 1; i++)
			buf[i] = 0;
	}
    return 0;
}

