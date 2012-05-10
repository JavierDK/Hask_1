#define BUF_SZ 5

int main()
{
  int sz = 0, over = 0;
  char buf[BUF_SZ+1];
  for (int i = 0; i <= BUF_SZ; i++)
	buf[i] = 0;
  int read_status, pr_status; 
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
	pr_status = procBuffer(read_status, &sz, &over, buf, BUF_SZ);
	if (pr_status < 0)
		return -1;
  }
  return 0;
}
