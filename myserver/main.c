#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>

typedef struct SocketPair
{
	int* ipv4;
	int* ipv6;
} SocketPair;

SocketPair createSockets(int n)
{
	SocketPair res;
	res.ipv6 = (int*)calloc(n, sizeof(int));
	res.ipv4 = (int*)calloc(n, sizeof(int));
	for (int i = 0; i < n; i++)
	{
		res.ipv6[i] = socket(AF_INET6, SOCK_STREAM, 0);
		res.ipv4[i] = socket(AF_INET, SOCK_STREAM, 0);
	}
	return res;
}

int main(int argc, char **argv)
{
	if (argc < 2)
	{
		char *err_mas = "There is no ports to bind socket\n";
		write(1, err_mas, sizeof(char)*strlen(err_mas));
		return -1; 
	}
	int socketAmount = argc - 1;
	createSockets(socketAmount);	
	return 0;
}
