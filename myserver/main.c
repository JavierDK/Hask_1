#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <netdb.h>

typedef struct SocketPair
{
	int* ipv4;
	int nv4, nv6;
	int* ipv6;
} SocketPair;

SocketPair createSockets(int n, char **ports)
{
	SocketPair res;
	res.ipv6 = (int*)calloc(n, sizeof(int));
	res.ipv4 = (int*)calloc(n, sizeof(int));
	int n_ipv4 = 0, n_ipv6 = 0;
	for (int i = 0; i < n; i++)
	{
		res.ipv6[i] = socket(AF_INET6, SOCK_STREAM, 0);
		if (res.ipv6[i] >= 0)
			n_ipv6++; 
		res.ipv4[i] = socket(AF_INET, SOCK_STREAM, 0);
		if (res.ipv4[i] >= 0)
			n_ipv4++;
	}
	printf("%d IPv6 sockets was created\n", n_ipv6);
	res.nv4 = n_ipv4;
	res.nv6 = n_ipv6;
	printf("%d IPv4 sockets was created\n", n_ipv4);
	if (n_ipv6 != n || n_ipv4 != n)
	{
		char *msg = "Error: Impossible to create socket!!!\n";
		write(2, msg, strlen(msg));
		exit(1);
	}
	return res;
}

void bindSocks(SocketPair *s, char **port, int n)
{
	for (int i = 0; i < n; i++)
	{
		for (int j = 0; j < strlen(port[i]); j++)
		{
			if ((port[i][j] < '0' || port[i][j] > '9'))
			{
				char *msg = "Error in arguments parsing\n";
				write(2, msg, strlen(msg));
				exit(1);
			}
		}
		int num = atoi(port[i]);
		struct sockaddr_in serv_in4, cli_in4;
		struct sockaddr_in6 serv_in6, cli_in6;
		serv_in4.sin_family = AF_INET;
		serv_in4.sin_addr.s_addr = INADDR_ANY;
		serv_in4.sin_port = htons(num);
		if (bind(s -> ipv4[i], (struct sockaddr *) &serv_in4, sizeof(serv_in4)) < 0)
		{
			char *msg = "Impossible to bind IPv4 socket and port\n";
			write(2, msg, strlen(msg));
			exit(1);
		}
		else
		{
			write(1, port[i], strlen(port[i]));
			char *msg = " port and IPv4 socket binded\n";
			write(1, msg, strlen(msg));
		}
		bzero(&serv_in6, sizeof(serv_in6));
		serv_in6.sin6_family = AF_INET6;
		serv_in6.sin6_port = htons(num);
		int fl;
		fl = setsockopt(s -> ipv6[i], IPPROTO_IPV6, IPV6_V6ONLY, "1", sizeof(int));
		if (bind(s -> ipv6[i], (struct sockaddr *) &serv_in6, sizeof(serv_in6)) < 0)
		{
			char *msg = "Impossible to bind IPv6 socket and port\n";
			write(2, msg, strlen(msg));
			exit(1);
		}
		else
		{
			write(1, port[i], strlen(port[i]));
			char *msg = " port and IPv6 socket binded\n";
			write(1, msg, strlen(msg));
		}		
	}
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
	SocketPair socks;
	socks = createSockets(socketAmount, argv+1);
	bindSocks(&socks, &argv[1], socketAmount);				
	return 0;
}
