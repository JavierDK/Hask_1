#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <netdb.h>
#include <poll.h>
#include <pthread.h>

int failed[1024];


typedef struct mypair
{
	char *port;
	int fd;
} mypair; 

typedef struct SocketPair
{
	int* ipv4;
	int nv4, nv6;
	int* ipv6;
} SocketPair;

typedef struct Node
{
	char *str;
	struct Node *prev, *next;
} Node;


Node **head, **tail;
int *sz;

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

void startSock4(void *data)
{
	struct sockaddr_in serv_in4, cli_in4;
	int new_sfd;
	char *port = ((mypair*) data) -> port;
	int sfd = ((mypair*) data) -> fd;
	for (int j = 0; j < strlen(port); j++)
	{
			if ((port[j] < '0' || port[j] > '9'))
			{
				char *msg = "Error in arguments parsing\n";
				write(2, msg, strlen(msg));
				return;
			}
	}
	int num = atoi(port);
	serv_in4.sin_family = AF_INET;
	serv_in4.sin_addr.s_addr = INADDR_ANY;
	serv_in4.sin_port = htons(num);
	if (bind(sfd, (struct sockaddr *) &serv_in4, sizeof(serv_in4)) < 0)
	{
		char *msg = "Impossible to bind IPv4 socket and port\n";
		write(2, msg, strlen(msg));
		return;
	}
	else
	{
		write(1, port, strlen(port));
		char *msg = " port and IPv4 socket binded\n";
		write(1, msg, strlen(msg));
	}
	socklen_t socklen;
	socklen = sizeof(cli_in4);
	listen(sfd, 5);
	new_sfd = accept(sfd, (struct sock_addr*) &cli_in4, &socklen);
}


void startSock6(void *data)
{
	struct sockaddr_in6 serv_in6, cli_in6;
	int new_sfd;
	char *port = ((mypair*) data) -> port;
	int sfd = ((mypair*) data) -> fd;
	for (int j = 0; j < strlen(port); j++)
	{
			if ((port[j] < '0' || port[j] > '9'))
			{
				char *msg = "Error in arguments parsing\n";
				write(2, msg, strlen(msg));
				return;
			}
	}
	int num = atoi(port);
	bzero(&serv_in6, sizeof(serv_in6));
	serv_in6.sin6_family = AF_INET6;
	serv_in6.sin6_port = htons(num);
	setsockopt(sfd, IPPROTO_IPV6, IPV6_V6ONLY, "1", sizeof(int));
	if (bind(sfd, (struct sockaddr6 *) &serv_in6, sizeof(serv_in6)) < 0)
	{
		char *msg = "Impossible to bind IPv6 socket and port\n";
		write(2, msg, strlen(msg));
		return;
	}
	else
	{
		write(1, port, strlen(port));
		char *msg = " port and IPv6 socket binded\n";
		write(1, msg, strlen(msg));
	}
	socklen_t socklen;
	socklen = sizeof(cli_in6);
	listen(sfd, 5);
	new_sfd = accept(sfd, (struct sock_addr6*) &cli_in6, &socklen);
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
	head = (Node **)calloc(2*socketAmount, sizeof(Node*));
	tail = (Node **)calloc(2*socketAmount, sizeof(Node*));
	sz = (int *)calloc(2*socketAmount, sizeof(int));
	mypair* arg4 = (mypair *)calloc(socketAmount, sizeof(mypair));
	mypair* arg6 = (mypair *)calloc(socketAmount, sizeof(mypair));
	pthread_t *threads4 = (pthread_t*)calloc(socketAmount, sizeof(pthread_t));
	pthread_t *threads6 = (pthread_t*)calloc(socketAmount, sizeof(pthread_t));
	for (int i = 0; i < socketAmount; i++)
	{
		arg4[i].port = argv[i+1];
		arg6[i].port = argv[i+1];
		arg4[i].fd = socks.ipv4[i];
		arg6[i].fd = socks.ipv6[i];
	}
	for (int i = 0; i < socketAmount; i++)
	{
		pthread_create(&threads4[i], NULL, startSock4, (void*) &arg4[i]);
		pthread_create(&threads6[i], NULL, startSock6, (void*) &arg6[i]);
	}
	for (int i = 0; i < socketAmount; i++)
	{
		pthread_join(threads4[i], NULL);
		pthread_join(threads6[i], NULL);
	}				
	return 0;
}

