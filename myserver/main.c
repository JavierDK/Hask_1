#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
#include <poll.h>

#define MSG_SIZE 21
#define QUE_SIZE 1001
#define MAX_FD 1000

struct addrinfo *hints;
struct addrinfo *clinfo;

int ofd[MAX_FD], ofdsz;
int nfd[MAX_FD], nfdsz;
struct pollfd action[MAX_FD];
int status[MAX_FD];


void bindAtPort(char *port)
{ 
	printf("There is port number %s\n", port);
	hints -> ai_family = AF_UNSPEC;
	hints -> ai_socktype = SOCK_STREAM;
	hints -> ai_flags = AI_PASSIVE;
	if (getaddrinfo(NULL, port, hints, &clinfo) !=0 )
	{
		char *msg = "Error: impossible to get address information!\n";
		write(2, msg, strlen(msg));
		exit(1);
	}
	struct addrinfo *it = clinfo;
	while (it != NULL)
	{ 
		int fd = socket(it -> ai_family, it -> ai_socktype | SOCK_NONBLOCK, it -> ai_protocol);
		if (fd < 0)
		{
			char *msg = "Error: impossible to create socket!\n";
			write(2, msg, strlen(msg));
			exit(1);
		}
		if (bind(fd, it -> ai_addr, it -> ai_addrlen) == 0)
		{
			printf("Binding socket %d at %s port\n", fd, port);
			if (ofdsz == MAX_FD)
			{
				char *msg = "Error: too much fd are in use!\n";
				write(2, msg, strlen(msg));
			}
			else
			{
				ofd[ofdsz++] = fd;
				listen(fd, 1);
			}
		}
		it = it -> ai_next;
	}
}

void serve()
{
	char *ip = calloc(50, sizeof(char));
	struct sockaddr *addr = malloc(sizeof(struct sockaddr));
	size_t addrlen = sizeof(struct sockaddr);
	for (int i = 0; i < ofdsz; i++)
	{
		int newfd = accept(ofd[i], addr, &addrlen);
		if (newfd >= 0)
		{
			if (nfdsz == MAX_FD)
			{
				char *msg = "Error: too much fd are in use!\n";
				write(2, msg, strlen(msg));
			}
			if (addr -> sa_family == AF_INET)
			{
				struct sockaddr_in *sin = addr;
				inet_ntop(AF_INET, &(sin->sin_addr), ip, 50);
			}
			else if (addr -> sa_family == AF_INET6)
			{
				struct sockaddr_in6 *sin = addr;
				inet_ntop(AF_INET6, &(sin->sin6_addr), ip, 50);
			}
			printf("Accepted with new fd %d\n", newfd);
		}
		nfd[nfdsz] = newfd;
		action[nfdsz].fd = newfd;
		action[nfdsz].events = POLLIN;
	}
}

int main(int argc, char **argv)
{
	hints = malloc(sizeof(struct addrinfo));
	clinfo = malloc(sizeof(struct addrinfo));
	if (argc == 1)
	{
		char *msg = "No socket to bind\n";
		write(2, msg, strlen(msg));
		return 1;
	}
	for (int i = 1; i < argc; i++)
		for (int j = 0; j < strlen(argv[i]); j++)
			if (argv[i][j] < '0' || argv[i][j] > '9')
			{
				printf("Error in socket parsing\n");
				return 1;
			}
	for (int i = 1; i < argc; i++)
		bindAtPort(argv[i]);
	while (1)
		serve();
	return 0;
}
