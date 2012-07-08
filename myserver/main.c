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

int ofd[MAX_FD], ofdsz;
int nfd[MAX_FD], nfdsz;


void bindAtPort(char *port)
{ 
	struct addrinfo hints;
	struct addrinfo *clinfo;
	printf("There is port number %s\n", port);
	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_PASSIVE;
	if (getaddrinfo(NULL, port, &hints, &clinfo) !=0 )
	{
		char *msg = "Error: impossible to get address information!\n";
		write(2, msg, strlen(msg));
		exit(1);
	}
	
}

int main(int argc, char **argv)
{
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
	return 0;
}
