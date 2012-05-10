#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <netdb.h>
#include <poll.h>
#include <pthread.h>
#include <unistd.h>
#include <poll.h>

#define MSG_SIZE 21
#define QUE_SIZE 1001

int failed[1024];
int socketAmount;

pthread_mutex_t qumutex = PTHREAD_MUTEX_INITIALIZER;

char* get_reversed(char* arr, int first, int last)
{
  int i, sz = 0;
  char ans[MSG_SIZE+1];
  for (i = last-1; i >= first; i--)
  {
    ans[sz] = arr[i];
    sz++;
  }
  ans[sz] = arr[last];
  sz++;
  if (ans[sz-1] == 0)
	sz--;	
  return ans;
}

char* procBuffer(int read_status, int *sz, int *over, char *buf)
{
	*sz += read_status;
    int pos = 0;
    for (int i = 0; i < *sz; i++)
    {
      if (buf[i] == '\n')
      {
        if (! *over)
 		   return get_reversed(buf, pos, i); 
        else
          *over = 0;
        pos = i+1;
      }
	}

	if (*sz == MSG_SIZE + 1 && pos == 0)
	{
	  *over = 1;
	  *sz = 0;
	  for (int i = 0; i <= MSG_SIZE; i++)
		buf[i] = 0;
	}
	else
	{
		for (int i = pos; i < *sz; i++)
			buf[i - pos] = buf[i];
		*sz = *sz - pos;
		for (int i = *sz; i < MSG_SIZE + 1; i++)
			buf[i] = 0;
	}
    
	return NULL;
}


typedef struct mypair
{
	char *port;
	int fd;
	int id;
} mypair; 

typedef struct pairii
{
	int id;
	int fd;
} pairii;

typedef struct SocketPair
{
	int* ipv4;
	int nv4, nv6;
	int* ipv6;
} SocketPair;

typedef struct ListData
{
	char *str;
	int ref;
} ListData;

typedef struct Node
{
	ListData data;
	struct Node *prev;
} Node;


Node *head, *tail;
Node ***qu;
int *first, *last;

void pop()
{
	if (head == tail)
	{
		head = NULL;
		tail = NULL;
		return;
	}
	head = head -> prev;
	free(head);
}

Node* push(char *str)
{
	Node *new = (Node *)malloc(sizeof(Node));
	new -> data.str	= str;
	new -> data.ref = 2*socketAmount;
	new -> prev = NULL;
	if (head == NULL)
	{
		head = new;
		tail = new;
	}	
	else
	{
		tail -> prev = new;
		tail = new; 
	}
	return new;
}

void erase(int i)
{
	if (i >= 2*socketAmount)
	 return;
	qu[i][first[i]] -> data.ref--;
	if (qu[i][first[i]] -> data.ref <= 0)
		pop();			
	first[i] = (first[i] + 1) % QUE_SIZE;	
}

void addAll(Node *elem)
{
	for (int i = 0; i < 2*socketAmount; i++)
	{
		qu[i][last[i]] = elem;
		last[i] = (last[i] + 1) % QUE_SIZE;
		if (last[i] == first[i])
			pop(i);
	}
}

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

void *writefd(void *ptr)
{
	int id = ((pairii*) ptr )-> id;
	int fd = ((pairii*) ptr )-> fd;
	char *msg;
	struct pollfd pfd;
	bzero(&pfd, sizeof(pfd));
	pfd.fd = fd;
	pfd.events = POLLOUT;
	while (1)
	{
		while (first[id] == last[id])
			sleep(1);
		msg = qu[id][first[id]] -> data.str;
		int status;
		poll(&pfd, 1, -1);
		for (int i = 0; i < 5; i++)
		{
			
			status = send(fd, msg, strlen(msg), 0);
			if (status > 0)
				break;			
		}
		if (status == -1)
		{
			failed[id] = 1;
			return;
		}
		pthread_mutex_lock(&qumutex);
		erase(id);		
		pthread_mutex_unlock(&qumutex);
	}
}

void *readfd(void *ptr)
{
	int status=1;
	int fd = ((pairii*) ptr) -> fd;
	int id = ((pairii*) ptr) -> id;
	char word[MSG_SIZE+1];
	char *ans;
	struct pollfd pfd;
	bzero(&pfd, sizeof(pfd));
	pfd.fd = fd;
	pfd.events = POLLIN;
	int sz = 0, over = 0;
	while (1)
	{
		write(1, "!!", 2);
		poll(&pfd, 1, -1);
		write(1, "!!", 2);
		status = 0;
		if (pfd.revents & POLLIN)
			status = recv(fd, word, MSG_SIZE, 0);
		if (status < 0)
			return;
		write(1,"!!",2);
		if (status>0)
		{
			write(1,"!!",2);
			ans = procBuffer(status, &sz, &over, word);
			pthread_mutex_lock(&qumutex);
			Node *new = push(ans);
			addAll(new);		
			pthread_mutex_unlock(&qumutex);
		}
		if (failed[id])
		{
			return;
		}
	}
}

void startSock4(void *data)
{
	struct sockaddr_in serv_in4, cli_in4;
	int new_sfd;
	char *port = ((mypair*) data) -> port;
	int sfd = ((mypair*) data) -> fd;
	int id = ((mypair*) data) -> id;
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
	char ack[100];
	long ip = cli_in4.sin_addr.s_addr;
	strcpy(ack,"The client connect from IP: ");
	strcat(ack, inet_ntoa(cli_in4.sin_addr));
	strcat(ack, "\n");
	pthread_mutex_lock(&qumutex);
	Node *node = push(ack);
	addAll(node);		
	pthread_mutex_unlock(&qumutex);
	pthread_t writer, reader;
	pairii p;
    p.id = id;
	p.fd = new_sfd;
	pthread_create(&writer, NULL, writefd, (void*)&p);
	pthread_create(&reader, NULL, readfd, (void*)&p);
}


void startSock6(void *data)
{
	struct sockaddr_in6 serv_in6, cli_in6;
	int new_sfd;
	char *port = ((mypair*) data) -> port;
	int sfd = ((mypair*) data) -> fd;
	int id = ((mypair*) data) -> id;
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
	socketAmount = argc - 1;
	SocketPair socks;
	socks = createSockets(socketAmount, argv+1);
	first = (int *)calloc(2*socketAmount, sizeof(int));
	last = (int *)calloc(2*socketAmount, sizeof(int));
/*	for (int i = 0; i < 2*socketAmount; i++)
		last[i] = 1;*/
	qu = (Node***)calloc(2*socketAmount, sizeof(Node**));
	for (int i = 0; i < 2*socketAmount; i++)
	  qu[i] = (Node **) calloc(QUE_SIZE, sizeof(Node*));
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
		arg4[i].id = i;
		arg6[i].id = socketAmount + i;
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

