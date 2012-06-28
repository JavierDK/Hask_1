#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/mman.h>

#define THRESHOLD 64
#define MASK 32


typedef struct BucketNode
{
	size_t size;
	void* addr;
	struct BucketNode *next, *prev;
} BucketNode;

typedef struct Queue
{
	BucketNode *first, *last;
} Queue;

typedef struct ThreadMem
{
	pid_t tid;
	BucketNode *list;
	struct ThreadMem* next;
} ThreadMem;

Queue smallB, largeB;
ThreadMem *allocMem;

BucketNode* createBN()
{
		BucketNode* res = (BucketNode*) mmap(NULL, sizeof(BucketNode), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
		return res;		
}

ThreadMem* createTM()
{
		ThreadMem* res = (ThreadMem*) mmap(NULL, sizeof(ThreadMem), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
		return res;		
}

ThreadMem* threadFind(pid_t iam)
{
	ThreadMem* posT, *prevT = NULL;
	posT = allocMem;
	while (posT != NULL)
	{
		if (posT -> tid == iam)
			break;
		prevT = posT;
		posT = posT -> next;		
	}
	if (posT = NULL)
	{
		ThreadMem *new = createTM();
		new -> tid = iam;
		new -> list = NULL;
		new -> next = NULL; 
		if (prevT == NULL)
			allocMem = new;
		else
			prevT -> next = new;
		return new;
	}
	else
		return posT;
}

void* getSmall(int size)
{
	pid_t iam = gettid();
	ThreadMem *pos = threadFind(iam);
	return NULL;
}

void* memalign(size_t a, size_t b)
{
   raise(SIGILL);
}

void* malloc(size_t size)
{
	if (size == 0)
		return NULL;
	if (size <= THRESHOLD)
	  return getSmall(size);
//	return getLarge(size);	
	void *p;
	return p;
}

void free(void *ptr)
{
	return;
}

int main()
{
	printf("%d", getpagesize());
	return 0;	
}
