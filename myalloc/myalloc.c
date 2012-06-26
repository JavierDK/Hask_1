#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>

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
} ThreadMem;

Queue smallB, largeB;
ThreadMem *allocMem;

void* getSmall(int size)
{
	pid_t iam = gettid();
	if (allocMem)
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
