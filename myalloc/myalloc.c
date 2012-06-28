#include <string.h>
#include <pthread.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/mman.h>

#define THRESHOLD 64
#define BIG_THREAD_MEM 5
#define SMALL_THREAD_MEM 20
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
	Queue alloc;
	BucketNode small[SMALL_THREAD_MEM], large[BIG_THREAD_MEM];
	struct ThreadMem* next;
} ThreadMem;

Queue smallB, largeB;
ThreadMem *allocMem;
pthread_mutex_t threadListMutex;

BucketNode* createBN()
{
		BucketNode* res = (BucketNode*) mmap(NULL, sizeof(BucketNode), PROT_READ | PROT_WRITE, MAP_PRIVATE, -1, 0);
		return res;		
}

ThreadMem* createTM()
{
		ThreadMem* res = (ThreadMem*) mmap(NULL, sizeof(ThreadMem), PROT_READ | PROT_WRITE, MAP_PRIVATE, -1, 0);
		return res;		
}

ThreadMem* threadFind(pid_t iam)
{
	ThreadMem* posT, *prevT = NULL;
	pthread_mutex_lock(&threadListMutex); 
	posT = allocMem;
	while (posT != NULL)
	{
		if (posT -> tid == iam)
			break;
		prevT = posT;
		posT = posT -> next;		
	}
	pthread_mutex_unlock(&threadListMutex); 
	if (posT = NULL)
	{
		ThreadMem *new = createTM();
		new -> tid = iam;
		new -> alloc.first = NULL;
		new -> alloc.last = NULL;
		new -> next = NULL;
		for (int i = 0; i < SMALL_THREAD_MEM; i++)
			new -> small[i].addr = NULL;			
		for (int i = 0; i < BIG_THREAD_MEM; i++)
			new -> large[i].addr = NULL;		
		pthread_mutex_lock(&threadListMutex); 
		if (prevT == NULL)
			allocMem = new;
		else
			prevT -> next = new;
		pthread_mutex_unlock(&threadListMutex); 
		return new;
	}
	else
		return posT;
}

void* getSmall(int size)
{
	pid_t iam = pthread_self();
	ThreadMem *pos = threadFind(iam);
	for (int i = 0; i < SMALL_THREAD_MEM; i++)
		if (pos -> small[i].addr != NULL && pos -> small[i].size >= size)
		{
			BucketNode *new = createBN();
			new -> addr = pos -> small[i].addr;
			new -> size = pos -> small[i].size;
			new -> next = NULL;
			new -> prev = NULL;
			if (pos -> alloc.first == NULL)
			{
				pos -> alloc.first = new;
				pos -> alloc.last = new;
			}
			else
			{
				pos -> alloc.last -> next = new;
				new -> prev = pos -> alloc.last;
				pos -> alloc.last = new;
			}
			return new -> addr;			
		}
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

void _init()
{
	pthread_mutex_init(&threadListMutex, 0);
}

void _fini()
{
	pthread_mutex_destroy(&threadListMutex);
}

int main()
{
	printf("%d", getpagesize());
	return 0;	
}
