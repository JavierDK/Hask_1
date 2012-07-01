#include <string.h>
#include <pthread.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/mman.h>

#define THRESHOLD 8
#define BIG_THREAD_MEM 5
#define SMALL_THREAD_MEM 20
#define MASK 32


typedef struct BucketNode
{
	size_t size;
	void* addr;
	struct BucketNode *next, *prev;
	int small;
} BucketNode;

typedef struct Queue
{
	BucketNode *first, *last;
} Queue;

typedef struct ThreadMem
{
	pid_t tid;
	Queue alloc;
	BucketNode *small[SMALL_THREAD_MEM], *large[BIG_THREAD_MEM];
	struct ThreadMem* next;
} ThreadMem;

Queue smallB, largeB;
ThreadMem *allocMem;
pthread_mutex_t threadListMutex, smallBucketMutex, largeBucketMutex;

BucketNode* createBN()
{
		BucketNode* res = (BucketNode*) mmap(NULL, sizeof(BucketNode), PROT_READ | PROT_WRITE, MAP_PRIVATE, -1, 0);
		return res;		
}

ThreadMem* createTM()
{
		ThreadMem* res = (ThreadMem*) mmap(NULL, sizeof(ThreadMem), PROT_READ | PROT_WRITE, MAP_PRIVATE, -1, 0);
		for (int i = 0; i < SMALL_THREAD_MEM; i++)
			res -> small[i] = NULL;
		for (int i = 0; i < BIG_THREAD_MEM; i++)
			res -> large[i] = NULL;
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
			new -> small[i] -> addr = NULL;			
		for (int i = 0; i < BIG_THREAD_MEM; i++)
			new -> large[i] -> addr = NULL;		
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

void addThreadMem(BucketNode *new, ThreadMem *pos)
{
	if (pos -> alloc.first == NULL)
	{
		pos -> alloc.first = new;
		pos -> alloc.last = new;
		pos -> alloc. first -> next = NULL;
		pos -> alloc. first -> prev = NULL;
	}
	else
	{
		pos -> alloc.last -> next = new;
		new -> prev = pos -> alloc.last;
		pos -> alloc.last = new;
	}
}

void* getSmall(int size)
{
	pid_t iam = pthread_self();
	ThreadMem *pos = threadFind(iam);
	for (int i = 0; i < SMALL_THREAD_MEM; i++)
		if (pos -> small[i] != NULL && pos -> small[i] -> size >= size)
		{
			BucketNode *new = createBN();
			new -> addr = pos -> small[i] -> addr;
			new -> size = pos -> small[i] -> size;
			new -> next = NULL;
			new -> prev = NULL;
			new -> small = 1;
			addThreadMem(new, pos);
			return new -> addr;			
		}
	BucketNode *it;
	pthread_mutex_lock(&smallBucketMutex);
	for (it = smallB.first; it != NULL; it = it -> next)	
		if (it -> size >= size)
		{
			if (it -> next)
				it -> next -> prev = it -> prev;
			if (it -> prev)
				it -> prev -> next = it -> next;
			if (it -> prev == NULL)
				smallB.first = it -> next;
			if (it -> next == NULL)
				smallB.last = it -> prev;
			munmap((void*) it, sizeof(BucketNode));
			BucketNode *new = createBN();
			new -> addr = it -> addr;
			new -> size = it -> size;
			new -> next = NULL;
			new -> prev = NULL;
			new -> small = 1;
			addThreadMem(new, pos);
			return it -> addr;
		}
	pthread_mutex_unlock(&smallBucketMutex);
	void* new_addr = mmap(0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE, -1, 0);
	BucketNode *new = createBN();
	new -> addr = new_addr;
	int adds = size;
	while (adds >= 0)
		size -= getpagesize();
	new -> size = size - adds;
	new -> next = NULL;
	new -> prev = NULL;
	new -> small = 1;
	addThreadMem(new, pos);
	return new_addr;
}

void* getLarge(int size)
{
	pid_t iam = pthread_self();
	ThreadMem *pos = threadFind(iam);
	for (int i = 0; i < BIG_THREAD_MEM; i++)
		if (pos -> large[i] != NULL && pos -> large[i] -> size >= size)
		{
			BucketNode *new = createBN();
			new -> addr = pos -> large[i] -> addr;
			new -> size = pos -> large[i] -> size;
			new -> next = NULL;
			new -> prev = NULL;
			new -> small = 0;
			addThreadMem(new, pos);
			return new -> addr;			
		}
	BucketNode *it;
	pthread_mutex_lock(&largeBucketMutex);
	for (it = largeB.first; it != NULL; it = it -> next)	
		if (it -> size >= size)
		{
			if (it -> next)
				it -> next -> prev = it -> prev;
			if (it -> prev)
				it -> prev -> next = it -> next;
			if (it -> prev == NULL)
				largeB.first = it -> next;
			if (it -> next == NULL)
				largeB.last = it -> prev;
			munmap((void*) it, sizeof(BucketNode));
			BucketNode *new = createBN();
			new -> addr = it -> addr;
			new -> size = it -> size;
			new -> next = NULL;
			new -> prev = NULL;
			new -> small = 0;
			addThreadMem(new, pos);
			return it -> addr;
		}
	pthread_mutex_unlock(&largeBucketMutex);
	void* new_addr = mmap(0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE, -1, 0);
	BucketNode *new = createBN();
	new -> addr = new_addr;
	int adds = size;
	while (adds >= 0)
		adds -= getpagesize();
	new -> size = size - adds;
	new -> next = NULL;
	new -> prev = NULL;
	new -> small = 0;
	addThreadMem(new, pos);
	return new_addr;
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
	return getLarge(size);
}

void* calloc(size_t num, size_t sz)
{
	void *res = malloc(num * sz);
	return memset(res, 0, num*sz);
}

void insertFreeMem(BucketNode *node, BucketNode **memArr, int sz, Queue *bucket)
{
	for (int i = 0; i < sz; i++)
		if (memArr[i] != NULL)
		{
			node -> prev = NULL;
			node -> next = NULL;
			memArr[i] = node;
			return;
		}
	if (node -> small)
		pthread_mutex_lock(&smallBucketMutex);
	else
		pthread_mutex_lock(&largeBucketMutex);
	if (bucket -> first == NULL)
	{
		node -> prev = NULL;
		node -> next = NULL;
		bucket -> first = node;
		bucket -> last = node;
	}
	else
	{
		bucket -> last -> next = node;
		node -> prev = bucket -> last;
		bucket -> last = node;
	}
	for (int i = 0; i < sz/2; i++)
	{
		bucket -> last -> next = memArr[i];
		memArr[i] -> prev = bucket -> last;
		bucket -> last = memArr[i];
		memArr[i] = NULL;
	}
	if (node -> small)
		pthread_mutex_lock(&smallBucketMutex);
	else
		pthread_mutex_lock(&largeBucketMutex);
}

void free(void *ptr)
{
	ThreadMem *pos = threadFind((pid_t)pthread_self);
	BucketNode *node;
	for (BucketNode* it = pos -> alloc. first; it != NULL; it = it -> next)
		if (ptr == it -> addr)
		{
			node = it;
			break;
		}
	if (node -> prev)
		node -> prev -> next = node -> next;
	else
		pos -> alloc. first = node -> next;
	if (node -> next)
		node -> next -> prev = node -> prev;
	else
		pos -> alloc. last = node -> prev;
	if (node -> small)
		insertFreeMem(node, pos -> small, SMALL_THREAD_MEM, &smallB);		
	else
		insertFreeMem(node, pos -> large, BIG_THREAD_MEM, &largeB);
}

void* realloc(void *ptr, size_t sz)
{
	if (ptr == NULL)
		return malloc(sz);
	ThreadMem *pos = threadFind((pid_t)pthread_self);
	BucketNode *node;
	for (BucketNode* it = pos -> alloc. first; it != NULL; it = it -> next)
		if (ptr == it -> addr)
		{
			node = it;
			break;
		}
	size_t oldSz = node -> size;
	void *new = malloc(sz);
	memcpy(new, ptr, sz < oldSz? sz : oldSz);
	free(ptr);
	return new;
} 

void _init()
{
	pthread_mutex_init(&threadListMutex, 0);
	pthread_mutex_init(&smallBucketMutex, 0);
	pthread_mutex_init(&largeBucketMutex, 0);
}

void _fini()
{
	pthread_mutex_destroy(&threadListMutex);
	pthread_mutex_destroy(&smallBucketMutex);
	pthread_mutex_destroy(&largeBucketMutex);
}

int main()
{
	printf("%d", getpagesize());
	return 0;	
}
