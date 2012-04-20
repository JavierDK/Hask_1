#include <stddef.h>
//#include <stdio.h>

#define LEAF_SZ 3
#define STR_LEN 10
#define MAX_ROPE_SZ 100

typedef struct Rope {
	struct Rope *left, *right;
	char key[LEAF_SZ];
	int amount;
	int prior;
} Rope;

Rope* concat(Rope *a, Rope *b)
{
	if (a == NULL)
		return b;
	if (b == NULL)
		return a;
	if (a -> prior < b -> prior)
	{
		a -> amount += b -> amount;
		a -> right = concat (b, a -> right);
		return a;
	}
	else
	{
		b -> amount += a -> amount;
		b -> left = concat (a, b -> left);
		return b; 
	}
}

void printReverse(Rope *x)
{
	if (x == NULL)
		return;
	printReverse(x -> right);
	for (int i = LEAF_SZ - 1; i >=0; i--)
		if (x -> key[i] != 0)
			write(1, (x -> key) + i, 1);
	printReverse(x -> left);
}

Rope* makeRope(char* str, int sz)
{
	Rope *node = calloc(1, sizeof(Rope));
	if (node == NULL)
		return NULL;
	if (sz > LEAF_SZ)
		return NULL;
	for (int i = 0; i < sz; i++)
		node -> key[i] = str[i];
	node -> prior = rand();
	node -> left = NULL;
	node -> right = NULL;
	return node;
}

void freeRope(Rope *node)
{
	if (node)
	{
		freeRope(node -> left);
		freeRope(node -> right);
		free(node);
	}
}

int main()
{
	char in[LEAF_SZ];
	Rope *root;
	while (1)
	{
		int status = read(0, in, LEAF_SZ);
		if (status == 0)
		{
			if (root -> amount < STR_LEN)
				printReverse(root);
			freeRope(root);
			return 0;
		}
		int sz = status;
		for (int i = 0; i < sz; i++)
		{
			if (in[i] == '\n')
			{
				char head[LEAF_SZ];
				memcpy(head, in, (size_t)i*sizeof(char));				
				memmove(head, in, (sz - (i+1))*sizeof(char));
				Rope* node = makeRope(head, i);
				printReverse(concat(root, node));
				write(1,"\n",1);
				sz = sz - (i + 1);
				freeRope(root);
				i = 0;
			}
		}
		concat(root, makeRope(in, sz));
	}
	return 0;
}
