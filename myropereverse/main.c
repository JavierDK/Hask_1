#include <stddef.h>

#define LEAF_SZ 3

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
		a -> right = concat (b, a -> right);
		return a;
	}
	else
	{
		b -> left = concat (a, b -> left);
		return b; 
	}
}

int main()
{
	while (1)
	{
		
	}
	return 0;
}
