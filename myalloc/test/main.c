#include <stdio.h>
#include <stdlib.h>

int main()
{
	int *a = malloc(sizeof(int)), *b = malloc(sizeof(int));
	scanf("%d%d", a, b);
	int *c = malloc(sizeof(int));
	*c = *a + *b;
	printf("%d\n", *c);
	free(a);
	free(b);
	free(c);
	return 0;
}
