#include <stdio.h>
main()
{
	int nb,c;
	nb=0;
	while (( c=getchar()) != EOF) 
		if( c == '\n') ++nb;
	printf("%d lines.\n",nb);
}
