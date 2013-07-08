#include "stock.h"

Tbl *malloced_split ( s, c )
char *s ;
int c ;
{
    Tbl *tbl ;
    char *orig, *copy ;
    int i ;
 
    tbl = newtbl(5) ;
 
    while ( *s != 0 )
        {
        while ( *s == c ) s++ ;
        if ( *s != 0 )
            pushtbl ( tbl, s ) ;
        while ( *s != c && *s != 0 )
            s++ ;
        if ( *s == c ) *s++ = 0 ;
        }
    
    for( i = 0; i < maxtbl( tbl ) ; i++ )
    	{
	orig = gettbl( tbl, i ) ;
	copy = malloc( strlen( orig ) + 1 ) ;
	strcpy( copy, orig ) ;
	settbl( tbl, i, copy ) ;
    	}
 
    return tbl ;
}
