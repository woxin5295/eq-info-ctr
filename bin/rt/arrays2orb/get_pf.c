#include <stdlib.h>
#include "stock.h"
#include "pf.h"

Pf *
get_pf( char *block_name )
{
	static Arr *pfs = 0;
	Pf	*pf;

	if( pfs == 0 )
	{
		pfs = newarr( 0 );
		pfread( block_name, &pf );
		setarr( pfs, block_name, (void *) pf );
	}
	else if( ( pf = (Pf *) getarr( pfs, block_name ) ) != 0 )
	{
		/* We've got it already */ ;
	}
	else
	{
		pfread( block_name, &pf );
		setarr( pfs, block_name, (void *) pf );
	}

	return pf;
}

