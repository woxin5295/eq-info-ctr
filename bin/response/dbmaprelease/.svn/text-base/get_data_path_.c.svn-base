#include "string.h"
#include "stock.h"
#include "pf.h"
 
#ifndef TRIM
#define TRIM(cptr)  { register char *ptr, *end;\
end = cptr; ptr = end + strlen(cptr) - 1;\
while(*ptr == ' ' && ptr >= end) ptr--;\
*(++ptr) = '\0'; }
#endif
 
#ifndef PAD
#define PAD(cptr,len) {register char *ptr, *end;\
end = cptr + (len); ptr = cptr + strlen(cptr);\
while(ptr < end) *(ptr++) = ' ';\
*ptr = '\0'; }
#endif

void
get_data_path_(basename, fullpath, lbase, lfull)
char	*basename,*fullpath ;
long	lbase, lfull ;
{
        Tbl *lbltbl;
        char *filename,*lblpath,path[80];
	FILE *file, *fopen();
        int i,n;

	(void) strcpy(path,"");

	/* cludge fix of segmentation fault Mitch 20060705*/
	for (i=0; i<80; ++i) {
	  if (basename[i] == ' ') { 
		basename[i] = '\0'; 
	  }
	}		
        filename = (char *) malloc(strlen(basename) * sizeof(char));
        lblpath = (char *) malloc(80 * sizeof(char));
        (void) strcpy(filename,basename);
        TRIM(filename);

    if ((char *)getenv("DBMAPEVENTS_DATA") == (char *)NULL) {
	(void) strcpy(lblpath, ".");
    } else
      {
	(void) strcpy(lblpath,(char *)getenv("DBMAPEVENTS_DATA"));
    }
    /* old crap that will cause segmentation fault Mitch 20060705 */
    /*if ( (lblpath = getenv ( "DBMAPEVENTS_DATA" ) ) == 0 ) {
    *    lblpath = "." ;
    *}
    */
    lblpath = strdup(lblpath) ;
    lbltbl = split ( lblpath, ':' ) ;
    n = maxtbl ( lbltbl ) ;
    for ( i=0 ; i<n ; i++ )
        {
        (void) strcpy ( path, gettbl(lbltbl, i ) );
        (void) strcat ( path, "/" ) ;
        (void) strcat ( path, filename ) ;
        file = fopen ( path, "r" ) ;
        if ( file != 0 )
            {
            fclose ( file ) ;
            break;
            }
        }
    freetbl ( lbltbl, 0 ) ;
    free(lblpath) ;
    free(filename);

    PAD(path,(int) lfull);
    (void) strcpy(fullpath,path);

    return ;
}
