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
get_profile_(filename_in,prof_title,prof_coord,nprf,width,depth,angle,
		scale,vert_exag,lfilename,lcustom,lspecial) 
char 	*filename_in,*prof_title,*prof_coord;
int	*nprf;
float 	*width,*depth,*angle,*scale,*vert_exag;
long	lfilename,lcustom,lspecial;
{
	Pf *pf;
	Tbl *tbl;
	char *filename,*title,*files;
	int i,num_bp;	

	filename = (char *) malloc(strlen(filename_in) * sizeof(char));
	strcpy(filename,filename_in);
	TRIM(filename);

	title = (char *) malloc(((int) lcustom + 1) * sizeof(char));
	files = (char *) malloc(((int) lspecial + 1) * sizeof(char));

	if( pfread (filename,&pf) != 0) {
		fprintf(stderr,"pfread barfed for %s\n",filename);
                *nprf = 0;
                return;
	}
	*width = pfget_int (pf,"width");
	*depth = pfget_int (pf,"depth");
	*angle = pfget_int (pf,"angle");
	*scale = pfget_int (pf,"scale");
	*vert_exag = pfget_int (pf,"vert_exag");

	strcpy(title,pfget_string(pf,"title"));
	PAD(title,(int) lcustom);
	strcpy(prof_title,title);

	tbl = pfget_tbl(pf,"prof_coord");
	*nprf = maxtbl(tbl);
	strcpy(files,"");
	for(i=0;i<*nprf;i++) {
		strcat(files,gettbl(tbl,i));
		strcat(files," ");
	}
	PAD(files,(int) lspecial);
	strcpy(prof_coord,files);

	return;
}

