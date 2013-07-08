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
get_params_(filename_in,custom_title,special_files,nsf,istaplt,istnam,
		ipdeplt,ipdepth,iporid,icities,ipipe,iblue,ipmag,idcirc,
		ititl,ipumps,iflt,icol,itran,lfilename,lcustom,lspecial) 
char 	*filename_in,*custom_title,*special_files;
int	*nsf;
int 	*istaplt,*istnam,*ipdeplt,*ipdepth,*iporid,*icities,*ipipe,*iblue;
int 	*ipmag,*idcirc,*ititl,*ipumps,*iflt,*icol,*itran;
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
		exit(1);
	}
	*istaplt = pfget_int (pf,"istaplt");
	*istnam = pfget_int (pf,"istnam");
	*ipdeplt = pfget_int (pf,"ipdeplt");
	*ipdepth = pfget_int (pf,"ipdepth");
	*iporid = pfget_int (pf,"iporid");
	*icities = pfget_int (pf,"icities");
	*ipipe = pfget_int (pf,"ipipe");
	*iblue = pfget_int (pf,"iblue");
	*ipmag = pfget_int (pf,"ipmag");
	*idcirc = pfget_int (pf,"idcirc");
	*ititl = pfget_int (pf,"ititl");
	*ipumps = pfget_int (pf,"ipumps");
	*iflt = pfget_int (pf,"iflt");
	*icol = pfget_int (pf,"icol");
	*itran = pfget_int (pf,"itran");

	strcpy(title,pfget_string(pf,"title"));
	PAD(title,(int) lcustom);
	strcpy(custom_title,title);

	tbl = pfget_tbl(pf,"label_files");
	*nsf = maxtbl(tbl);
	strcpy(files,"");
	for(i=0;i<*nsf;i++) {
		strcat(files,gettbl(tbl,i));
		strcat(files,".label ");
	}
	tbl = pfget_tbl(pf,"boilerplate_files");
	num_bp= maxtbl(tbl);
	*nsf += num_bp;
	for(i=0;i<num_bp;i++) {
		strcat(files,gettbl(tbl,i));
		strcat(files,".bp ");
	}
	PAD(files,(int) lspecial);
	strcpy(special_files,files);

	return;
}

