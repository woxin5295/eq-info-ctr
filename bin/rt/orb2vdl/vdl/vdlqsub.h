/************************* Function Prototypes **************************/
#include <time.h>
#ifdef __STDC__
void i2swap(short i, void *swap);
void i4swap(long i, void *swap);
int yrday(struct nsntime tc, long *iyear, long *doy);
int nsnsub(struct nsntime tc1, struct nsntime tc2);
struct nsntime maknsn(int iy, int id, int ih, int im,int is, int ms, int leap);
struct nsntime nsnadd(struct nsntime tc, long msadd);
struct nsntime buftim(struct chan_desc *ch,int iss);
char * asctim();		/* asctim returns our standard system time as string*/
#else
char * asctim();		/* asctim returns our standard system time as string*/
void i2swap();			/* (short i, short *swap) swap bytes in I2 */
void i4swap();			/* (long i, long *swap) end for end byte swap i*4*/
int nsnint();			/* (struct nsntime tc,long *yr,*day,*hr,*min,*sec,*ms)
							Converts Time code to its integer parts. Return
							zero unless time code is somehow illegal */
struct nsntime nsnadd();/* (struct nsntime tc, long msadd)  Add MSADD to the
							time code and return the revised timecode.  MSADD
							can be negative.  */
struct nsntime maknsn();
#endif
