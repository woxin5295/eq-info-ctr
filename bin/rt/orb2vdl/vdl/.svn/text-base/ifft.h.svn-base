/************************ Funtion Prototypes IFFT.c ********************/
#if __STDC__
void renorm(int n1, int n2, long im, long mx,int *me, long ia[]);
void ifft128(int *ie, long ia[], long ir[]);
void ifft256(int *ie, long ia[], long ir[]);
void ifftst128();
void ifftst256();
int kshft(long ia, int ib);
int mxabs( long im, long ia);

#else

void renorm();			/* (int n1,n2, long im,mx, int *me, long ia[]) used
							internally to normalize IA by powers of two such that
							the max in IA is < MX. IM is maximum before shift.*/
void ifft128();			/* (int *ie, long ia[],ir[]) compute transform of IA
							in place and power spectal window in IR.  IE is 
							power normalizing constant as a power of 2. */
void ifftst128();		/* (void) called to compute FFT cosines and initialize
							global FFT variables */
void ifft256();			/* (int *ie, long ia[],ir[]) compute transform of IA
							in place and power spectal window in IR.  IE is 
							power normalizing constant as a power of 2. */
void ifftst256();		/* (void) called to compute FFT cosines and initialize
							global FFT variables */
int mxabs();			/* (long im,ia) MXABS returns max of IM and IABS(IA) */
int kshft();			/* (long ia,int ib) Arithmetic shift of IA by IB bits
							left shift is positive, right negative) */

#endif

