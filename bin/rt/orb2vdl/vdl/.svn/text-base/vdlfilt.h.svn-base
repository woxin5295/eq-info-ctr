#define MAX_FILT_STAGE 5
#define VDLFILT 1
struct decimate {
	int decimation;						/* the decimation factor */
	int nstage;							/* number of stages in filter */
	short stage[MAX_FILT_STAGE];		/* up to 5 stages */
	short nhist[MAX_FILT_STAGE];		/* space for history for this stage */
	long * hist;						/* pointer to total history space */
	int delay;							/* group delay in MS */
};
#ifdef __STDC__
struct decimate setfilt(int,int,int);
long decimate(struct decimate,long *);
#else
struct decimate setfilt();				/* prototype for setfilt */
long decimate();

#endif
