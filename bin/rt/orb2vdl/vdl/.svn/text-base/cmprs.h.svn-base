#define NBK	2017		/* Buffer length of compression blocks */
/***************** struct CMPRFR *****************************/
/*  This is the structure of the memory in the user area used by CMPRS to
maintain variable for each channel is use.  This structure and the ICMP buffer
are the user defined areas where compression takes place.*/

struct cmprfr
{
   long id0;            /* data value to difference against */
   long ia0;            /* forward integration constant */
   long ian;            /* backward integration constant */
   long iv;             /* data field absolute maximum data value */
   long lsav[28];       /* data first difference look ahead buffer */
   long iasav[3];       /* potential forward integration constants */
   short ifr;           /* frame count */
   short ipt;           /* record byte pointer */
   short nct;           /* block byte count */
   short npt;           /* record sample count */
   short kpt;           /* frame sample count */
   short isav;          /* high water mark of lsav */
   short is;            /* frame data start pointer (into lsav) */
   short ie;            /* frame data end pointer (into lsav) */
   short isub;          /* data field pointer (into layout) */
   short layout[2][3];  /* data field keys and data pointers */
   short dummy;
};
/************************* Function Prototypes for cmprs.c **************/
#ifdef __STDC__
  void cmprs(struct cmprfr *memex, short n, long idat[],short *last, long icmp[]);
  void cmfin(struct cmprfr *, long []);
  void cmset(struct cmprfr *memex, long icmp[]); 
  void prtial(struct cmprfr *memex, unsigned char icmp[]);
  void kdif(short i, long idat);
  int kbits(void);
  void pack(short *ibas, long icmp[]);
  void bufout(short last, long icmp[]);
  void snible(char ib[],long ia[], short *ns, short nb, short n);
  void putbuf(short npt, unsigned char *icmp,short last, short ipt, int partial);

#else
  void cmprs();			/* (struct cmprfr *memex,short n, long idat[], 
							short *last,long icmp[]) CMPRS adds N points of 
							timesires in IDAT to compression from in CMP.  
							LAST is a flag from the user saying its time to
							wrap up the series.  It is returned as zero if
							this call filled the last frame. MEMEX is where
							we keep various things needed for each channel */
  void cmfin();			/* (struct cmprfr *memex,long icmp[]) Causes the 
							compression frame to be forced out and zero padded
							if necessary */
  void cmset();			/* (struct cmprfr *memex, long icmp[]) Initialize 
							variables for channel.  Must be called once per 
							channel */
 void kdif();			/* (short i, long idat) used internall to compute 
							and maintain differences */
  int kbits();			/* (void) from global variables compute number of
							bits to use. Used internally by compression */
  void pack();			/* (short *ibas, long icmp[]) used internally to pack
							frames blocks and maintain keys */
  void bufout();			/* (short last, long icmp[]) used internally to pass
							completed compression packet to user after doing
							end of buffer processing.  User routine is PUTBUF*/
  void snible();			/* (char ib[],long ia[],short *ns,nb,n) puts N nibbles
							of length NB into array IB beginning at byte IB(NS).
							NS is updated to pint to next unprocessed byte in
							IB.  Even length nibbles up to 32 bits except 
							30 bits work fine. */
  void putbuf();			/* (short npt, long icmp[],short last) user supplied
							routine called as each compression buffer is 
							filled.  Last will be the state of the last flag
							at the time of calling */
  void prtial();			/* (struct cmprfr *memex,long icmp[]) Causes the 
							compression frame to be updated via putbuf with
							parial flag set true.  Used for LP data to get it
							out sooner and build complete records in Golden*/
#endif

