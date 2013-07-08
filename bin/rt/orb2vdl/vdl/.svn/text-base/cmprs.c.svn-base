/* NBK is the compression block length and is defined in CMPRS.H */
#include <stdio.h>
#include "cmprs.h"		/* function prototypes and structs for this module */
#define   NST       6
#define   NEN   NBK-1
#define   NTR   NEN-5
#define   NTR1  NTR+1
#define   NBKL  (NBK+3)/4

static struct cmprfr mem;	/*Temp place to put a channels variables.  These
								are copied in and out at beginning and end of
								call.*/

static short nib[3][16] = {4, 4, 4, 6, 6, 8, 8,10,10,12,14,16,20,24,28,32,
                           4, 8,12, 4, 8, 4, 8, 4, 8, 4, 4, 4, 4, 4, 4, 4,
                           2, 4, 6, 3, 6, 4, 8, 5,10, 6, 7, 8,10,12,14,16};

static long mask[16] = {0x00000003,0x0000000F,0x0000003F,0x000000FF,
                        0x000003FF,0x00000FFF,0x00003FFF,0x0000FFFF,
                        0x0003FFFF,0x000FFFFF,0x003FFFFF,0x00FFFFFF,
                        0x03FFFFFF,0x0FFFFFFF,0x3FFFFFFF,-1};
/***************************************************************************/
/*
   Cmprs adds the n points in array idat[n] to the current compression 
   series being generated.  Subsequent buffers are added until the 
   series is closed out by calling subroutine cmfin.  last if > 0 says
   compression will only continue until next block is full.  This routines
   clears last when the full block is submitted to BUFOUT and user routine
   PUTBUF to notify caller that compression segment is complete.
*/
#ifdef __STDC__
  void cmprs(struct cmprfr *memex,short n,long idat[],short *last,long icmp[])
#else
  void cmprs(memex,n,idat,last,icmp)
  struct cmprfr *memex;
  short n,*last;
  long idat[],icmp[];
#endif
{
   short j,i,ibas,nend,kb,jb,lb,jen;

   if(n <= 0) return;
/* Remember all of the common variables needed to control this channel. */
   mem = *memex;

/* Do some initialization every call. */
   i = mem.isav;                        /* relative data pointer */
   ibas = -i-1;                         /* base data pointer */
   if(*last == 0) nend = NEN;           /* record end pointer */
   else           nend = NTR;

/* Do more initialization on the first call. */
   if(mem.isub <= 0)
   {
      if(mem.layout[1][0] >= 0)
      {
         mem.id0 = idat[0];             /* previous data value */
         mem.ia0 = mem.id0;             /* forward integration constant */
         ibas = ibas+1;                 /* base pointer */
         mem.layout[1][0] = -1;         /* reset flag */
      }
      jb = -1;                          /* last data field key value */
   }
   else
      jb = mem.layout[1][mem.isub-1];

   for(;;)
   {
/* Form, buffer, and max the first differences. */
      do
      {
         if((++i)+ibas >= n) goto exit;
         kdif(i,idat[i+ibas]);
      } while(i < mem.ie);

/* Get the key value for the last four first differences. */
      kb = kbits();

/* Add the new data field information.  Is+1 points to the start of the 
 differences in lsav and kb is the key value. */
      mem.layout[0][mem.isub] = mem.is+1;
      mem.layout[1][mem.isub] = kb;
 
/* See if the conditions are right for merging this data field with the 
 previous one. */
      if(jb >= 0)
         if(nib[0][kb] == nib[0][jb] && jb < 15)	/* update line 2*/
            if(nib[0][jb+1] == nib[0][jb])
            {
               mem.isub = mem.isub-1;
               kb = jb+1;
               mem.layout[1][mem.isub] = kb;
            }
      jb = kb;

/* Save potential forward integration constants. */
      if(mem.isub == 1) mem.iasav[2] = idat[i+ibas];

/* We have enough data to compress. */
      if(mem.isub >= 2)
      {
/* Push down the potential forward integration constants. */
         mem.iasav[0] = mem.iasav[1];
         mem.iasav[1] = mem.iasav[2];
/* Project how many bytes the next current frame will take. */
         lb = mem.layout[1][0];
         jen = nib[2][lb];
         lb = mem.layout[1][1];
         jen = jen+nib[2][lb];

/* Wrap up the buffer. */
        if(mem.ipt+jen >= nend)
         {
/* If last is set when the output buffer is already too full, force the
	current record out and fill up one more. */
			if(*last > 0 && mem.ipt >=NTR1)
			{
				bufout((short) 0,icmp);
				mem.ia0 = mem.iasav[0];
			}
/* otherwise, process the buffer normally. */
			else
			{
	            if(*last > 0) mem.ian = mem.iasav[0];
        	    bufout(*last,icmp);
	            mem.ia0 = mem.iasav[0];
        	    if(*last > 0)
	            {
        	       cmset(memex,icmp);
	               *last = 0;
        	       return;
	            }
	        }
         }
 
/* Wrap up the frame. */
         mem.ifr = mem.ifr+1;
         pack(&ibas,icmp);

/* Set up a new frame. */
         i = 3;
         mem.isub = 0;
         mem.ie = i;
      }

/* Set up a new sub-frame. */
      mem.isub = mem.isub+1;
      mem.is = mem.ie;
      mem.ie = mem.is+4;
   }

/* Out of data.  Set up for the next buffer. */
exit:
   mem.ian = idat[--i+ibas];

/* Remember the temporary variables needed for this channel. */
   *memex = mem;
   return;
}

/*
   Cmfin closes out a time series by forcing all currently buffered 
   data into the output record buffer and tacking on the trailer 
   information.  Care must be taken to pad out the last frame with 
   zero differences.
*/
#ifdef __STDC__
  void cmfin(struct cmprfr *memex,long icmp[])
#else
  void cmfin(memex,icmp)
  struct cmprfr *memex;
  long icmp[];
#endif
{
   short ibas,jb,kb,left,i1,j,lb,jen;

/* Get the remembered variables back. */
   mem = *memex;

/* Do some initialization. */
	if(mem.isub <= 0) return;
   ibas = -mem.isav-1;
   jb = mem.layout[1][mem.isub-1];

   if(mem.isav > mem.is)
   {
/* Figure the key value on what we've got. */
      kb = kbits();

/* Add the new data field information. */
      mem.layout[0][mem.isub] = mem.is+1;
      mem.layout[1][mem.isub] = kb;
   
/* If we have three data fields, see if the last can be merged with the 
 previous one.  If we have two data fields, this would be counter productive 
 as we need two data fields to close out the series. */
      if(mem.isub > 1)
         if(jb >= 0)
            if(nib[0][kb] == nib[0][jb])
               if(nib[0][jb+1] == nib[0][jb])
               {
                  mem.isub = mem.isub-1;
                  kb = jb+1;
                  mem.layout[1][mem.isub] = kb;
                  mem.is = mem.layout[0][mem.isub]-1;
               }
      jb = kb;
      left = mem.ie-mem.isav;
   }
   else
/* If we just filled up the last data field, we are all set. */
   {
      mem.isub = mem.isub-1;
      mem.is = mem.layout[0][mem.isub]-1;
   }

   for(;;)
   {
/* If we have one data field left, sometimes we can break into two data 
 fields and avoid padding with zeros. */
      if(mem.isub < 1)
         if(nib[1][jb] > 4)
         {
/* Yes, we can do it.  Jimmy the key and ... */
            jb  =  jb-1;
            mem.layout[1][mem.isub] = jb;
/* ... add the new data field. */
            mem.isub = mem.isub+1;
            mem.is = mem.is+nib[1][jb];
            mem.layout[0][mem.isub] = mem.is+1;
            if(nib[1][jb] > 4) jb = jb-1;
            mem.layout[1][mem.isub] = jb;
         }
         else
         {
/* If we still have less than two data fields left, we will have to pad 
 with a dummy data field to complete the last logical record. */
            mem.isub = mem.isub+1;
            mem.is = mem.is+nib[1][jb];
            mem.ie = mem.is+4;
/* Dummy data fields are always kept as short as possible. */
            mem.layout[0][mem.isub] = mem.is+1;
            mem.layout[1][mem.isub] = 0;
            jb = 1;
            left = left+4;
         }

/* Keep track of the forward integration constants just in case. */
      mem.iasav[0] = mem.iasav[1];
      mem.iasav[1] = mem.iasav[2];

/* Zero out the first difference buffer for padded values. */
      if(mem.isav < mem.ie)
      {
         i1 = mem.isav+1;
         for(j = i1; j <= mem.ie; j++) mem.lsav[j] = 0;
      }

/* Project the length of the current frame. */
      lb = mem.layout[1][0];
      jen = nib[2][lb];
      mem.is = mem.is-nib[1][lb];
      lb = mem.layout[1][1];
      jen = jen+nib[2][lb];
      mem.is = mem.is-nib[1][lb];

/* If it won't fit, we will have to write the current logical record and 
 start another one for the dregs we have left. */
      if(mem.ipt+jen >= NTR)
      {
         bufout((short) 0,icmp);
         mem.ia0 = mem.iasav[0];
      }

/* Pack the current frame. */
      mem.ifr = mem.ifr+1;
      pack(&ibas,icmp);
      mem.isub = mem.isub-2;

/* If that's all folks, write the last logical record.  Otherwise, go back 
 to see if more padding needs to be done. */
      if(mem.isub < 0) break;
   }

/* Wrap it. */
   bufout((short) 1,icmp);
   cmset(memex,icmp);
   return;
}
/**************************************************************************/
/*
   Initialize all relevant variables.  Note that since cmset is 
   effectively called by, it is only necessary to call cmset once 
   per channel.  However, if you choose to call cmset each time you 
   begin a new trigger, it will cause no problem.
*/
#ifdef __STDC__
  void cmset(struct cmprfr *memex, long icmp[])
#else
  void cmset(memex,icmp)
  struct cmprfr *memex;
  long icmp[];
#endif
{
   short j;

   memex->iv = 0;               /* data field absolute maximum data value */
   memex->ifr = 0;              /* frame counter */
   memex->ipt = NST;            /* record byte pointer */
   memex->nct = memex->ipt-1;   /* block byte counter */
   memex->npt = 0;              /* record sample counter */
   memex->kpt = 0;              /* frame sample counter */
   memex->isav = -1;            /* lsav pointer */
   memex->is = -1;              /* data field data sample start pointer */
   memex->ie = 3;               /* data field data sample end pointer */
   memex->isub = 0;             /* data field pointer */
   memex->layout[1][0] = 0;     /* initialize last key value */

/* Pre-zero the compression buffer. */
   for(j = 0; j < NBKL; j++) icmp[j] = 0;
   return;
}
/************************************************************************/
#ifdef __STDC__
  void kdif(short i, long idat)
#else
  void kdif(i,idat)
  short i;
  long idat;
#endif
{
   long ifd;

/* Set up the first difference look ahead. */
   mem.isav = i;
   ifd = idat-mem.id0;
   mem.lsav[mem.isav] = ifd;
   mem.id0 = idat;

/* Find the absolute maximum (compensating for the 2's complement. */
   ifd = (ifd >= 0)?ifd:-ifd-1;
   mem.iv = (mem.iv >= ifd)?mem.iv:ifd;
   return;
}
/*************************************************************************/
#ifdef __STDC__
  int kbits(void)
#else
  int kbits()
#endif
{
   short ibits;

   if(mem.iv < 8192)            /* lower half of range */
      if(mem.iv < 128)
         if(mem.iv < 32)
            if(mem.iv < 8)
               ibits = 0;               /* 4 bit nibbles */
            else
               ibits = 3;               /* 6 bit nibbles */
         else
            ibits = 5;                  /* 8 bit nibbles */
      else
         if(mem.iv < 2048)
            if(mem.iv < 512)
               ibits = 7;               /* 10 bit nibbles */
            else
               ibits = 9;               /* 12 bit nibbles */
         else
            ibits = 10;                 /* 14 bit nibbles */
   else
      if(mem.iv < 524288)       /* upper half of range */
         if(mem.iv < 32768)
            ibits = 11;                 /* 16 bit nibbles */
         else
            ibits = 12;                 /* 20 bit nibbles */
      else
         if(mem.iv < 8388608) 
            ibits = 13;                 /* 24 bit nibbles */
         else
            if(mem.iv < 134217728)
               ibits = 14;              /* 28 bit nibbles */
            else
               ibits = 15;              /* 32 bit nibbles */

   mem.iv = 0;
   return(ibits);
}
/*********************************************************************/
#ifdef __STDC__
  void pack(short *ibas, long icmp[])
#else
  void pack(ibas,icmp)
  short *ibas;
  long icmp[];
#endif
{
   long key[2],ktmp;
   short jl,jb,js,j;

   jl = 0;
/* Pack the keys. */
   key[0] = mem.layout[1][0];
   key[1] = mem.layout[1][1];
   snible((char *) icmp,key,&mem.ipt,(short) 4,(short) 2);

/* Loop over the two data fields in the frame. */
   for(j = 0; j <= 1; j++)
   {
      js = mem.layout[0][j];
      jb = mem.layout[1][j];
      snible((char *) icmp,&mem.lsav[js],&mem.ipt,nib[0][jb],nib[1][jb]);
      jl = jl+nib[1][jb];
   }

/* Update the frame and record sample counts and the base pointer. */
   mem.kpt = (jl < mem.isav+1)?jl:mem.isav+1;
   mem.npt = mem.npt+mem.kpt;
   *ibas = *ibas+mem.kpt+mem.layout[0][0];

/* Do end of block stuff if necessary. */
   if(mem.ifr%7 == 0)
   {
      ktmp = mem.ipt-mem.nct;
      snible((char *)icmp,&ktmp,&mem.ipt,(short) 8,(short) 1);
      mem.nct = mem.ipt-1;
   }

/* Shift the saved differences down. */
   js = mem.layout[0][2];
   if(js <= mem.isav)
   {
      for(j = js; j <= mem.isav; j++)
         mem.lsav[j-js] = mem.lsav[j];
      mem.layout[0][0] = 0;       mem.layout[1][0] = mem.layout[1][2];
      mem.isav = mem.isav-js;
   }
   else
      mem.isav = -1;
   return;
}
/***********************************************************************/
#ifdef __STDC__
  void bufout(short last, long icmp[])
#else
  void bufout(last,icmp)
  short last;
  long icmp[];
#endif
{
   long ktmp;
   short itmp,j;
	int zero=0;
/* Add the header information. */
   itmp = 0;
   snible((char *) icmp,&mem.ia0,&itmp,(short) 32,(short) 1);    /* forward integration 
                                                           constant */
   ktmp = mem.npt;
   snible((char *) icmp,&ktmp,&itmp,(short) 16,(short) 1);
                                                        /* sample count */

/* Add the end of block back pointer.  Note that the last block may have less 
 than seven frames. */
   if(mem.nct < mem.ipt-1)
   {
      ktmp = mem.ipt-mem.nct;
      snible((char *) icmp,&ktmp,&mem.ipt,(short) 8,(short) 1);
   }

/* Take care of the last record. */
   if(last > 0) 
   {
      ktmp = mem.kpt;
      snible((char *) icmp,&ktmp,&mem.ipt,(short) 8,(short) 1);
                                                        /* last frame sample 
                                                           count */
      itmp=NBK-4;
      snible((char *) icmp,&mem.ian,&itmp,(short) 32,(short) 1); /* reverse integration 
                                                           constant */
   }

/* Call a user supplied output routine. */

   putbuf(mem.npt,(unsigned char *) icmp,last,mem.ipt,0);


/* Reset the pointers and counters for the next logical record. */
   mem.ifr = 0;
   mem.npt = 0;
   mem.ipt = NST;
   mem.nct = mem.ipt-1;
/* Pre-zero the compession buffer. */
   for(j = 0; j <= NBKL; j++) icmp[j] = 0;
   return;
}
/**************************************************************************/
#ifdef __STDC__
  void prtial(struct cmprfr *memex, unsigned char icmp[])
#else
  void prtial(memex,icmp)
  unsigned char icmp[];
  struct cmprfr *memex;
#endif
{
	long ktmp;						/* temporary storage */
	extern FILE *logout;
	short ipt0,itmp;				/* Temp storage for IPT */
	mem = *memex;					/* Set internal MEM to be MEMEX */
#ifdef DEBUG_PRINT
	fprintf(logout,"Prtial call npt=%d ipt=%d\n",mem.npt,mem.ipt);
#endif
	if(mem.npt <=0) return;			/* nothing to update */
	itmp=0;							/* replaces IPT to replace  ia0 */
	snible(( char *)icmp,&mem.ia0,&itmp,(short) 32,(short) 1);/* stuff fwd integration*/
	ktmp=mem.npt;
	snible((char *)icmp,&ktmp,&itmp,(short) 16,(short) 1);/* stuff NPT # of points*/
	ipt0=mem.ipt;						/* save value of IPT for restore at exit*/
	if(mem.nct < mem.ipt-1) {
		ktmp=mem.ipt-mem.nct;
		snible(( char *)icmp,&ktmp,&mem.ipt,(short) 8, (short) 1);/* stuff back pnt*/
	} else {
		ktmp=0;
		snible(( char *)icmp,&ktmp,&mem.ipt,(short) 8, (short) 1);/* stuff zero if blk*/
	}
	mem.ipt=ipt0;					/* restore IPT */
#ifdef DEBUG_PRINT
	fprintf(logout,"prtial : npt=%d last 3=%d %d %d\n",ktmp,icmp[mem.ipt-2],
		icmp[mem.ipt-1],icmp[mem.ipt]);
#endif
	putbuf(mem.npt,icmp,0,mem.ipt,1);	/* call put buf partial flg true*/
	return ;
}
/**************************************************************************/
/*
 $$$$$ calls only library routines $$$$$

   Snible puts n consecutive nibbles of length nb bits into byte array 
   ib beginning at byte ib(ns), taking them from integer*4 array ia.  
   Bits in ib are undisturbed except where nibbles are inserted.  Ns is 
   updated to point to the next unprocessed byte in ib.  Note that even 
   length nibbles up to 32-bits work except for 30-bits.
*/
#ifdef __STDC__
  void snible(char ib[], long ia[], short *ns, short nb, short n)
#else
  void snible(ib,ia,ns,nb,n)
  char ib[];
  long ia[];
  short *ns,nb,n;
#endif
{
   char ja[4];
   long *ka;
   short kb,isw,mb,mbe,krun,kshf,ishf,ke,k,i,j;
	int zero=0;
/* Initialize some constants. */
   ka = (long*)ja;
   kb = nb/2-1;
   isw = (kb%4)+1;
   mb = 4-(kb+5-isw)/4;

   switch (isw)
   {
      case 1:   /* 2, 10, 18, and 26-bit nibbles */
         krun = 4;
         goto cs2;

      case 2:   /* 4, 12, 20, and 28-bit nibbles */
         krun = 2;
cs2:
         kshf = 2*isw;
/* Take the data in groups of krun. */
         for(k = 0; k < n; k = k+krun)
         {
            ishf = 8;
            ke = k+krun-1;
            ib[*ns] = 0;
/* Pack each word in this group. */
            for(i = k; i <= ke; i++)
            {
               ishf = ishf-kshf;
/* Mask the input word and shift it into position. */
               *ka = (ia[i]&mask[kb])<<ishf;
/* Or in the first byte as it may overlap the previous nibble, then copy
   in the rest of the bytes. */
#ifdef _INTEL
              ib[*ns] = ib[*ns]|ja[3-mb];
              if(mb < 3)
                 for(j = mb+1; j <= 3; j++) ib[++(*ns)] = ja[3-j];
#else
              ib[*ns] = ib[*ns]|ja[mb];
              if(mb < 3)
                 for(j = mb+1; j <= 3; j++) ib[++(*ns)] = ja[j];
#endif
            }
/* Each group ends on a byte boundary, so adjust ns. */
            (*ns)++;
         }
         break;

      case 3:   /* 6, 14, 22, and 30-bit nibbles */
         kshf = 2*isw;
/* Take the data in groups of 4. */
         for(k = 0; k < n; k = k+4)
         {
            ishf = 8;
            ke = k+3;
            ib[*ns] = 0;
/* Pack each word in this group. */
            for(i = k; i <= ke; i++)
            {
               ishf = ishf-kshf;
/* In this case, the second and third words of the group take an extra byte. */
               if(ishf < 0)
               {
                  mbe = mb-1;
                  ishf = ishf+8;
               }
               else mbe = mb;
/* Mask the input word and shift it into position. */
               *ka = (ia[i]&mask[kb])<<ishf;
/* Or in the first byte as it may overlap the previous nibble, then copy 
   in the rest of the bytes. */
#ifdef _INTEL
               ib[*ns] = ib[*ns]|ja[3-mbe];
               if(mbe < 3)
                  for(j = mbe+1; j <= 3; j++) ib[++(*ns)] = ja[3-j];
#else
               ib[*ns] = ib[*ns]|ja[mbe];
               if(mbe < 3)
                  for(j = mbe+1; j <= 3; j++) ib[++(*ns)] = ja[j];
#endif
            }
/* Each group ends on a byte boundary, so adjust ns. */
            (*ns)++;
         }
         break;

      case 4:   /* 8, 16, 24, and 32-bit nibbles */
         (*ns)--;
/* Loop over each input word. */
         for(i = 0; i < n; i++)
         {
/* Mask out the part needed and copy it in. */
            *ka = ia[i]&mask[kb];
#ifdef _INTEL
            for(j = mb; j <= 3; j++) ib[++(*ns)] = ja[3-j];
#else
            for(j = mb; j <= 3; j++) ib[++(*ns)] = ja[j];
#endif
         }
         (*ns)++;
         break;
   }
   return;
}

