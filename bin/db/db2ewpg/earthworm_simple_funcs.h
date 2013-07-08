          /***************************************************
           *            earthworm_simple_funcs.h             *
           *                                                 *
           *         Earthworm administrative setup:         *
           *        global info for all installations        *
           *                                                 *
           ***************************************************/

#ifndef EARTHWORM_SIMPLE_FUNCS_H
# define EARTHWORM_SIMPLE_FUNCS_H

/* DO NOT PUT ANY #includes in this file!!!! */

/* This file contains prototypes for earthworm libsrc
   functions that are simple and require no special
   type definitions.  If you have more complex functions
   (semaphores, threads, mutexes, sockets, etc.) you should
   put them in earthworm_complex_funcs.h

   Note, please try to keep functions from the same object
   together in one section of one file.  So all of the logit.c
   stuff should go together.
   Davidk 2001/04/06
*************************************************************/

/* Prototypes for Earthworm utility functions
 ********************************************/
long  GetKey  ( char * );                   /* getutil.c    sys-independent  */
int   GetInst ( char *, unsigned char * );  /* getutil.c    sys-independent  */
int   GetModId( char *, unsigned char * );  /* getutil.c    sys-independent  */
int   GetType ( char *, unsigned char * );  /* getutil.c    sys-independent  */
int   GetLocalInst( unsigned char * );      /* getutil.c    sys-independent  */
char *GetKeyName  ( long );                 /* getutil.c    sys-independent  */
char *GetInstName ( unsigned char );        /* getutil.c    sys-independent  */
char *GetModIdName( unsigned char );        /* getutil.c    sys-independent  */
char *GetTypeName ( unsigned char );        /* getutil.c    sys-independent  */
char *GetLocalInstName( void );             /* getutil.c    sys-independent  */
void  GetUtil_LoadTable( void );            /* getutil.c    sys-independent  */

void logit_init( char *, short, int, int ); /* logit.c      sys-independent  */
void addslash (char *);                                 /* logit.c      appends platform-
                                                                                                                        specific directory
                                                                                                                        slash to string  */
void html_logit( char *, char *, ... );     /* logit.c      sys-independent  */
void logit( char *, char *, ... );          /* logit.c      sys-independent  */
int  get_prog_name( char *, char * );       /* logit.c      DEPRECATED!!!!!  */
int  get_prog_name2( char *, char *, int ); /* logit.c      sys-independent  */


int SendMail( char [][60], int, char *, char *,
              char *, char *, char *, char *, char * );  

/* System-dependent stuff goes here
   ********************************/

int  copyfile( char *, char *, char *, char *, char *, char *, char * );
                                            /* copyfile.c   system-dependent */

int  chdir_ew( char * );                    /* dirops_ew.c  system-dependent */
int  CreateDir( char * );                   /* dirops_ew.c  system-dependent */
int  RecursiveCreateDir( char * );          /* dirops_ew.c  system-dependent */
int  GetFileName( char * );                 /* dirops_ew.c  system-dependent */
int  OpenDir( char * );                     /* dirops_ew.c  system-dependent */
int  GetNextFileName( char * );             /* dirops_ew.c  system-dependent */
int  rename_ew( char *, char * );           /* dirops_ew.c  system-dependent */

int  GetDiskAvail( unsigned * );            /* getavail.c   system-dependent */

int  getsysname_ew( char *, int );          /* getsysname_ew.c sys-dependent */

int SendPage( char * );                     /* sendpage.c   system-dependent */

void sleep_ew( unsigned );                  /* sleep_ew.c   system-dependent */


int  pipe_init ( char *, unsigned long );   /* pipe.c       system-dependent */
int  pipe_put  ( char *, int );             /* pipe.c       system-dependent */
int  pipe_get  ( char *, int, int * );      /* pipe.c       system-dependent */
void pipe_close( void );                    /* pipe.c       system-dependent */

/* from geo_to_km.c */
int geo_to_km (double lat1, double lon1, double lat2, double lon2,
              double* dist, double* azm);
int geo_to_km_deg (double lat1, double lon1, double lat2, double lon2,
              double* dist, double *xdeg, double* azm);

#endif /* EARTHWORM_SIMPLE_FUNCS_H */


