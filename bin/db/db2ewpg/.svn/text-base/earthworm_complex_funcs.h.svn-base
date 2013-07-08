          /***************************************************
           *            earthworm_complex_funcs.h             *
           *                                                 *
           *         Earthworm administrative setup:         *
           *        global info for all installations        *
           *                                                 *
           ***************************************************/

#ifndef EARTHWORM_COMPLEX_FUNCS_H
#define EARTHWORM_COMPLEX_FUNCS_H

/* This file contains prototypes for earthworm libsrc
   functions that require special type definitions, such as
   (semaphores, threads, mutexes, sockets, etc.).

   If you have functions that only use primitive types and you
   do not need any extra header files for them to compile, then
   you can put them into earthworm_simple_funcs.h.

   Note, please try to keep functions from the same object
   together in one section of one file.  So all of the sema_ew.c
   stuff should go together.  Thank You!
   Davidk 2001/04/06
*************************************************************/

/* System-dependent stuff goes here
   ********************************/
#include "platform.h"

void CreateSemaphore_ew( void );            /* sema_ew.c    system-dependent */
void PostSemaphore   ( void );              /* sema_ew.c    system-dependent */
void WaitSemPost     ( void );              /* sema_ew.c    system-dependent */
void DestroySemaphore( void );              /* sema_ew.c    system-dependent */
void CreateMutex_ew  ( void );              /* sema_ew.c    system-dependent */
void RequestMutex( void );                  /* sema_ew.c    system-dependent */
void ReleaseMutex_ew( void );               /* sema_ew.c    system-dependent */
void CloseMutex( void );                    /* sema_ew.c    system-dependent */

/*void CreateSpecificMutex( mutex_t * );
void CloseSpecificMutex( mutex_t * );
void RequestSpecificMutex( mutex_t * );
void ReleaseSpecificMutex( mutex_t * );*/

                                            /* sendmail.c   system-dependent */
void SocketSysInit( void   );               /* socket_ew.c  system-dependent */
void SocketClose  ( int    );               /* socket_ew.c  system-dependent */
void SocketPerror ( char * );               /* socket_ew.c  system-dependent */
int sendall( int, const char *, long, int );/* socket_ew.c  system-dependent */

int  WaitThread( unsigned * );              /* threads_ew.c system-dependent */
int  KillThread( unsigned int );            /* threads_ew.c system-dependent */
int  KillSelfThread( void );                /* threads_ew.c system-dependent */
/*int  StartThread( thr_ret fun(void *), unsigned stack_size, unsigned *thread_id );
int  StartThreadWithArg( thr_ret fun(void *), void *arg, unsigned stack_size, unsigned *thread_id );
*/
#endif /* EARTHWORM_COMPLEX_FUNCS_H */



