              /*************************************************
               *                   platform.h                  *
               *                                               *
               *  System-dependent stuff.                      *
               *  This file is included by earthworm.h         *
               *************************************************/

#ifndef PLATFORM_H
#define PLATFORM_H

#ifdef _WINNT
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <windows.h>
#include <winsock.h>               /* Socket stuff */
#include <process.h>               /* Required for getpid() */
#include <sys\types.h>

/* Thread functions return this */
#define thr_ret void              

#define getpid _getpid
typedef int    pid_t;
typedef HANDLE sema_t;
typedef HANDLE mutex_t;
typedef HANDLE timer_t;

/* added so that logit.c can call vsnprintf for all platforms */
# define vsnprintf _vsnprintf
# define  snprintf  _snprintf

/* Thread priority constants.  These are based off the Win32 constants in winbase.h */
#define EW_PRIORITY_LOWEST      THREAD_PRIORITY_IDLE
#define EW_PRIORITY_LOW         THREAD_PRIORITY_BELOW_NORMAL
#define EW_PRIORITY_NORMAL      THREAD_PRIORITY_NORMAL
#define EW_PRIORITY_HIGH        THREAD_PRIORITY_ABOVE_NORMAL
#define EW_PRIORITY_CRITICAL    THREAD_PRIORITY_TIME_CRITICAL

#define DIR_SLASH   '\\'
#else
#define DIR_SLASH   '/'
#endif /* _WINNT */

#ifdef _OS2
#define INCL_DOSPROCESS
#define INCL_DOSMEMMGR
#define INCL_DOSSEMAPHORES
#define INCL_DOSFILEMGR
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <os2.h>
#include <netinet\in.h>       /* contains typedef of struct sockaddr_in */
#include <process.h>               /* Required for getpid() */
#include <types.h>
#include <nerrno.h>
#include <sys\socket.h>            /* Socket stuff */
#include <netdb.h>                 /* Socket stuff */
typedef void thr_ret;              /* Thread functions return this */
typedef int  pid_t;
typedef HEV  sema_t;
typedef HMTX mutex_t;
typedef long timer_t;

typedef long DWORD;
#endif /* _OS2 */


#ifdef _LINUX
#define _UNIX
/* broke this out on 2006/03/08 - paulf */
/* note the LINUX/POSIX includes go here, mostly pthread changes */
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <sys/types.h>
#include <netinet/in.h>            /* Socket stuff */
#include <arpa/inet.h>             /* Socket stuff */
#include <signal.h>
#include <sys/shm.h>
#include <sys/wait.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/socket.h>            /* Socket stuff */
#include <netdb.h>                 /* Socket stuff */
#define mutex_t pthread_mutex_t
#define sema_t int
#define thread_t int
#define USYNC_THREAD 0
#undef SHM_INFO
#define fork1 fork

/* Thread functions return this */
#define thr_ret void*             

#ifndef LONG
#define LONG long
#endif
#ifndef LONG_t
#define LONG_t
#ifdef _CYGWIN
typedef unsigned long ulong ;
#endif
typedef unsigned long ULONG ;
#endif
typedef long DWORD ;

#endif /* _LINUX */

#ifdef _MACOSX
#define _UNIX
#include <stdio.h>
#include <stdlib.h>
#include <sys/malloc.h>            /* Redundant - included in stdlib.h */
#include <sys/types.h>
#include <netinet/in.h>            /* Socket stuff */
#include <arpa/inet.h>             /* Socket stuff */
#include <signal.h>
#include <sys/shm.h>
#include <sys/wait.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/socket.h>            /* Socket stuff */
#include <netdb.h>                 /* Socket stuff */
#define mutex_t pthread_mutex_t
#define sema_t int
#define thread_t int
#define USYNC_THREAD 0
#undef SHM_INFO
#define fork1 fork

/* Thread functions return this */
#define thr_ret void*             

#ifndef LONG
#define LONG long
#endif
#ifndef LONG_t
#define LONG_t
typedef unsigned long ULONG ;
typedef unsigned long ulong;
#endif
typedef long DWORD ;

typedef long timer_t;
#endif /* _MACOSX */

#ifdef _SOLARIS
/* all SOLARIS includes now specifically go here */
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <sys/types.h>
#include <netinet/in.h>            /* Socket stuff */
#include <arpa/inet.h>             /* Socket stuff */
#include <signal.h>
#include <synch.h>                 /* for mutex's */
#include <sys/ipc.h>
#include <sys/shm.h>
#include <wait.h>
#include <thread.h>
#include <unistd.h>
#include <sys/socket.h>            /* Socket stuff */
#include <netdb.h>                 /* Socket stuff */

/* Thread functions return this */
#define thr_ret void*             

#ifndef LONG_t
#define LONG_t
typedef long LONG;
#endif
typedef long DWORD;

/* Thread priority constants.  These are arbitrary values chosen over the Solaris priority range
 * (0-127).  These may need to be tweaked.  (MMM 10/7/04)
 */
#define EW_PRIORITY_LOWEST      0
#define EW_PRIORITY_LOW         1
#define EW_PRIORITY_NORMAL      8
#define EW_PRIORITY_HIGH       16
#define EW_PRIORITY_CRITICAL  127

#endif /* _SOLARIS */

#endif /* PLATFORM_H */


