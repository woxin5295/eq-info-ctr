/**************************************************
	Safe TCP socket structure
*/
struct tcpsocket {
	char *hostname;							/* host name from init */
	char *dotadr;							/* dot address from first init */
	int path;								/* path of this socket */
	int port;								/* port number to open */
};
/*
	Function Prototypes for safe tcp
*/
#ifdef __STDC__

int readtcp(struct tcpsocket *, void *, int);
int writetcp(struct tcpsocket *, void *, int len);
int init_safetcp(struct tcpsocket *, char *host, char *dot, int port);
int open_tcp(struct tcpsocket *);

#else

int readtcp();								/* prototype for read function */
int writetcp();								/* prototype for writes */
int open_tcp();								/* user opens for a socket */
int init_safetcp();							/* internal opener, not user call */

#endif
