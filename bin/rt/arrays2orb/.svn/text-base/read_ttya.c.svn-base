#include <stdio.h>   /* Standard input/output definitions */
#include <string.h>  /* String function definitions */
#include <unistd.h>  /* UNIX standard function definitions */
#include <fcntl.h>   /* File control definitions */
#include <errno.h>   /* Error number definitions */
#include <termios.h> /* POSIX terminal control definitions */
#include <sys/file.h>

 /*
  * 'open_port()' - Open serial port 1.
  */
int open_port(void) {
	int fd; 
	fd = open("/dev/cua/a", O_RDWR | O_NOCTTY | O_NDELAY);
	if (fd == -1) {
		fprintf(stderr, "open_port: Unable to open /dev/cua/a - %s\n",
			strerror(errno));
	}
	return (fd);
}
/* #define MAX_DATA_BUF_SIZE 5000 */
/*#define MAX_DATA_BUF_SIZE 256 */
#define MAX_DATA_BUF_SIZE 384 
main(argc, argv)
int argc;
char **argv;
{
	struct termios ios;
	int fd, num, num_nccs, n;
	int rts_mask;
	unsigned char buf[MAX_DATA_BUF_SIZE];
	fd = open_port();

	fcntl(fd, F_SETFL, FNDELAY);	 /* reading data from port */
	if(ioctl(fd,TCGETS,&ios) == -1) {
		fprintf(stderr, "Unable to get termio structure \n");
		exit(1);
	}
	ios.c_lflag = 0x0000;
	ios.c_iflag = 0x0000; 
	ios.c_oflag = 0x0000; 
	ios.c_cflag = (B38400|CS8|CREAD|CRTSXOFF); 
	/* ios.c_cflag = (B38400|CS8|CREAD|CLOCAL); */
	if(ioctl(fd,TCSETS,&ios) == -1) {
		fprintf(stderr, "Unable to set termio structure and enable the receiver \n");
		exit(1);
	}
	rts_mask = TIOCM_RTS;
	while(1) {
		num = read(fd, buf, MAX_DATA_BUF_SIZE);
		if ( num > 0 ) {
			write(1,buf,num);
		}
	}
}
