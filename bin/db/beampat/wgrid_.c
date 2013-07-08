#include <malloc.h>
#include <string.h>
#include <math.h>
#include <fcntl.h>

char *strf2c();

void
wgrid_ (fname, nx, ny, xlist, ylist, zgrid, len_fname)

char *fname;
int *nx;
int *ny;
float *xlist;
float *ylist;
float *zgrid;

int len_fname;

{
	int fd;

	fd = open (strf2c(fname, len_fname), (O_RDWR|O_CREAT|O_TRUNC), 0666);
	if (fd < 0) return;
	if (write (fd, nx, sizeof(int)) != sizeof(int)) {
		close (fd);
	}
	if (write (fd, ny, sizeof(int)) != sizeof(int)) {
		close (fd);
	}
	if (write (fd, xlist, (*nx)*sizeof(float)) != (*nx)*sizeof(float)) {
		close (fd);
	}
	if (write (fd, ylist, (*ny)*sizeof(float)) != (*ny)*sizeof(float)) {
		close (fd);
	}
	if (write (fd, zgrid, (*nx)*(*ny)*sizeof(float)) != (*nx)*(*ny)*sizeof(float)) {
		close (fd);
	}
	close (fd);
}

char *
strf2c (string, len)

char *string;
int len;

{
	static char out[512];
	int i, j, k;

	out[0] = '\0';
	if (len < 1) return (out);
	for (i=0; i<len; i++) if (string[i] != ' ') break;
	if (i == len) return (out);
	for (j=len; j>0; j--) if (string[j-1] != ' ') break;
	if (j == 0) return (out);
	for (k=0; i<j; i++) out[k++] = string[i];
	out[k] = '\0';
	return (out);
}

