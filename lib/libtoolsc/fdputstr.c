/* BEGIN fdgetstr.c */

#include <stdio.h>

int fdputstr (fd, string)
  int fd;
  char *string;
{

  int status, nbytes_write, write();
  size_t strlen();

  if (string == NULL ) {
    status = -1;
    }

  else {
    nbytes_write = strlen(string);
    
    if (nbytes_write > 0) {
      if ((status = write(fd, string, nbytes_write)) != nbytes_write) {
        fprintf (stderr,"Write failed.  %d bytes written of %d\n",
                 status, nbytes_write);
        perror ("fdputstr:write");
        status = -1;
        }
      else if ((status += write(fd, "\n", 1)) != nbytes_write+1) {
        perror ("fdputstr:write");
        status = -1;
        }
      }
    else {
      status = -1;
      }
    }

  return (status);

} /* End fdputstr() */

