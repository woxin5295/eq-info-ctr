/* Echo_err - writes characters to standard error. */
/*   Option: -n Do not append newline              */
#include <stdio.h>

main (argc, argv)
  short argc;
  char *argv[];
{
  int strcmp();
  short new_line = 1;	/* Initialize new_line flag on */

  if (strcmp(argv[1],"-n") == 0) {
    new_line = 0;	/* Clear new_line flag */
    argc--;		/* Decrease argument count */
    argv++;		/* Move on to next argument */
    }

  while (--argc) {	/* Loop through the arguments */
    fputs (*++argv, stderr);
    /* Put a space between each word */
    if (argc > 1) putc (' ', stderr);
    }

  if (new_line) putc ('\n', stderr);

}
