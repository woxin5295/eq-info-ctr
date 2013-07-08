#include <stdio.h>

char *getstr (string, max_chars)

  char string[];
  int max_chars;
{

  int i, n, c;

  n = max_chars - 1;

  for (i=0; i<n; i++) {

    if ((c = getchar()) != EOF) {
      if ((char) c != '\n') string[i] = (char) c;
      else {
        string[i] = '\0';
        return (string);
        }
      } /* End if got a non-EOF character */

    else {
      if (i > 0) {
        string[i] = '\0';
        return (string);
        }
      else return (NULL);

      } /* End else we hit EOF */
    } /* End for i to n-1 characters */

  string[n] = '\0';
  return (string);

}
