      INTEGER FUNCTION strlen (string)

      CHARACTER*(*) string
      INTEGER i, max

      max = LEN(string)

      DO 100 i=max, 1, -1
        IF (string(i:i).NE.' ') THEN
          strlen = i
          RETURN
        ENDIF
 100  CONTINUE

      strlen = 0
      RETURN
      END
