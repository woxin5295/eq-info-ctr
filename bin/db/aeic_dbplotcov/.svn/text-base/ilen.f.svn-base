c
c******************************************************************************
c
c    Integer function ILEN
c
c******************************************************************************
c
      INTEGER FUNCTION ILEN(STRING)
c
c    Function ILEN will return the number of non-blank characters
c    in character string STRING.
c
      CHARACTER*(*) STRING
c
      L = LEN(STRING)
      ILEN = 0
      DO 10  I = 1, L
        IF (STRING(I:I) .NE. ' ') ILEN = I
   10 CONTINUE
c
      RETURN
c
c    End of function ILEN
c
      END

c $Id: ilen.f,v 1.1.1.1 2000-05-23 23:27:54 kent Exp $ 
