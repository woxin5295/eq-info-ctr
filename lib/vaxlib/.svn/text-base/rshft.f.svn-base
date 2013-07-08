c
c  PROGRAM TO RIGHT-JUSTIFY A CHARACTER VARIABLE
c
c  ARRAY - A CHARACTER VARIABLE
      character *(*)function rshft(array)
      character*256 temp
c
c  GET LENGTH OF ARRAY
c
      character array*(*)
# 11 "rshft.for"
      n = len(array)
      rshft = array
c
c  FIND THE POSITION OF THE FIRST NON-BLANK CHARACTER FROM THE RIGHT
c
# 13 "rshft.for"
      if (n .eq. 0) return 
# 17 "rshft.for"
      do 10 i = 1, n
      j1 = (n - i) + 1
      if (array(j1:j1) .ne. ' ') goto 20
c
c  ALL CHARACTERS ARE BLANK, SO RETURN
c
# 20 "rshft.for"
   10 continue
c
c  SHIFT CHARACTERS TO RIGHT
c
# 24 "rshft.for"
      return 
# 28 "rshft.for"
   20 if (j1 .eq. n) return 
      temp(1:j1) = rshft(1:j1)
c     rshft((n - j1) + 1:n) = rshft(1:j1)
      rshft((n - j1) + 1:n) = temp(1:j1)
      rshft(1:n - j1) = ' '
      return 
      end
