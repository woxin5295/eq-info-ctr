c
c  PROGRAM TO LEFT-JUSTIFY A CHARACTER VARIABLE
c
c  ARRAY - A CHARACTER VARIABLE
      character *(*)function lshft(array)
      character*256 temp
c
c  GET LENGTH OF ARRAY
c
      character array*(*)
# 11 "lshft.for"
      n = len(array)
      lshft = array
c
c  FIND THE POSITION OF THE FIRST NON-BLANK CHARACTER FROM THE LEFT
c
# 13 "lshft.for"
      if (n .eq. 0) return 
# 17 "lshft.for"
      do 10 i = 1, n
      if (array(i:i) .ne. ' ') goto 20
c
c  ALL CHARACTERS ARE BLANK, SO RETURN
c
# 19 "lshft.for"
   10 continue
c
c  SHIFT CHARACTERS TO LEFT
c
# 23 "lshft.for"
      return 
# 27 "lshft.for"
   20 if (i .eq. 1) return 
      temp(1:n) = lshft(1:n)
c     lshft(1:(n - i) + 1) = lshft(i:n)
      lshft(1:(n - i) + 1) = temp(i:n)
      lshft((n - i) + 2:n) = ' '
      return 
      end
