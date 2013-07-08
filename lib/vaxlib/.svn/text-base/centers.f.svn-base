c
c  PROGRAM TO CENTER A CHARACTER VARIABLE
c
c  ARRAY - A CHARACTER VARIABLE
      subroutine centers(array)
      character array*(*)
c
c  GET LENGTH OF ARRAY
c
      character newarr*150
c
c  FIND THE POSITION OF THE FIRST NON-BLANK CHARACTER FROM THE LEFT
c
# 12 "centers.for"
      n = len(array)
# 16 "centers.for"
      do 10 i = 1, n
      if (array(i:i) .ne. ' ') then
      nbkl = i - 1
      goto 20
      end if
c
c  ALL CHARACTERS ARE BLANK, SO RETURN
c
# 21 "centers.for"
   10 continue
c
c FIND THE NUMBER OF BLANKS ON THE RIGHT
      return 
c
# 28 "centers.for"
   20 nbkr = n - lentru(array)
c
c  ARRAY SHOULD BEGIN IN (NBKR + NBKL)/2.
c
# 30 "centers.for"
      if (nbkr .eq. nbkl) return 
# 34 "centers.for"
      nbeg = 0.5 + ((nbkr + nbkl) / 2.)
      newarr = ' '
      newarr(nbeg:n) = array(nbkl + 1:n)
      array = newarr
      return 
      end
