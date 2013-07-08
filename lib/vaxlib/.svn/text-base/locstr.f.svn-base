c
c -- ROUTINE TO FIND THE STARTING POSTION AND LENGTH OF THE NEXT SUBSTRI
cNG WHICH
c    BEGINS AT THE FIRST NON-BLANK CHARACTER FROM THE LEFT OF THE INPUT 
cSTRING
c    AND IS DELIMITED ON THE RIGHT BY A BLANK OR THE END OF THE INPUT ST
cRING.
c	STRING	- THE STRING TO BE SEARCHED
c	IB	- THE STARTING POSITION OF THE SUBSTRING
c	LENSUB	- LENGTH IN BYTES OF THE SUBSTRING
      subroutine locstr(string, ib, lensub)
      character string*(*)
# 13 "locstr.for"
      ib = 0
      lensub = 0
      lenstr = len(string)
      if (lenstr .eq. 0) return 
# 18 "locstr.for"
      ib = 1
      do while (ib .le. lenstr)
      if (string(ib:ib) .ne. ' ') goto 100
      ib = ib + 1
      end do
cALL BLANKS
# 23 "locstr.for"
      return 
# 25 "locstr.for"
  100 if (ib .lt. lenstr) lensub = index(string(ib + 1:lenstr),' ')
      if (lensub .eq. 0) lensub = (lenstr - ib) + 1
      return 
      end
