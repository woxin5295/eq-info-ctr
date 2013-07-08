c PROGRAM COUNTS THE NUMBER OF ARGUMENTS IN A CHARACTER STRING
c VALID DELIMITERS ARE BLANK AND COMMA
c NUMARG IS SET TO -1 IF AN ERROR IS DETECTED
c NOTE THAT NULL ARGUMENTS (,,) ARE NOT VALID
      integer function numarg(string)
      character string*(*)
      character ctest*1
# 11 "numarg.for"
      numarg = 0
# 13 "numarg.for"
      lenstr = len(string)
      if (lenstr .eq. 0) return 
# 16 "numarg.for"
      do 10 i = lenstr, 1, -1
      if (string(i:i) .ne. ' ') then
      lenstr = i
      goto 20
      end if
   10 continue
# 23 "numarg.for"
      return 
# 25 "numarg.for"
   20 if (string(lenstr:lenstr) .eq. ',') then
      numarg = -1
      return 
      end if
c SET TO 1 IF LAST CHARACTER WAS A DELIMITER
# 30 "numarg.for"
      lastc = 0
# 32 "numarg.for"
      do 30 i = 1, lenstr
      ctest = string(i:i)
      if (ctest .eq. ' ') then
      if (lastc .eq. 0) numarg = numarg + 1
      lastc = 1
      else if (ctest .eq. ',') then
      if (lastc .lt. 2) then
      numarg = numarg + 1
      lastc = 2
      else
      numarg = -1
      return 
      end if
      else
      lastc = 0
      end if
   30 continue
# 50 "numarg.for"
      numarg = numarg + 1
      return 
      end
