c PROGRAM COUNTS THE NUMBER OF ARGUMENTS IN A CHARACTER STRING
c VALID DELIMITERS ARE BLANK AND COMMA
c NARG IS SET TO -1 IF AN ERROR IS DETECTED
c NOTE THAT NULL ARGUMENTS (,,) ARE NOT VALID
      subroutine nmarg(string, narg, locbeg)
      character string*(*)
      character ctest*1
      dimension locbeg(100)
# 12 "nmarg.for"
      narg = 0
# 14 "nmarg.for"
      lenstr = len(string)
      if (lenstr .eq. 0) return 
# 17 "nmarg.for"
      lenstr = lentru(string)
      if (lenstr .eq. 0) return 
# 20 "nmarg.for"
   20 if (string(lenstr:lenstr) .eq. ',') then
      narg = -1
      return 
      end if
c SET TO 1 IF LAST CHARACTER WAS A DELIMITER
# 25 "nmarg.for"
      lastc = 0
# 27 "nmarg.for"
      do 30 i = 1, lenstr
      ctest = string(i:i)
      if (ctest .eq. ' ') then
      lastc = 1
      else if (ctest .eq. ',') then
      if (lastc .lt. 2) then
      narg = narg + 1
      lastc = 2
      else
      narg = -1
      return 
      end if
      else
      if ((lastc .ne. 0) .or. (narg .eq. 0)) then
      narg = narg + 1
      locbeg(narg) = i
      lastc = 0
      end if
      end if
   30 continue
      return 
      end
