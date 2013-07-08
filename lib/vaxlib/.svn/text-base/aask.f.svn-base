c
c AASK prompts on LUNOUT=5, then reads a real value from
c LUNIN=0 using A80 format and a default value of dflt.
c
c ORIGINAL VERSION WRITTEN BY LARRY BAKER AND EXTENSIVELY MODIFIED BY J 
cLAHR 
c AND C STEPHENS.
c
c *** Machine dependent CODE (WORKS ON PDP VAX): ***
c *** $ and Q Format descriptors ***
c
c val = AASK (prompt,dflt,lenf)
c
c       val    = CHARACTER STRING response.
c       prompt = Prompt string.
c       dflt   = Default supplied for carriage return response
c       lenf   = If positive, then right justify response in first
c                  lenf characters
c                If negative, left justify in field of length iabs(lenf)
c
      character *(*)function aask(prompt, dflt, lenf)
      parameter (lunin = 5)
      parameter (lunout = 0)
      character prompt*(*)
      character dflt*(*), fmt*45, inline*200
c
      logical eof
# 27 "aask.for"
1     lenfld = iabs(lenf)
      lenaask = len(aask)
      aask = ' '
      if (lenfld .le. lenaask) goto 5
      write(unit=lunout, fmt=4) lenfld, lenaask
    4 format(1x,35h  ** Warning -- character length of,i3,10h specified
     &,/17h but truncated to,i3,
     &39h, the specified length of FUNCTION AASK)
# 34 "aask.for"
      lenfld = lenaask
    5 if (prompt .ne. ' ') then
      lendflt = lentru(dflt)
      if (lendflt .eq. 0) lendflt = 1
      if (lendflt .gt. lenaask) lendflt = lenaask
      fmt = '(1H , A, 3H [A,I1,4H;CR=, /, 1X, A, 3H]? , $)'
      if ((lenfld .gt. 0) .and. (lenfld .lt. 10)) goto 505
# 42 "aask.for"
      if ((lenfld .gt. 9) .and. (lenfld .lt. 100)) then
      fmt(17:17) = '2'
      goto 505
      end if
      if (lenfld .gt. 99) then
      fmt(17:17) = '3'
      goto 505
      end if
  505 if ((lendflt + len(prompt)) .lt. 69) fmt(27:28) = '  '
      write(unit=lunout, fmt=fmt) prompt(1:len(prompt)), lenfld, dflt(1:
     &lendflt)
c
# 53 "aask.for"
      end if
# 55 "aask.for"
      read(unit=lunin, fmt=504, end=9900, err=9100) inline
  504 format(a)
      nch = lentru(inline)
      eof = .false.
c
# 59 "aask.for"
      if (nch .gt. 0) goto 9000
# 61 "aask.for"
 8000 inline = dflt
      nch = lentru(dflt)
c
# 63 "aask.for"
      if (nch .eq. 0) nch = 1
c      Right justify
c      Put rightmost nonblank character in position lenfld
# 65 "aask.for"
 9000 if (lenf .gt. 0) then
# 68 "aask.for"
      i = lentru(inline)
      if (i .eq. 0) then
      aask = ' '
      return 
      end if
 9070 ifrst = (i - lenfld) + 1
      ist = (- ifrst) + 2
      if (ist .lt. 1) ist = 1
      if (ifrst .lt. 1) ifrst = 1
      aask(ist:lenfld) = inline(ifrst:i)
      if (ist .ne. 1) aask(1:ist - 1) = ' '
c      Left justify
# 79 "aask.for"
      else
# 81 "aask.for"
      i = ibegtru(inline)
      aask(1:lenfld) = inline(i:nch)
      end if
c
# 84 "aask.for"
 9085 return 
# 86 "aask.for"
 9100 continue
      write(unit=lunout, fmt=9110) 
 9110 format(1h0,30h  ** Error in FUNCTION AASK --,
     &23h Input line is too long/)
c
# 90 "aask.for"
      goto 5
# 92 "aask.for"
 9900 eof = .true.
      goto 8000
      end
