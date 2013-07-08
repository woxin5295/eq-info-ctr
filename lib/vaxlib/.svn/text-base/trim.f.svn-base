c -- "TRIM" LEADING AND TRAILING BLANKS FROM A CHARACTER STRING BY LEFT-
c --  JUSTIFYING THE STRING AND THEN REPORTING THE POSITION OF THE LAST
c --  NON-BLANK CHARACTER (NCH)
c
c	STRING	-	ON INPUT, THE STRING TO BE MODIFIED
c			ON OUTPUT, THE MODIFIED STRING
c	NCH	-	ON OUTPUT, THE NUMBER OF CHARACTERS IN
c			   THE MODIFIED STRING; A VALUE LESS THAN 0
c			   INDICATES A FAILURE.
      subroutine trim(string, nch)
      character string*(*)
# 14 "trim.for"
      nch = 0
      lenstr = len(string)
      if (lenstr .eq. 0) return 
# 18 "trim.for"
      ib = 1
      do while (ib .le. lenstr)
      if (string(ib:ib) .ne. ' ') goto 100
      ib = ib + 1
      end do
cSTRING IS ALL BLANKS
# 23 "trim.for"
      return 
# 25 "trim.for"
  100 ie = lenstr
      do while (ie .ge. ib)
cSUCCESS
# 27 "trim.for"
      if (string(ie:ie) .ne. ' ') then
      nch = (ie - ib) + 1
      string(1:nch) = string(ib:ie)
      if (ie .lt. lenstr) string(nch + 1:lenstr) = ' '
      return 
      end if
      ie = ie - 1
      end do
cFAILURE - THIS SHOULD NEVER OCCUR
# 36 "trim.for"
      nch = -1
      return 
      end
