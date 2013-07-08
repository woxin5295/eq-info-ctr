c     TEST DECOD
c     character chararray(5)*4, inpu*40 
c     character nameit*4, aask*40
c     dimension intarray(2), realarray(2), it(7)
c     inpu = '2 2 ABC DE Z                                          '
c     inpu = 
c    * aask('input 2 real and up to 5 character strings', inpu, -40)
c     it(1) = 82
c     it(2) = 82
c     it(3) = 4
c     it(4) = -4
c     it(5) = 1
c     it(6) = 1
c     it(7) = 1
c     print *, 'input = ', inpu
c     call decod(inpu, 40, 7, 5, chararray, 0, intarray, 2, realarray, 
c    &it, 1, ierr)
c     write(unit=*, fmt=*) chararray
c     write(unit=*, fmt=*) realarray
c     call decod(inpu, 40, 2, 0, 0, 0, 0, 2, realarray, it, 1, ierr)
c     write(unit=*, fmt=*) realarray
c     inpu = ' TEST'
c     call decod(inpu, 5, 1, 1, nameit, 0, 0, 0, 0, -4, 1, ierr)
c     write(unit=*, fmt=*) nameit
c     end
c------ FREE FORMAT DECODING OF A CHARACTER STRING
c     INPU             CHARACTER*(NCS)     INPUT CHARACTER STRING
c     NCS              INTEGER             LENGTH OF INPU
c     NTOT             INTEGER             TOTAL NUMBER OF VARIABLES
c     NCHAR            INTEGER             NUMBER OF CHARACTER VARIABLES
C     CHARARRAY        CHARACTER           OUTPUT ARRAY OF CHARACTERS
c     NINT             INTEGER             NUMBER OF INTEGER VARIABLES
c     INTARRAY         INTEGER             OUTPUT ARRAY OF INTEGERS
c     NREAL            INTEGER             NUMBER OF REAL VARIABLES
c     REALARRAY        REAL                OUTPUT ARRAY OF REALS
c     ITP              INTEGER(NTOT)       ARRAY OF VARIABLE TYPES
c                                           81  = INTEGER
c                                           82  = REAL
c                                          1-80 = LENGTH OF CHARACTER VA
cRIABLES
c     ICOMMA           INTEGER             0 MEANS DO NOT ALLOW NULL VAR
cIABLES
c                                            AT END OF LINE
c                                          1 MEANS ASSUME NULL VARIABLES
c AT END
c     IERR             INTEGER             1 MEANS DECODE ERROR, OTHERWI
cSE 0
      subroutine decod(inpu, ncs, ntot, nchar, chararray, nint, intarray
     &, nreal, realarray, it, icomma, ierr)
      character chararray(*)*(*), inpu*(*), iquote*1, inchar*132, inint*
     &132
      character inreal*132, input*132
      character quote
      dimension intarray(*), realarray(*), it(*), itp(100), lndc(100)
      data quote / '''' /
      data init / 0 /
      do 9998 i = 1, ntot
      itp(i) = it(i)
c--OPEN SCRATCH FILE FOR FREE FORMAT DECODING OF PARAMETERS.
 9998 continue
      if (init .eq. 1) goto 10
      init = 1
      write(unit=iquote, fmt=5) quote
    5 format(a1)
      open(unit=8, file='FREE.XXX', form='FORMATTED', status='UNKNOWN') 
   10 ierr = 0
      call compress(inpu, ncs, input, nc, iquote, icomma)
      imark = 1
      icmark = 1
      iimark = 1
      irmark = 1
      ndc = 0
      do 500 i = 1, ntot
      if (itp(i) .eq. 81) goto 100
      if (itp(i) .eq. 82) goto 200
c------ STRIP OFF ONE CHARACTER VARIABLE WITH LENGTH ITP(I)
      if (iabs(itp(i)) .gt. 132) itp(i) = 132 * isign(1,itp(i))
      if (input(imark:imark) .ne. iquote) goto 95
      call strip(input, nc, imark, inchar, icmark)
      goto 98
   95 call stripc(input, nc, imark, inchar, icmark, iabs(itp(i)), iquote
     &, ierr)
      if (ierr .eq. 1) return 
   98 continue
      ndc = ndc + 1
      lndc(ndc) = itp(i)
c------ STRIP OFF ONE INTEGER VALUE
      goto 500
  100 call strip(input, nc, imark, inint, iimark)
c------ STRIP OFF ONE REAL VALUE
      goto 500
  200 call strip(input, nc, imark, inreal, irmark)
c
c------ READ CHARACTER VARIABLES
  500 continue
      if (nchar .eq. 0) goto 600
      do 552 i = 1, nchar
  552 chararray(i) = ' '
      read(unit=inchar, fmt=*, err=9000) (chararray(i),i = 1, nchar)
      do 553 i = 1, nchar
      if (lndc(i) .lt. 0) goto 553
      call right(chararray(i)(1:iabs(lndc(i))))
c------ READ INTEGER VARIABLES
  553 continue
c      REWIND 8
c      WRITE(8,550) ININT
c      REWIND 8
  600 if (nint .eq. 0) goto 700
      do 602 i = 1, nint
  602 intarray(i) = 0
c------ READ REAL VARIABLES
      read(unit=inint, fmt=*, err=9000) (intarray(i),i = 1, nint)
c      REWIND 8
c      WRITE(8,550) INREAL
c      REWIND 8
  700 if (nreal .eq. 0) return 
      do 702 i = 1, nreal
  702 realarray(i) = 0.
      read(unit=inreal, fmt=*, err=9000) (realarray(i),i = 1, nreal)
      return 
c      REWIND 8
c      READ(8,9010) INREAL
 9000 continue
      write(unit=6, fmt=9020) inreal
 9020 format(1x,a,/,27h INPUT FORMAT ERROR DUE TO:,/
     &26h    WRONG VARIABLE TYPE OR,/
     &50h    IF ICOMMA = 0, THEN POSSIBLY TOO FEW VARIABLES)
      ierr = 1
      return 
      end
c------ STRIP OFF ONE REAL OR ONE INTEGER VALUE
      subroutine strip(input, nc, imark1, in, irmark)
c------ FIND NEXT COMMA
c      PRINT *, 'INPUT, NC, IMARK1, IN, IRMARK'
c      PRINT *, INPUT, NC, IMARK1, IN, IRMARK
      character input*(*), in*(*)
      do 500 i = imark1, nc
      if (input(i:i) .eq. ',') goto 600
  500 continue
  600 imark2 = i
      irmark2 = irmark + (imark2 - imark1)
      in(irmark:irmark2) = input(imark1:imark2)
      imark1 = imark2 + 1
      irmark = irmark2 + 1
      return 
      end
c------ STRIP OFF ONE CHARACTER VARIABLE WITH LENGTH ITP
c------ THIS IS FOR THE CASE OF NO ENCLOSING QUOTES
      subroutine stripc(input, nc, imark1, inchar, icmark, itp, iquote, 
     &ierr)
      character input*(*), inchar*(*), iquote*1
c------ FIND NEXT COMMA
      ierr = 0
   10 do 500 i = imark1, nc
      if (input(i:i) .eq. ',') goto 600
  500 continue
  600 imark2 = i - 1
      length = (imark2 - imark1) + 1
      if (length .ne. 0) goto 700
      icmark2 = icmark
      goto 1200
  700 idif = itp - length
      if (idif .lt. 0) goto 9000
c------ IF NECESSARY, BACK FILL WITH BLANKS
      if (idif .eq. 0) goto 1000
 1000 inchar(icmark:icmark) = iquote
      icmark = icmark + 1
      icmark2 = (icmark + itp) - 1
      inchar(icmark:icmark2) = input(imark1:imark2)
      icmark2 = icmark2 + 1
      inchar(icmark2:icmark2) = iquote
      icmark2 = icmark2 + 1
 1200 inchar(icmark2:icmark2) = ','
      icmark = icmark2 + 1
      imark1 = imark2 + 2
      return 
 9000 write(unit=6, fmt=9010) input(imark1:imark2), itp
 9010 format(1x,a,/,40h THIS CHARACTER VARIABLE IS LONGER THAN ,i5)
      ierr = 1
      return 
      end
c------ REFORMAT INPUT SO THAT ALL ELEMENTS ARE SEPARATED BY ONE COMMA
c------ REMOVE BLANKS UNLESS SURROUNDED BY QUOTES
c------ IF ICOMMA EQUALS 1, THEN ADD MANY COMMAS TO END OF INPU
      subroutine compress(input, nc, inpu, ncles, iquote, icomma)
      character input*(*), inpu*(*), iquote*1
      next = 1
      mark = 0
   50 mark = mark + 1
c------ MOVE POINTER TO FIRST NON-BLANK CHARACTER
      if (mark .eq. (nc + 1)) goto 1000
   60 call nextnonbk(input, nc, mark)
      if (input(mark:mark) .eq. iquote) goto 100
c------ TRANSFER NUMERICAL FIELD
      if (input(mark:mark) .eq. ',') goto 300
c      PRINT *, 'INPU(NEXT:NEXT), NEXT, INPUT(MARK:MARK), MARK, NC'
c      PRINT *,  INPU(NEXT:NEXT), NEXT, INPUT(MARK:MARK), MARK, NC
   70 inpu(next:next) = input(mark:mark)
      next = next + 1
      mark = mark + 1
      if (mark .eq. (nc + 1)) then
      next = next + 1
      goto 1000
      end if
      if ((input(mark:mark) .eq. ' ') .or. (input(mark:mark) .eq. iquote
     &)) goto 250
      if (input(mark:mark) .eq. ',') goto 300
c------ TRANSFER QUOTED STRING
      goto 70
  100 iqend = 0
  110 inpu(next:next) = input(mark:mark)
      next = next + 1
      mark = mark + 1
      if (mark .eq. (nc + 1)) goto 1000
      if (iqend .eq. 1) goto 200
      if (input(mark:mark) .eq. iquote) iqend = 1
c------ TRANSFER DIVIDER
      goto 110
  200 call nextnonbk(input, nc, mark)
c------ REDUCE MARK BY ONE
      if (input(mark:mark) .eq. ',') goto 300
  250 mark = mark - 1
c------ TRANSFER COMMA AND GET NEXT FIELD
      if (mark .eq. (nc + 1)) goto 1000
  300 inpu(next:next) = ','
      next = next + 1
      mark = mark + 1
      if (mark .eq. (nc + 1)) goto 1000
      goto 60
 1000 ncles = next - 1
      if (icomma .eq. 0) return 
      do 2000 i = ncles, 132
 2000 inpu(i:i) = ','
      ncles = 132
      end
c------ MOVE POINTER TO NEXT NON-BLANK CHARACTER
      subroutine nextnonbk(input, nc, mark)
      character input*(*)
      do 100 i = mark, nc
      if (input(i:i) .ne. ' ') goto 200
  100 continue
      mark = nc
      return 
  200 mark = i
      return 
      end
