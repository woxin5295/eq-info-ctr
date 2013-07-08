c     TEST DECOD
      character chararray(5)*4, inpu*40, iquote*1, inchar*80, inint*80
      character inreal*80, input*80
      dimension intarray(2), realarray(2), it(4)
# 5 "dec.for"
      inpu = '2 2 ABC DE Z                                          '
      it(1) = 82
      it(2) = 82
      it(3) = 4
      it(4) = -4
      it(5) = 1
      it(6) = 1
      it(7) = 1
      ntot = 3
      ncs = 3
      call decod(inpu, 40, 7, 5, chararray, 0, intarray, 2, realarray, 
     &it, 1, ierr)
      write(unit=*, fmt=*) chararray
      write(unit=*, fmt=*) realarray
      call decod(inpu, 40, 2, 0, 0, 0, 0, 2, realarray, it, 1, ierr)
      write(unit=*, fmt=*) realarray
      end
c------ FREE FORMAT DECODING OF A CHARACTER STRING
c     INPU             CHARACTER*(NCS)     INPUT CHARACTER STRING
c     NCS              INTEGER             LENGTH OF INPU
c     NTOT             INTEGER             TOTAL NUMBER OF VARIABLES
c     NCHAR            INTEGER             NUMBER OF CHARACTER VARIABLES
c    
c     CHARARRAY        CHARACTER ARRAY     OUTPUT ARRAY OF CHAR VARIABLE
cS
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
      dimension intarray(*), realarray(*), it(*), itp(100), lndc(100)
      data quote / '''' /
      data init / 0 /
# 47 "dec.for"
      do 9998 i = 1, ntot
      itp(i) = it(i)
c--OPEN SCRATCH FILE FOR FREE FORMAT DECODING OF PARAMETERS.
# 49 "dec.for"
 9998 continue
# 51 "dec.for"
      if (init .eq. 1) goto 10
      init = 1
      write(unit=iquote, fmt=5) quote
    5 format(a1)
c     1STATUS='SCRATCH',CARRIAGECONTROL='NONE') !DOES NOT WORK IN WE:[SE
cISCR]
# 55 "dec.for"
      open(unit=8, file='FREE.XXX', form='FORMATTED', status='NEW') 
# 58 "dec.for"
   10 ierr = 0
c      WRITE(6,300) INPU,INPUT(1:NC)
c300   FORMAT(2(1X,A/))
# 59 "dec.for"
      call compress(inpu, ncs, input, nc, iquote, icomma)
# 62 "dec.for"
      imark = 1
      icmark = 1
      iimark = 1
      irmark = 1
      ndc = 0
      do 500 i = 1, ntot
      if (itp(i) .eq. 81) goto 100
      if (itp(i) .eq. 82) goto 200
c------ STRIP OFF ONE CHARACTER VARIABLE WITH LENGTH ITP(I)
# 70 "dec.for"
      if (iabs(itp(i)) .gt. 132) itp(i) = 132 * isign(1,itp(i))
# 72 "dec.for"
      if (input(imark:imark) .ne. iquote) goto 95
      call strip(input, nc, imark, inchar, icmark)
      goto 98
   95 call stripc(input, nc, imark, inchar, icmark, iabs(itp(i)), iquote
     &, ierr)
      if (ierr .eq. 1) return 
   98 continue
      ndc = ndc + 1
c      WRITE(6,9876) INCHAR(1:ICMARK),NCHAR
# 80 "dec.for"
      lndc(ndc) = itp(i)
c------ STRIP OFF ONE INTEGER VALUE
# 82 "dec.for"
      goto 500
c      WRITE(6,9876) ININT(1:IIMARK),NINT
c9876  FORMAT(1X,A,I10)
# 84 "dec.for"
  100 call strip(input, nc, imark, inint, iimark)
c------ STRIP OFF ONE REAL VALUE
      goto 500
c      WRITE(6,9876) INREAL(1:IRMARK),NREAL
  200 call strip(input, nc, imark, inreal, irmark)
c
c------ READ CHARACTER VARIABLES
# 91 "dec.for"
  500 continue
# 94 "dec.for"
      if (nchar .eq. 0) goto 600
      rewind(unit=8) 
      write(unit=8, fmt=550) inchar
  550 format(a,1h )
      rewind(unit=8) 
      do 552 i = 1, nchar
  552 chararray(i) = ' '
      read(unit=8, fmt=*, err=9000) (chararray(i),i = 1, nchar)
      do 553 i = 1, nchar
      if (lndc(i) .lt. 0) goto 553
      call right(chararray(i)(1:iabs(lndc(i))))
c------ READ INTEGER VARIABLES
# 105 "dec.for"
  553 continue
# 107 "dec.for"
  600 if (nint .eq. 0) goto 700
      rewind(unit=8) 
      write(unit=8, fmt=550) inint
      rewind(unit=8) 
      do 602 i = 1, nint
  602 intarray(i) = 0
c------ READ REAL VARIABLES
# 113 "dec.for"
      read(unit=8, fmt=*, err=9000) (intarray(i),i = 1, nint)
# 115 "dec.for"
  700 if (nreal .eq. 0) return 
      rewind(unit=8) 
      write(unit=8, fmt=550) inreal
      rewind(unit=8) 
      do 702 i = 1, nreal
  702 realarray(i) = 0.
      read(unit=8, fmt=*, err=9000) (realarray(i),i = 1, nreal)
      return 
 9000 rewind(unit=8) 
      read(unit=8, fmt=9010) inreal
 9010 format(a)
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
# 139 "dec.for"
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
# 153 "dec.for"
      ierr = 0
# 155 "dec.for"
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
c      DO 900 I = 1,IDIF
c      IMARK1 = IMARK1 - 1
c900   INPUT(IMARK1:IMARK1) = ' '
# 165 "dec.for"
      if (idif .eq. 0) goto 1000
# 170 "dec.for"
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
# 191 "dec.for"
      next = 1
      mark = 0
   50 mark = mark + 1
c------ MOVE POINTER TO FIRST NON-BLANK CHARACTER
# 194 "dec.for"
      if (mark .eq. (nc + 1)) goto 1000
# 196 "dec.for"
   60 call nextnonbk(input, nc, mark)
      if (input(mark:mark) .eq. iquote) goto 100
c------ TRANSFER NUMERICAL FIELD
# 198 "dec.for"
      if (input(mark:mark) .eq. ',') goto 300
c      PRINT *, 'INPU(NEXT:NEXT), NEXT, INPUT(MARK:MARK), MARK, NC'
c      PRINT *,  INPU(NEXT:NEXT), NEXT, INPUT(MARK:MARK), MARK, NC
# 200 "dec.for"
   70 inpu(next:next) = input(mark:mark)
# 203 "dec.for"
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
# 212 "dec.for"
      goto 70
# 214 "dec.for"
  100 iqend = 0
  110 inpu(next:next) = input(mark:mark)
      next = next + 1
      mark = mark + 1
      if (mark .eq. (nc + 1)) goto 1000
      if (iqend .eq. 1) goto 200
      if (input(mark:mark) .eq. iquote) iqend = 1
c------ TRANSFER DIVIDER
# 221 "dec.for"
      goto 110
# 223 "dec.for"
  200 call nextnonbk(input, nc, mark)
c------ REDUCE MARK BY ONE
# 224 "dec.for"
      if (input(mark:mark) .eq. ',') goto 300
# 226 "dec.for"
  250 mark = mark - 1
c------ TRANSFER COMMA AND GET NEXT FIELD
# 227 "dec.for"
      if (mark .eq. (nc + 1)) goto 1000
# 229 "dec.for"
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
# 243 "dec.for"
      do 100 i = mark, nc
      if (input(i:i) .ne. ' ') goto 200
  100 continue
      mark = nc
      return 
  200 mark = i
      return 
      end
