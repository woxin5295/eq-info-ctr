c     THE WADATI SUBROUTINE USES P AND S ARRIVAL TIMES THAT ARE ENTERED
c     TO THE NEAREST .01 SEC IN SINGLE PRECISION.  THE PURPOSE OF THIS
c     SUBROUTINE IS TO CONVERT THESE NUMBERS TO THE NEAREST DOUBLE
c     PRECISION NUMBER, SETTING LESS SIGNIFICANT DECMIAL DIGITS TO ZERO.
c     THIS VERSION IS 16 TIMES FASTER THAN DUBL1, WHICH USES INTERNAL
c     WRITE AND READ STATEMENTS.
c     LAHR & STEPHENS   FEB 1986
      double precision function dubl(x)
c
c      PRINT *, 'DUBL COMPUTES FACTOR USING LOGS'
      double precision dfact
# 12 "dubl.for"
      absx = abs(x)
      al = alog10(absx)
c      IF(AL .LT. 0.)  AL = AL + .0000005
# 14 "dubl.for"
      if (al .lt. 0.) al = al - .9999995
c      PRINT '(A6, E30.15, I5)', 'AL = ', AL, I
# 16 "dubl.for"
      i = al
c      PRINT *, 'MULTIPLY NUMBER BY ', DFACT, ' AND TRUNCATE.'
# 18 "dubl.for"
      dfact = 10.d0 ** (7 - i)
c      PRINT *, 'IX = ', IX
# 20 "dubl.for"
      ix = (x * dfact) + sign(.5,x)
# 22 "dubl.for"
      dubl = dble(ix) / dfact
      return 
      end
