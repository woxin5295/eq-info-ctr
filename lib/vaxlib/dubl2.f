c     THE PURPOSE IS TO EXCLUDE GARBAGE FROM BEYOND THE LEAST SIGNIFICAN
cT
c     DIGIT ON CONVERSION OF A NUMBER FROM REAL*4 TO REAL*8
c     LIMITED TO NUMBERS GREATER THAN ABOUT 10**-7
c     LAHR & STEPHENS   FEB 1986
c
c      PRINT *, 'DUBL2 COMPUTES FACTOR WITH DO LOOP'
      double precision function dubl2(x)
# 8 "dubl2.for"
      i = -7
      absx = abs(x)
   10 if (absx .gt. (10 ** i)) then
      i = i + 1
      goto 10
      end if
c      PRINT *, 'IEX = ', IEX
# 14 "dubl2.for"
      iex = 7 - i
c      PRINT *, 'MULTIPLY NUMBER BY ', FACT, ' AND TRUNCATE.'
# 16 "dubl2.for"
      fact = 10. ** iex
c      PRINT *, 'IX = ', IX
# 18 "dubl2.for"
      ix = (x * fact) + .4
# 20 "dubl2.for"
      dubl2 = dble(ix) / fact
      return 
      end
