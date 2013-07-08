c     THIS REPLACES SYSTEM DBLE FUNCTION THAT IS APPARENTLY BROKEN!
c     THE PURPOSE IS TO EXCLUDE GARBAGE FROM BEYOND THE LEAST SIGNIFICAN
cT
c     DIGIT ON CONVERSION OF A NUMBER FROM REAL*4 TO READ*8
c     LIMITED TO NUMBERS GREATER THAN ABOUT 10**-7
c     LAHR & STEPHENS   FEB 1986
      double precision function dble(x)
      character temp*14
# 8 "dble.for"
      write(unit=temp, fmt=10) x
   10 format(e14.7)
      read(unit=temp, fmt=20) dble
   20 format(d20.10)
      return 
      end
