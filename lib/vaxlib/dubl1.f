c     THE WADATI SUBROUTINE USES P AND S ARRIVAL TIMES THAT ARE ENTERED
c     TO THE NEAREST .01 SEC IN SINGLE PRECISION.  THE PURPOSE OF THIS
c     SUBROUTINE IS TO CONVERT THESE NUMBERS TO THE NEAREST DOUBLE
c     PRECISION NUMBER, SETTING LESS SIGNIFICANT DECMIAL DIGITS TO ZERO.
c     THIS VERSION IS 16 TIMES SLOWER THAN DUBL, WHICH USES LOGS.
c     LAHR & STEPHENS   FEB 1986
      double precision function dubl1(x)
      character temp*15
# 9 "dubl1.for"
      write(unit=temp, fmt=10) x
   10 format(e15.8)
      read(unit=temp, fmt=20) dubl1
   20 format(d20.10)
      return 
      end
