c TEST DRIVER FOR FUNCTION INSIDE - J. C. LAHR    18 MAY 1986
c     dimension px(4), py(4)
c     px(1) = 0.
c     py(1) = 0.
c     px(2) = 10.
c     py(2) = 0.
c     px(3) = 10.
c     py(3) = 10.
c     px(4) = 0.
c     py(4) = 10.
c  20 x0 = raskk('XVALUE',x0)
c     y0 = raskk('YVALUE',y0)
c     ini = inside(x0,y0,px,py,4)
c     write(unit=*, fmt=*) ini
c     if (ini .eq. 0) write(unit=*, fmt=*) 'POINT IS OUTSIDE'
c     if (ini .eq. 1) write(unit=*, fmt=*) 'POINT AT VERTEX'
c     if (ini .eq. 4) write(unit=*, fmt=*) 
c    &'FOR COUNTER CLOCKWISE REGION, POINT ON B0TTOM OR RIGHT SIDE'
c     if (ini .eq. (-4)) write(unit=*, fmt=*) 
c    &'FOR COUNTER CLOCKWISE REGION, POINT ON TOP OR LEFT SIDE'
c     if (ini .eq. (-2)) write(unit=*, fmt=*) 
c    &'POINT IS INSIDE COUNTER-CLOCKWISE REGION'
c     if (ini .eq. 2) write(unit=*, fmt=*) 
c    &'POINT IS INSIDE CLOCKWISE REGION'
c     goto 20
c     end
c
c     CHECK IF POINT X0, Y0 IS INSIDE POLYGON PX(I), P(Y), I = 1 TO N
c     (N IS THE NUMBER OF VERTACES - EG. FOR A RECTANGULAR, N = 4)
c     BASED ON GODKIN & PULLI, PSSA 74, P. 1845-1848, 1984.
c     CONVERTED TO FORTRAN FROM B. JULIAN'S C IMPLEMENTATION 
c     BY J. C. LAHR  -  18 MAY 1986
c     RETURNS 0 IF POINT OUTSIDE POLYGON
c             1 IF POINT AT VERTEX
c           +-4 IF POINT ON AN EDGE
c           +-2 IF POINT INSIDE
c
      integer function inside(x0, y0, px, py, n)
      logical vertex
      dimension px(n), py(n)
# 40 "tinside.for"
      inside = 0
      vertex = .false.
      x1 = px(n) - x0
      y1 = py(n) - y0
c       PRINT *, '******************>> LINE SEGMENT ', I, VERTEX
# 44 "tinside.for"
      do 20 i = 1, n
# 46 "tinside.for"
      x2 = px(i) - x0
      y2 = py(i) - y0
      y1y2 = y1 * y2
c         PRINT *, ' LINE CROSSES OR AT LEAST TOUCHES X AXIS'    
# 49 "tinside.for"
      if (y1y2 .le. 0.) then
c         PRINT *, 'KSI = ', KSI
# 51 "tinside.for"
      ksi = ksicr(x1,y1,x2,y2,y1y2,vertex)
# 53 "tinside.for"
      if (iabs(ksi) .eq. 4) then
      inside = ksi
      return 
      end if
      if (vertex) then
      inside = 1
      return 
      end if
      inside = inside + ksi
      end if
      x1 = x2
      y1 = y2
   20 continue
      return 
      end
c     SIGNED CROSSING NUMBER - CONVERTED TO FORTRAN BY J. C. LAHR FROM
c       B. JULIAN'S C CODE.    18 MAY 1986
c    
c       0                          :  DOES NOT CROSS -X AXIS
c       0 WITH VERTEX SET TO TRUE  :  ONE END AT POINT
c       +/-4                       :  GOES THROUGH POINT
c       +/-2                       :  CROSSES -X AXIS
c       +/-1                       :  HALF CROSSES -X AXIS
c     [TEST FOR Y1*Y2 .LE. 0. IS PERFORMED IN CALLING ROUTINE.]
c     [OTHERWISE Y1*Y2 .GT. 0. WOULD BE PLACED HERE AND RETURN ZERO.]
      integer function ksicr(x1, y1, x2, y2, y1y2, vertex)
      logical vertex
c     PRINT *, 'T = ', T
# 80 "tinside.for"
      t = (x1 * y2) - (x2 * y1)
c       PRINT *, 'LINE PASSES THROUGH OR TO POINT'
# 82 "tinside.for"
      if ((t .eq. 0.) .and. ((x1 * x2) .le. 0.)) then
c         PRINT *, 'NEITHER END OF LINE TERMINATES AT POINT'
# 84 "tinside.for"
      if (y1y2 .ne. 0.) then
c           PRINT *, 'LINE PASSES UP THROUGH POINT'
# 86 "tinside.for"
      if (y2 .gt. 0.) then
# 88 "tinside.for"
      ksicr = 4
c           PRINT *, 'LINE PASSES DOWN THROUGH POINT'
# 89 "tinside.for"
      else
# 91 "tinside.for"
      ksicr = -4
      end if
      return 
c         PRINT *, 'Y1*Y2 = 0. AND X1*X2 = 0.'
# 94 "tinside.for"
      else if ((x1 * x2) .eq. 0.) then
c         PRINT *, 'VERTEX = ', VERTEX
# 96 "tinside.for"
      vertex = .true.
# 98 "tinside.for"
      else
c           PRINT *, 'LINE PASSES TO RIGHT THROUGH POINT'
# 99 "tinside.for"
      if (x1 .lt. x2) then
# 101 "tinside.for"
      ksicr = 4
c           PRINT *, 'LINE PASSES TO LEFT THROUGH POINT'
# 102 "tinside.for"
      else
# 104 "tinside.for"
      ksicr = -4
      end if
      return 
      end if
c       PRINT *, 'COMPLETE CROSSING OF X AXIS'
# 108 "tinside.for"
      else if (y1y2 .lt. 0.) then
c         PRINT *, 'COMPLETE CROSSNG OF -X ASIX'
# 110 "tinside.for"
      if ((t * y2) .lt. 0) then
# 112 "tinside.for"
      if (y2 .gt. 0.) then
      ksicr = 2
      else
      ksicr = -2
      end if
      return 
      end if
c       PRINT *, 'HALF CROSSING, Y1 EQUALS 0'
# 119 "tinside.for"
      else if (y1 .eq. 0.) then
# 121 "tinside.for"
      if ((x1 .lt. 0) .and. (y2 .ne. 0.)) then
      if (y2 .gt. 0.) then
      ksicr = 1
      else
      ksicr = -1
      end if
      return 
      end if
c       PRINT *, 'HALF CROSSING, Y2 MUST EQUAL 0'
# 129 "tinside.for"
      else
# 131 "tinside.for"
      if (x2 .lt. 0) then
      if (y1 .lt. 0) then
      ksicr = 1
      else
      ksicr = -1
      end if
      return 
      end if
      end if
      ksicr = 0
      return 
      end
