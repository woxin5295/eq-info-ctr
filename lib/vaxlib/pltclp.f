c
c+
c
c     Subroutine PLCLIP (vector, window, iclip)
c
c PLCLIP - Clip vector to plotting window.
c
c Call PLCLIP (vector,window,iclip)
c
c      vector = The vector to be clipped (in place)
c               (vector(1,1),vector(2,1)) to (vector(1,2),vector(2,2))
c      window = Clipping limits (X-Min,Y-Min), (X-Max,Y-Max)
c      iclip  = 0 If vector good
c             = 1 If vector clipped once
c             = 2 If vector clipped twice
c             = 3 If vector bad
c
c       Called by:  PLPLOT, PLINE1, PLINE2
c
c           Calls:  None
c
c Parameters used:  None
c
c    Commons used:  None
c
c-
c
      subroutine pltclp(xold, yold, xnew, ynew, xmin, xmax, ymin, ymax, 
     &clipon, iclip)
      dimension vector(2, 2), window(2, 2)
      logical clipon
# 32 "pltclp.for"
      if (.not. clipon) return 
      vector(1,1) = xold
      vector(2,1) = yold
      vector(1,2) = xnew
      vector(2,2) = ynew
      window(1,1) = xmin
      window(1,2) = xmax
      window(2,1) = ymin
c
c...  Vector limits check and clipping
c
c...  Test each coordinate
# 40 "pltclp.for"
      window(2,2) = ymax
# 45 "pltclp.for"
      iclip = 0
      do 100 i = 1, 2
c
c...     Test individual ordinates
# 47 "pltclp.for"
      nerr = 0
c
c...        Test upper limits
# 50 "pltclp.for"
   50 do 90 j = 1, 2
c
c...        Upper limit OK?
# 53 "pltclp.for"
      almt = window(j,2)
c
c...           Upper limit bad?
# 56 "pltclp.for"
      if (vector(j,i) .gt. almt) then
# 59 "pltclp.for"
      if (vector(j,3 - i) .gt. almt) then
      goto 9900
      else
      goto 70
      end if
c
c...        Test lower limits
# 64 "pltclp.for"
      end if
c
c...        Lower limit OK?
# 67 "pltclp.for"
      almt = window(j,1)
c
c...           Lower limit bad?
# 70 "pltclp.for"
      if (vector(j,i) .lt. almt) then
c
c...           Ordinate out of range - attempt intersection
# 73 "pltclp.for"
      if (vector(j,3 - i) .lt. almt) goto 9900
c
c...           Intersection failure?
# 76 "pltclp.for"
   70 nerr = nerr + 1
# 79 "pltclp.for"
      if (nerr .gt. 2) then
      goto 9900
      else
      vector(3 - j,i) = (((vector(3 - j,1) - vector(3 - j,2)) / (vector(
     &j,1) - vector(j,2))) * (almt - vector(j,1))) + vector(3 - j,1)
# 85 "pltclp.for"
      vector(j,i) = almt
      goto 50
      end if
      end if
c
c...     Test for any clipping done for this point
# 89 "pltclp.for"
   90 continue
c
  100 if (nerr .gt. 0) iclip = iclip + 1
# 94 "pltclp.for"
 9000 xnew = vector(1,2)
      ynew = vector(2,2)
      xold = vector(1,1)
      yold = vector(2,1)
c
c...  Intersection failure
# 98 "pltclp.for"
      return 
# 101 "pltclp.for"
 9900 iclip = 3
c
# 102 "pltclp.for"
      goto 9000
# 104 "pltclp.for"
      end
