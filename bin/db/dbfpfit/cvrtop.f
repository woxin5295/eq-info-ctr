      subroutine cvrtop(x, y, r, theta)
c
c-------- bruce julian
c
c-------- cvrtop - convert from rectangular to polar coordinates
c
c-------- standard fortran function required:  atan2
c-------- function required:  hypot
c
      r = hypot(x, y)
      theta = 0.
      if((y .ne. 0.) .or. (x .ne. 0.)) theta = atan2(y, x)
      return
      end
c
      real function hypot(a, b)
c
c-------- bruce julian
c
c
c-------- hypot - calculates euclidian distance, accurately and
c            avoids overflow
c
      real a, b
      real abs, l, s, t, sqrt
      l = abs(a)
      s = abs(b)
      if (s .le. l) goto 1
         t = s
         s = l
         l = t
   1  if (l .ne. 0.0) goto 2
         hypot = 0.0
         return
   2  s = s/l
      hypot = l*sqrt(s*s+1.0)
      return
      end

