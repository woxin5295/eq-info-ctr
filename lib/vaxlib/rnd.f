c rnd.for
      function rnd(idum)
c     acm algorithm 267 by m.c.pike
c     ran3 is a random number generator from the book numerical recipes by
c     press and others that gives a random distribution over the
c     interval 0 to 1.
c     this function computes a normal distribution with mean zero and
c     standard deviation 1.0.
c     use a negative number for idum to reset seed.
      if(idum .lt. 0) then
        f = ran3(idum)
      endif
      f = ran3(0)
      x1 = sqrt(-2.*alog(f))
      t = 6.2831853072*ran3(0)
      rnd = x1*sin(t)
      return
      end
c end rnd
