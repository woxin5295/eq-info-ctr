      subroutine back1 (delt, azzz, newlat, newlon, slat, slon)
c
c-------- back1- calculate geocentric coordinates of secondary point from 
c            step in of delt degrees in direction azzz
c
c input:  delt      change in earthquake position
c         azzz      direction of move clockwise from north
c output: newlat    new earthquake geocentric latitude in radians
c         newlon    new earthquake longitude in radians
c
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (polrad = 6356.5838)
      real newlat,newlon
      st0 = cos(slat)
      ct0 = sin(slat)
      delta = delt*rad
      az1 = -azzz*rad
      if (az1 .lt. 0.0) az1 = az1 + twopi
      sdelt = sin(delta)
      cdelt = cos(delta)
      cz0 = cos(az1)
      ct1 = st0*sdelt*cz0+ct0*cdelt
      call cvrtop(st0*cdelt-ct0*sdelt*cz0, sdelt*sin(az1), st1, dlon)
      newlat = atan2(ct1, st1)
      newlon = slon + dlon
      if (abs(newlon) .gt. pi) newlon = newlon - sign(twopi, newlon)
      return
      end

