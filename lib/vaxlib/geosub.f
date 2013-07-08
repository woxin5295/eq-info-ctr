      function cvlat(alat)
c
c convert between geographic and geocentric latitude
c     (modified by c. stephens from version written by b. julian)
c
c       flat = 3.3529e-03
      data halfpi/1.570796/
c       c1 = (1.0 - flat)**2
      data c1/9.933054e-01/
c       c2 = halfpi*(1.0/c1 - 1.0)
      data c2/1.058656e-02/
      data thmax/0.02/
      cvlat = 1.
c
c       ggtogc - convert from geographic to geocentric latitude
c
      entry ggtogc(alat)
      if ((halfpi - abs(alat)) .lt. thmax) go to 1
      ggtogc = atan(c1*tan(alat))
      return
c
    1 ggtogc = alat/c1 - sign(c2,alat)
      return
c
c       gctogg - convert from geocentric to geographic latitude
c
      entry gctogg(alat)
      if ((halfpi - abs(alat)) .lt. thmax) go to 2
      gctogg = atan(tan(alat)/c1)
      return
c
    2 gctogg = c1*(alat + sign(c2,alat))
      return
      end
c

c
c sign convention is north and west positive
c
      subroutine distaz(alat, alon, slat, slon, dedeg, az0, az1)
c
c-------- distaz - calculate the distance, azimuth, and back
c           azimuth in degrees
c
c input:  alat     event geocentric latitude (radians)
c         alon     event geocentric longitude (radians)
c         slat     station geocentric latitude (radians)
c         slon     station geocentric longitude (radians)
c output: dedeg    distance form earthqauke to station in degrees
c         az0      azimuth from earthquake to station measured clockwise 
c                     from north, in degrees
c         az1      back azimuth
c
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
c
      st0 = cos(alat)
      ct0 = sin(alat)
c---- use approximation of local radius for derivative of surface
c----     distance with geocentric latitude.
c----  more accurate formulation would be:
c      drdth = -r**3 * cos(alat)*sin(alat)*( (1.-flat)**(-2) - 1. )/a**2
c      dsd = sqrt(drdth**2 + r**2)
      radius = (cos(alat)**2/equrad**2 + sin(alat)**2/polrad**2)**(-.5)
      ct1 = sin(slat)
      st1 = cos(slat)
      sdlon = sin(alon - slon)
      cdlon = cos(alon - slon)
      cdelt = st0*st1*cdlon+ct0*ct1
      call cvrtop(st0*ct1-st1*ct0*cdlon, st1*sdlon, sdelt, az0)
      dedeg = atan2(sdelt, cdelt)*deg
      dekm = radius*atan2(sdelt, cdelt)
      if (az0 .lt. 0.0) az0 = az0 + twopi
      if (az0 .ge. twopi) az0 = az0 - twopi
      az0 = az0*deg
c---- calculation of back azimuth if needed
      call cvrtop(st1*ct0 - st0*ct1*cdlon, (-sdlon)*st0, sdelt, az1)
      if (az1 .lt. 0.0) az1 = az1 + twopi
      if (az1 .ge. twopi) az1 = az1 - twopi
      az1 = az1*deg
      return
      end
      subroutine delaz(alat, alon, dekm, dedeg, az0, slat, slon)
c
c-------- delaz - calculate the distance in km (approx equal to geocentric 
c      distance times local radius), and azimuths in degrees
c
c input:  alat     earthquake geocentric latitude in radians
c         alat     earthquake geocentric longitude in radians
c         slat     station geocentric latitude in radians
c         slon     station geocentric longitude in radians
c output: dekm     distance from earthquake to station in kilometers
c         dedeg    distance form earthqauke to station in degrees
c         az0      azimuth in degrees from earthquake to station measured clockwise 
c                     from north
c
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
c
      st0 = cos(alat)
      ct0 = sin(alat)
c---- use approximation of local radius for derivative of surface
c----     distance with geocentric latitude.
c----  more accurate formulation would be:
c      drdth = -r**3 * cos(alat)*sin(alat)*( (1.-flat)**(-2) - 1. )/a**2
c      dsd = sqrt(drdth**2 + r**2)
      radius = (cos(alat)**2/equrad**2 + sin(alat)**2/polrad**2)**(-.5)
      ct1 = sin(slat)
      st1 = cos(slat)
      sdlon = sin(alon - slon)
      cdlon = cos(alon - slon)
      cdelt = st0*st1*cdlon+ct0*ct1
      call cvrtop(st0*ct1-st1*ct0*cdlon, st1*sdlon, sdelt, az0)
      dedeg = atan2(sdelt, cdelt)*deg
      dekm = radius*atan2(sdelt, cdelt)
      if (az0 .lt. 0.0) az0 = az0 + twopi
      if (az0 .ge. twopi) az0 = az0 - twopi
      az0 = az0*deg
c---- calculation of back azimuth if needed
c      call cvrtop(st1*ct0 - st0*ct1*cdlon, (-sdlon)*st0, sdelt, az1)
c      if (az1 .lt. 0.0) az1 = az1 + twopi
c      if (az1 .ge. twopi) az1 = az1 - twopi
c      az1 = az1*deg
      return
      end
      subroutine back (delat, delon, newlat, newlon, slat, slon)
c
c-------- back - calculate geocentric coordinates of secondary point from 
c            step in latitude (km) and longitude (km)
c
c input:  delat     change in earthquake latitude in km (northward positive)
c         delon     change in earthquake longitude in km (westward positive)
c         slat      original latitude in radians
c         slon      original longitude in radians
c output: newlat    new earthquake geocentric latitude in radians
c         newlon    new earthquake longitude in radians
c
      real newlat,newlon
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
c
      st0 = cos(slat)
      ct0 = sin(slat)
      call cvrtop(delat,delon,delta,az1)
      if (az1 .lt. 0.0) az1 = az1 + twopi
c---- use approximation of local radius for derivative of surface
c----     distance with geocentric latitude.
c----  more accurate formulation would be:
c      drdth = -r**3 * cos(alat)*sin(alat)*( (1.-flat)**(-2) - 1. )/a**2
c      dsd = sqrt(drdth**2 + r**2)
      radius = (cos(slat)**2/equrad**2 + sin(slat)**2/polrad**2)**(-.5)
      sdelt = sin(delta/radius)
      cdelt = cos(delta/radius)
      cz0 = cos(az1)
      ct1 = st0*sdelt*cz0+ct0*cdelt
      call cvrtop(st0*cdelt-ct0*sdelt*cz0, sdelt*sin(az1), st1, dlon)
      newlat = atan2(ct1, st1)
      newlon = slon + dlon
      if (abs(newlon) .gt. pi) newlon = newlon - sign(twopi, newlon)
      return
      end
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
      subroutine fold
c
c-------- given geographic coordinates compute geocentric lat and lon
c
c input:  la     degree portion of latitude in degrees
c         ins    n for north, s for south
c         ala    minutes portion of latitude 
c         lo     degree portion of longitude
c         iew    e for east, w for west
c         alo    minutes portion of longitude
c output: alat   geocentric latitude in radians
c         alon   geocentric longitude in radians
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
c      parameter (equrad = 6378.2064)
c      parameter (polrad = 6356.5838)
c      parameter (flat = (equrad - polrad)/equrad)
c      parameter (c1 = (1.0 - flat)**2)
      parameter (c1 = .9932773)
      parameter (c2 = halfpi*(1.0/c1 - 1.0))
c
      character*1 ins,iew,dnstrng
c
      entry fold1(gcla, gclo, ggla, gglo)
      alat = ggla*rad
c---- ggtogc - convert from geographic to geocentric latitude
      if(halfpi - abs(alat) .ge. 0.02) then
        gcla = atan(c1*tan(alat))
      else
        gcla = alat/c1-sign(c2,alat)
      endif
c     gcla = atan2( c1*sin(alat), cos(alat) )
      gclo = gglo*rad
      return
c
      entry fold2(gcla, gclo, la, ins, ala, lo, iew, alo)
c
      alat = (la + ala*1.6666667e-2)*rad
c---- ggtogc - convert from geographic to geocentric latitude
      if(halfpi - abs(alat) .ge. 0.02) then 
        gcla = atan(c1*tan(alat)) 
      else 
        gcla = alat/c1-sign(c2,alat) 
      endif 
c     gcla = atan2( c1*sin(alat), cos(alat) )
      if(dnstrng(ins) .eq. 's') gcla = -gcla
      gclo = (lo + alo*1.6666667e-2)*rad
      if(dnstrng(iew) .eq. 'e') gclo = - gclo
      return
      end
      subroutine unfold
c
c-------- given geocentric lat and lon compute geographic coordinates
c            suitable  for printing
c
c input and output definition just reverse of entry fold
c
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
c      parameter (equrad = 6378.2064)
c      parameter (polrad = 6356.5838)
c      parameter (flat = (equrad - polrad)/equrad)
c      parameter (c1 = (1.0 - flat)**2)
      parameter (c1 = .9932773)
      parameter (c2 = halfpi*(1.0/c1 - 1.0))
c
      character*1 ins,iew
c
      entry unfold1(alat,alon,degla,deglo)
c     input:
c      alat     geocentric lat (radians)
c      alon     geocentric lon (radians)
c     output:
c      degla    geographic lat (degrees)
c      deglo    geographic lon (degrees)
c
c---- gctogg - convert from geocentric to geographic latitude
      if (halfpi-abs(alat) .ge. 0.02) then
        blat = atan(tan(alat)/c1)
      else
        blat = c1*(alat + sign(c2,alat))
      endif
c     blat = atan2( sin(alat)/c1, cos(alat) )
      degla = blat*deg
      deglo = alon*deg
      return
c
      entry unfold2(alat,alon,la,ins,ala,lo,iew,alo)
c
c---- convert from geocentric lat and lon to geographic
c
c     input:
c      alat     geocentric lat (radians)
c      alon     geocentric lon (radians)
c     output:
c      la,ins,ala  geographic lat (deg and min)
c      lo,iew,alo  geographic lon (deg and min)
c
c---- gctogg - convert from geocentric to geographic latitude
      if (halfpi-abs(alat) .ge. 0.02) then
        blat = atan(tan(alat)/c1) 
      else 
        blat = c1*(alat + sign(c2,alat)) 
      endif 
c     blat = atan2( sin(alat)/c1, cos(alat) )
      ins = 'n'
      iew = 'w'
      ala1 = blat*deg
      la = ala1
      ala = abs(ala1 - la)*60.
      la = iabs(la)
      if(ala1 .lt. 0.0) ins = 's'
      alo1 = alon*deg
      lo = alo1
      alo = abs(alo1 - lo)*60.
      lo = iabs(lo)
      if(alo1 .lt. 0.0) iew = 'e'
      return
      end
c
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

