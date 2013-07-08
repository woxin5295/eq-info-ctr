c    TILTDAZ     5/16/84    J. C. LAHR
      character nors*1, eorw*1, aask*1
# 3 "tiltdaz.for"
      write(unit=6, fmt=100) 
  100 format(44h WELCOME TO TILTDAZ!  THIS PROGRAM WILL TEST,/
     &42h THE SUBROUTINE TILTDAZ WHICH COMPUTES THE,/
     &47h COORDINATES OF A POINT IN A TILTED CYLINDRICAL,/
     &19h COORDINATE SYSTEM.)
      clat = 60.
      clon = -150.
      az = 0.
      dip = 90.
      nors = 'N'
      lat = 60
      alat = 0.
      eorw = 'W'
      lon = 150
      alon = 0.
      z = 0.
  200 clat = raskk('LATITUDE OF COORDINATE REFERENCE POINT (N POS)',clat
     &)
      clon = raskk('LONGITUDE (W NEG)',clon)
      az = raskk('STRIKE OF CYLINDER AXIS (DEG)',az)
      dip = raskk('DIP OF CYLINDER AXIS',dip)
      write(unit=*, fmt=*) 'COORDINATES OF ARBITRARY POINT'
      nors = aask('N OR S LATITUDE',nors,-1)
      lat = iaskk('DEGREE PORTION OF LATITUDE',lat)
      alat = raskk('MINUTES PROTION',alat)
      eorw = aask('E OR W LONGITUDE',eorw,-1)
      lon = iaskk('DEGREE PORTION OF LONGITUDE',lon)
      alon = raskk('MINUTES PORTION',alon)
      z = raskk('DEPTH',z)
      call tiltdaz(clat, clon, az, dip, lat, nors, alat, lon, eorw, alon
     &, z, zz, azpr, hpr)
      write(unit=*, fmt=*) 'ZZ, AZPR, HPR', zz, azpr, hpr
      goto 200
      end
c             J. C. LAHR     5/16/84
c  INPUT:
c     CLATD, CLOND         COORDINATES OF CENTER OF NEW COORDINATE SYSTE
cM
c                            (N POS AND W NEG)   (DEGREES)
c     AZD, DIPD            DOWN DIP AZIMUTH AND DIP OF AXIS OF CYLINDRIC
cAL
c                            COORDINATE SYSTEM
c     LAT, NORS, ALAT      LATITUDE OF POINT TO BE CONVERTED TO NEW SYST
cEM
c     LON, EORW, ALON      LONGITUDE " "
c     Z                    DEPTH     " "
c  OUTPUT:
c     ZZ, AZPR             DEPTH (KM), AZIMUTH (DEG) CLOCKWISE FROM AZIM
cUTH
c                            WITH STEEPEST UPWARD DIRECTION
c     HPR                  DISTANCE (KM) FROM CYLINDER AXIS
c
      subroutine tiltdaz(clatd, clond, azd, dipd, lat, nors, alat, lon, 
     &eorw, alon, z, zz, azprd, hpr)
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0 * pi)
      parameter (halfpi = 0.5 * pi)
      parameter (rad = pi / 180.)
      parameter (deg = 1.0 / rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad) / equrad)
      parameter (c1 = (1.0 - flat) ** 2)
c
c---- CONVERT FROM DEGREES TO RADIANS
      parameter (c2 = halfpi * ((1.0 / c1) - 1.0))
# 65 "tiltdaz.for"
      clat = clatd * rad
      clon = - (clond * rad)
      az = azd * rad
c
c-------- GIVEN GEOGRAPHIC COORDINATES COMPUTE GEOCENTRIC LAT AND LON
# 68 "tiltdaz.for"
      dip = dipd * rad
# 71 "tiltdaz.for"
      call fold(plat, plon, lat, nors, alat, lon, eorw, alon)
c         PRINT *, 'GEOCENTRIC LATITUDE OF CENTER', AJUNK
c
c---- GGTOGC - CONVERT CLAT FROM GEOGRAPHIC TO GEOCENTRIC LATITUDE
# 72 "tiltdaz.for"
      ajunk = plat * deg
# 76 "tiltdaz.for"
      if ((halfpi - abs(clat)) .ge. 0.02) goto 10
      clat = (clat / c1) - sign(c2,clat)
      goto 20
   10 clat = atan(c1 * tan(clat))
   20 continue
c         PRINT *, 'GEOCENTRIC LATITUDE', AJUNK
c
c---- DELAZ - CALCULATE THE DISTANCE IN KM (APPROX EQUAL TO GEOCENTRIC 
c      DISTANCE TIMES LOCAL RADIUS), AND AZIMUTHS IN DEGREES
# 81 "tiltdaz.for"
      ajunk = clat * deg
c      PRINT *, 'DISTANCE (KM) = ', HPR1
c      PRINT *, 'AZIMUTH (DEG) = ', AZPR1
# 86 "tiltdaz.for"
      call delaz(plat, plon, hpr1, dedeg, azpr1, clat, clon)
c
c---- PROJECT ONTO PLANE CONTAINING THE CYLINDER AXIS
# 89 "tiltdaz.for"
      azpr1 = azpr1 * rad
# 92 "tiltdaz.for"
      azwxx = azpr1 - az
      yy = hpr1 * sin(azwxx)
c      PRINT *, 'XXX, Z', XXX, Z
c
c---- NOW ROTATE INTO XX, ZZ
# 94 "tiltdaz.for"
      xxx = hpr1 * cos(azwxx)
# 98 "tiltdaz.for"
      xx = (xxx * sin(dip)) - (z * cos(dip))
c      PRINT *, 'XX, YY, ZZ', XX, YY, ZZ
# 99 "tiltdaz.for"
      zz = (z * sin(dip)) + (xxx * cos(dip))
# 101 "tiltdaz.for"
      hpr = sqrt((xx * xx) + (yy * yy))
      call cvrtop(xx, yy, r, azpr)
      azprd = azpr * deg
      return 
      end
