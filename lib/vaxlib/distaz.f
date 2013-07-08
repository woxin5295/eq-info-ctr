c*reference:
c$akeys:
c$ikeys: 10 8 82
c$rkeys:
c$title: compute distances in deg. and azimuths using bruce julian's method
c$author: john c. lahr
c$institution: usgs, 345 middlefield road, menlo park, ca 94025
c*abstract: this is an interactive program written for the pdp vax
c           computer to test the distance calculations used in 
c           hypoellipse.  distances may be computed between any two
c           points, and the program can also compute a new location
c           derived by shifting a previously given location a
c           specified distance.  unlike richter's formulation, 
c           distances may be global and latitudes may exceed
c           70n or 70s.  the sign convention is northward and westward
c           positive.  azimuths are measured clockwise from north.
c
c           subroutines and functions required:
c                 refpt, delaz, back, fold, and unfold:  calculate dist & az
c                 and converts
c                 from input format (ie, 60 n 14.21) to internal storage
c                 mode (radians) and visa versa.
c                       cvrtop to convert from rectangular to
c                       polar coordinates.
c                          cvrtop uses function hypot to calculate 
c                             sqrt(x**2 + y**2)
c                 most subroutines were written by b. julian
c              aask, iaskk and raskk are interactive functions that 
c                 prompt for terminal input.
c
c*format:  input:   interactive input form unit 5
c          output:  interactive output to unit 5
c
c*end
c-------- pub1:[lahr.hypotest]testdaz1.for
c
c modified 10-24-97 to handle real-valued lat and lon degrees - cds

      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)

c-------- get the reference point coordinates
c
      character*1 aask, ins, iew, eqins, eqiew, stins, stiew
c
      write(6,30)
   30 format(' welcome to geosub, a program to test subroutines',
     1      /'  using bruce julian"s distance equations.'
     2      /' distance is given in degrees.')
c
c-------- get the earthquake location
c
   50 continue
      write(6,'(1h )')
      eqlatd = raskk(' epicenter latitude,  degrees:  ', eqlatd)
      eqlatm = raskk('                      minutes:  ', eqlatm)
      eqins =   aask('           (n)orth or (s)outh:  ', 'n', -1)
      eqlond = raskk('           longitude, degrees:  ', eqlond)
      eqlonm = raskk('                      minutes:  ', eqlonm)
      eqiew =   aask('             (e)ast or (w)est:  ', 'w', -1)
c
c
c-------- convert earthquake location geocentric radians
c
      la = int(eqlatd)
      ala = eqlatm + (eqlatd - float(la))*60.
      lo = int(eqlond)
      alo = eqlonm + (eqlond - float(lo))*60.
      call fold2(alat,alon,la,eqins,ala,lo,eqiew,alo)
      radius = (cos(alat)**2/equrad**2 + sin(alat)**2/polrad**2)**(-.5)
c     akmpd = .0174533 * radius
      akmpd = rad * radius
      print *, 'Geocentric latitude = ', deg*alat
      print *, 'at this latitude there are ', akmpd, ' km/deg'
c
c-------- choose between calculating distance and azimuth to a station and
c            shifting the epicenter
c
   75 write(6,100)
  100 format(/' to calculate a distance and azimuth answer 1 or {cr}',/,
     1       ' to shift epicenter by dist (deg), az  answer 2',/,
     1       ' to shift epicenter by dist (m),   az  answer 3',/,
     2       ' to shift epicenter by dlat, dlon      answer 4',/,
     2       ' to define a new epicenter             answer 5',/,
     3       ' to quit                               answer 6')
      ians = iaskk('answer', ians)
      if (ians .lt. 0 .or. ians .gt. 6) then
	write(6,'(''  Invalid response'')')
	go to 75
      endif
      if(ians .eq. 2) go to 500
      if(ians .eq. 3) go to 520
      if(ians .eq. 4) go to 620
      if(ians .eq. 5) go to 50
      if(ians .eq. 6) go to 700
      write(6,'(1h )')
      stlatd = raskk(' station latitude,  degrees:  ', stlatd)
      stlatm = raskk('                    minutes:  ', stlatm)
      stins =   aask('         (n)orth or (s)outh:  ', 'n', -1)
      stlond = raskk('         longitude, degrees:  ', stlond)
      stlonm = raskk('                    minutes:  ', stlonm)
      stiew =   aask('           (e)ast or (w)est:  ', 'w', -1)
c
c-------- convert station location into geocentric coordinates (radians)
c            and calculate distance and azimuth  
c
      la = int(stlatd)
      ala = stlatm + (stlatd - float(la))*60.
      lo = int(stlond)
      alo = stlonm + (stlond - float(lo))*60.
      call fold2(slat, slon, la, stins, ala, lo, stiew, alo)
      call delaz(alat, alon, dekm, dedeg, az0, slat, slon)
d     print *,   alat, alon, dekm, dedeg, az0, slat, slon
d     write(6, 350) dedeg, dekm, az0
  350 format(' calculated distance (deg and km) ', 2e20.7, /,
     *       ' and azimuth from epicenter to station (deg) ', f9.5)
      call distaz(alat, alon, slat, slon, dedeg, az0, az1)
      write(6, 352) dedeg, dedeg*rad, dekm, az0, az1
  352 format(/,' calculated distance:', f12.5,' deg'/,
     *       '                     ', f12.5, ' rad'/,
     *       '                     ', f12.5, ' km'/,
     *       ' azimuth from epicenter to station (deg):', f10.5, /,
     *       '                      back azimuth (deg):', f10.5)
      go to 75
c
c-------- shift epicenter by distance (deg) and azimuth
c
  500 delat = raskk(' give delta (deg) change in eq: ', delin)
      azin = raskk(' give azimuth (deg clockwise from n) of change:',
     *              azin)
c      azin = - azin
      call back1 (delat, azin, newlat, newlon, alat, alon)
      call unfold2(newlat, newlon, la, ins, ala, lo, iew, alo)
      write(6, 510) la,ins,ala,lo,iew,alo
  510 format(' new lat and lon are:  ',i2,1x,a1,f8.3,1x,i3,1x,a1,f8.3)

      eqlatd = la
      eqlatm = ala
      eqins = ins
      eqlond = lo
      eqlonm = alo
      eqiew = iew

      go to 75
c
c-------- shift epicenter by distance (m) and azimuth
c
  520 delm = raskk(' give delta (meters) change in eq: ', delin)
      deldeg = delm / (1000. * akmpd)
      azin = raskk(' give azimuth (deg clockwise from n) of change:',
     *              azin)
c      azin = - azin
      call back1 (deldeg, azin, newlat, newlon, alat, alon)
      call unfold2(newlat, newlon, la, ins, ala, lo, iew, alo)
      write(6, 510) la,ins,ala,lo,iew,alo

c compute again (delat and delon are km shift in lat and lon)
      delat = delm * cos(azin*rad) / 1000.
      delon = -delm * sin(azin*rad) / 1000.
      call back (delat, delon, newlat, newlon, alat, alon)
      call unfold2(newlat, newlon, la, ins, ala, lo, iew, alo)
      write(6, 510) la,ins,ala,lo,iew,alo

      eqlatd = la
      eqlatm = ala
      eqins = ins
      eqlond = lo
      eqlonm = alo
      eqiew = iew

      go to 75
c
c--------- shift epicenter by del-lat and del-lon
  620 delat = raskk('shift in latitude (north positive)', delat)
      delon = raskk('shift in longitude (west positive)', delon)
      call back (delat, delon, newlat, newlon, alat, alon)
      call unfold2(newlat, newlon, la, ins, ala, lo, iew, alo)
      write(6, 510) la,ins,ala,lo,iew,alo

      eqlatd = la
      eqlatm = ala
      eqins = ins
      eqlond = lo
      eqlonm = alo
      eqiew = iew

      go to 75
  700 stop
      end
