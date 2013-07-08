      subroutine getpol(taz, tdp, paz, pdp, az2, dp2, az3, dp3, rad, 
     * dpr)
c
c-----   given the p & t axes, find the poles to the fault planes
c
c              az1        pole of first plane
      real     az1
c              dp1        pole of first plane
      real     dp1
c              az2        pole of second plane
      real     az2
c              dp2        pole of second plane
      real     dp2
c              paz        p axis azimuth
      real     paz
c              pdp        p axis dip
      real     pdp
c              rad        radians per degree
      real     rad
c              taz        t axis azimuth
      real     taz
c              tdp        t axis dip
      real     tdp
c              shift      distance to shift axes
      real     shift      
c
c compute azimuth from paz, pdp to taz, tdp
c       call distaz(p2lat,      p2lon, lat,        lon,dedeg,  az0,  
c    *  paz)
        call distaz(-pdp*rad, paz*rad, -tdp*rad, taz*rad, dum1, az0, 
     *  bkaz)
      if (abs(dum1-90.) .gt. .1) then
c       print *, 'distance between poles to fault planes, ', dum1,
c    *  ' but should be 90.  subroutine getpol.'
      endif
c
c move 45 degrees from (paz, pdp) in the direction az0, to reach 
c plat, plon
c
      shift = 45.
c     call back1 (shift, azimuth, newlat, newlon,   oldlat,  oldlon)
      call back1 (shift,     az0,   plat,   plon, -pdp*rad, paz*rad)
      az2 = plon*dpr
      dp2 = -plat*dpr
c      if (dp2 .lt. 0.) then
c        dp2 = -dp2
c        az2 = az2 + 180.
c        if (az2 .gt. 360.) az2 = az2 - 360.
c      endif
c
c move 45 degrees from (paz, pdp) in the direction az0+180, to reach 
c tlat, tlon
c
c     call back1 (shift, azimuth, newlat, newlon,   oldlat,  oldlon)
      call back1 (shift, az0+180.,   tlat,   tlon, -pdp*rad, paz*rad)
      az3 = tlon*dpr
      dp3 = -tlat*dpr
c      if (dp3 .lt. 0.) then
c        dp3 = -dp3
c        az3 = az3 + 180.
c        if (az3 .gt. 360.) az3 = az3 - 360.
c      endif
      return
      end
