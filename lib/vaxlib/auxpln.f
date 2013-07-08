!10    dd1 = raskk('dip dir', dd1)
!      da1 = raskk('dip', da1)
!      sa1 = raskk('rake', sa1)
!      call auxpln (dd1, da1, sa1, dd2, da2, sa2)
!      print *, dd1, da1, sa1, dd2, da2, sa2
!      goto 10
!      end
      subroutine auxpln (dd1, da1, sa1, dd2, da2, sa2)
c
c    calculate the auxilliary plane of a double couple fault plane solution, 
c    given the principle plane.
c
c    written by paul reasenberg, june, 1984, from class notes by dave boore, (both at the u.s.g.s., menlo park.) 
c    angle variables phi, del and lam are as defined in aki and richards, (1980), p.114.
c
      real              da1                             ! dip angle in degrees
      real              dd1                             ! dip directions in degrees
      real              sa1                             ! slip angle in degrees
      real              da2                             ! dip angle of auxilliary plane
      real              dd2                             ! dip direction of auxilliary plane
      real              sa2                             ! slip angle of auxillary plane
c 
      
      double precision  bot                             ! scratch variable 
      double precision  del1                            ! dip angle of principal plane in radians
      logical           first                           ! test: true if first time into routine
      double precision  phi1                            ! fault plane strike of principal plane
      double precision  phi2                            ! strike of auxilliary plane in radians
      double precision  rad                             ! conversion factor from degrees to radian
      double precision  sgn                             ! saves principal plane slip angle for assigning proper sign to auxilliary
      double precision  top                             ! scratch variable 
      double precision  xlam1                           ! slip angle of principal plane in radians
      double precision  xlam2                           ! slip angle of auxilliary plane
c
      data first /.true./
      save first, rad
c
      if (first) then
        first = .false.
        rad = datan(1.0d0)/45.0d0
      end if
c
c keep dip direction in range 0 to 360
      if(dd1 .lt. 0.) dd1 = dd1 + 360.
      if(dd1 .ge. 360.) dd1 = dd1 - 360.
c
c keep rake between +/- 180.
      if(sa1 .gt. 180.) sa1 = sa1 - 360.
      if(sa1 .lt. -180.) sa1 = 360. + sa1
c
c convert from dip direction to strike in range 0 to 360
      phi1 = dd1 - 90.0d0
      if (phi1 .lt. 0.0d0) phi1 = phi1 + 360.0d0
      phi1 = phi1*rad
      del1 = da1*rad
      sgn = sa1
      xlam1 = sa1*rad
c
      top = dcos(xlam1)*dsin(phi1) - dcos(del1)*dsin(xlam1)*dcos(phi1)
      bot = dcos(xlam1)*dcos(phi1) + dcos(del1)*dsin(xlam1)*dsin(phi1)
      dd2 = datan2(top, bot)/rad
      phi2 = (dd2 - 90.0d0)*rad
      if (sa1 .lt. 0.0d0) dd2 = dd2 - 180.0d0
      if (dd2 .lt. 0.0d0) dd2 = dd2 + 360.0d0
      if (dd2. gt. 360.0d0) dd2 = dd2 - 360.0d0
c
      da2 = dacos(dsin(dabs(xlam1))*dsin(del1))/rad
      xlam2 = -dcos(phi2)*dsin(del1)*dsin(phi1) +
     & dsin(phi2)*dsin(del1)*dcos(phi1)
c
c machine accuracy problem
c
      if (dabs(xlam2) .gt. 1.0d0) then
        xlam2 = dsign(1.0d0, xlam2)
      end if
      xlam2 = dsign(dacos(xlam2), sgn)
      sa2 = xlam2/rad
c
c
c check for dip greater than 90 degrees
c
!d     print *, 'auxpln check'
      if (da2 .gt. 90.) then
!d       print *, dd2, da2, sa2, 'original'
        da2 = 180. - da2 
        dd2 = dd2 - 180.
        sa2 = -sa2
!d       print *, dd2, da2, sa2, ' modified dd2 and da2'
      endif
      if (da2 .lt. 0.) then
        da2 = -da2
        dd2 = dd2 - 180.
        sa2 = sa2 + 180.
        if(sa2 .gt. 180.) sa2 = sa2 - 360.
!d       print *, dd2, da2, sa2, ' modified dd2 and da2'
      endif
c
c
c now check for negative dip direction
c
60    if (dd2 .lt. 0.) then
        dd2 = dd2 + 360.
!d       print *, dd2, da2, sa2, ' modified dd2'
        goto 60
      endif
      return
      end
c
c
