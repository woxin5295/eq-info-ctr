      subroutine getmech(taz, tdp, paz, pdp, dd1, da1, rake1)
c
c-----   given the p & t axes, find the dip direction, dip, and rake
c        of one of the fault planes
c
      	real     dd1       ! dip-direction of first nodal plane, degrees
      	real     da1       ! dip of first nodal plane, degrees
	real     rake1     ! rake of first nodal plane, degrees

c                paz        p axis azimuth
      	real     paz
c                pdp        p axis dip
      	real     pdp
c                rad        radians per degree
      	real     rad
c                taz        t axis azimuth
      	real     taz
c                tdp        t axis dip
      	real     tdp
c                shift      distance to shift axes
      	real     shift      
c		 plat1, plon1  pole of 1st plane
	real     plat1, plon1
c		 plat2, plon2  pole of 2nd plane
	real     plat2, plon2


        parameter (pi = 3.14159265)
        parameter (rad = pi/180.)
	parameter (dpr = 1./rad)

	if((pdp .lt. 0.0001) .and. (tdp .lt. 0.0001)) then
c	  print *, 'Pure strike slip faulting'
	  dd1 = paz + 45.
	  if(paz .ge. 360.) paz = paz - 360.
	  da1 = 90.
	  rake1 = 180.
	  return
	endif

	if(abs(pdp - tdp) .lt. 0001) then
c	  print *, 'pdp = tdp'
c 	  print *, 'Oblique slip on vertical plane or strike slip'
c	  print *, ' on non-vertical plane'
	  dd1 = 0.5*(paz + taz) - 90.
	  if(dd1 .lt. 0.) dd1 = dd1 + 360.
	  da1 = 90.
	  rake1 = dpr*asin(sin(pdp*rad)*sqrt(2.))
	  return
	endif
c
c compute azimuth from paz, pdp to taz, tdp
c distaz convention is north and west positive
c       call distaz(p2lat,      p2lon, lat,        lon, dedeg,  az0,  
c    *  paz)
        call distaz(-pdp*rad, paz*rad, -tdp*rad, taz*rad, dum1, az0, 
     *  bkaz)
      if (abs(dum1-90.) .gt. .1) then
c       print *, 'distance between poles to fault planes, ', dum1,
c    *  ' but should be 90.  subroutine getpol.'
      endif
c
c	print *, 'move 45 degrees from (paz, pdp) ', paz, pdp
c	print *, 'represented by lat, lon = ', -pdp, paz
c	print *, 'in the direction az0', az0
c
c     	call back1 (shift, azimuth, newlat, newlon,   oldlat,  oldlon)
      	call back1 (45.,     az0,   plat1,   plon1, -pdp*rad, paz*rad)
      	plat1 = plat1*dpr
      	plon1 = plon1*dpr
c	print *, 'to reach plat1, plon1 ', plat1, plon1
c	print *, 'This is the pole of first nodal plane'

c make sure vector is pointing upward
      	if (plat1 .lt. 0.) then
          plat1 = -plat1
          plon1 = plon1 + 180.
          if (plon1 .ge. 360.) plon1 = plon1 - 360.
      	endif
c
c move 45 degrees from (paz, pdp) in the direction az0 + 180, to reach
c plat2, plon2
c
c       call back1 (shift, azimuth, newlat, newlon,   oldlat,  oldlon)
        call back1 (45., az0+180., plat2,   plon2, -pdp*rad, paz*rad)
        plat2 = plat2*dpr
        plon2 = plon2*dpr
 
c make sure vector is point upward
        if (plat2 .lt. 0.) then
          plat2 = -plat2
          plon2 = plon2 + 180.
          if (plon2 .ge. 360.) plon2 = plon2 - 360.
        endif
c	print *, 'The second nodal plane"s pole is ', plat2, plon2

c compute dip-direction, dip, and rake of plane represented by 1st pole
	dd1 = plon1
	da1 = 90. - plat1
c	print *, 'The dip direction and dip of the first plane is ',
c    *    dd1, da1

c find rake, distance from the strike of this plane (dip-direction - 90.)
c on the equator to the pole of the other plane
c       call distaz(p2lat,      p2lon, lat,        lon, dedeg,  az0,
c    *  paz)
        call distaz(plat2*rad, plon2*rad, 
     *    0., (plon1 - 90.)*rad, rake1, az0, bkaz)
c	print *, 'distaz from 0, ', plon1-90, ' to ', plat2, plon2
c	print *, 'equals ', rake1
	if(pdp .gt. tdp) then
c normal faulting, negative rake
	  if(rake1 .gt. 0.) then
	    rake1 = rake1 - 180. 
	  endif
	else
c thrust faulting, positive rake
	  if(rake1 .lt. 0.) then
	    rake1 = 180. + rake1
	  endif
	endif

      return
      end
