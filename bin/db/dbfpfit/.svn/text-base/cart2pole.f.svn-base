	subroutine cart2pole(wvect, rotaz, rotdip)
c convert from polar to cartesian coordinates

	real	rotaz	 ! azimuth clockwise in degrees with respect to north
	real	rotdip   ! dip in degrees downward from the horizontal
	real	wvect(3) ! north, east, down vector

        parameter (pi = 3.14159265)
        parameter (rad = pi/180.)
        parameter (deg = 1./rad)
c
	rotaz = deg*atan2(wvect(2), wvect(1))
	x = sqrt(wvect(1)**2 + wvect(2)**2)
	rotdip = deg*atan(wvect(3)/x)
	if (rotdip .lt. 0.) then
	  rotdip = -rotdip
	  rotaz = rotaz + 180.
	  if (rotaz .ge. 360.) rotaz = rotaz - 360.
	endif
	return
	end
