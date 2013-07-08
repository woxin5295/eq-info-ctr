	subroutine pole2cart(rotaz, rotdip, wvect)
c convert from polar to cartesian coordinates

	real	rotaz	 ! azimuth clockwise in degrees with respect to north
	real	rotdip   ! dip in degrees downward from the horizontal
	real	wvect(3) ! north, east, down vector

        parameter (pi = 3.14159265)
        parameter (rad = pi/180.)
c
        wvecthoriz = cos(rotdip*rad)
        wvect(1) = wvecthoriz*cos(rotaz*rad)
        wvect(2) = wvecthoriz*sin(rotaz*rad)
        wvect(3) = sin(rotdip*rad)

	return
	end
