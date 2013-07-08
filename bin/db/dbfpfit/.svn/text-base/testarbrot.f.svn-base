c test subroutine arbrot

c coordinate system (lat, -lon, z)

	dimension wvect(3), transt(3,3)
	parameter (pi = 3.14159265)
        parameter (rad = pi/180.)


  	az = 90.
	dip = 0.
	rot = 45.

20	print *, 'Specify a vector direction and the'
	print *, 'rotation about this vector (right-hand).'
	az = raskk('azimuth of vector in degrees', az)
	dip = raskk('dip of vector in degrees', dip)
	rot = raskk('amount of rotation in degrees', rot)


	wvecthoriz = cos(dip*rad)
	wvect(1) = wvecthoriz*cos(az*rad)
	wvect(2) = wvecthoriz*sin(az*rad)
	wvect(3) = sin(dip*rad)

	call arbrot(wvect, rot, transt)

	print *, 'The x (lat) axis transforms to ',
     *    transt(1,1), transt(1,2), transt(1,3)

	print *, 'The y (-lon) axis transforms to ',
     *    transt(2,1), transt(2,2), transt(2,3)

	print *, 'The z (z) axis transforms to ',
     *    transt(3,1), transt(3,2), transt(3,3)

	if(rot .ne. 0.) goto 20
	stop
	end
