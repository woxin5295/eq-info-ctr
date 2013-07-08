c filter to convert x, y, z, dip_direction, dip, rake into
c x, y, z, paz, plen, taz, tlen, pzaz, pzlen, tzaz, tzlen

c rotate a focal mechanism

        real	dd1		! dip directions in degrees
        real	da1		! dip angle in degrees
        real	sa1		! slip angle in degrees
	real	paz, pdip 	! map view P azimuth and dip
	real	taz, tdip 	! map view T azimuth and dip
	real	pzaz, pzdip 	! north-looking view P azimuth and dip
	real	tzaz, tzdip 	! north-looking view T azimuth and dip
	real	x		! x location of event in x,y plane	
	real	y		! y location of event in x,y plane	
	real	plen		! map view length - cos(dip of p)
	real	tlen		! map view length - cos(dip of t)
	real	pzlen		! north-looking length - cos(dip of p)
	real	tzlen		! north-looking length - cos(dip of t)
c
        parameter (pi = 3.14159265)
        parameter (rad = pi/180.)
	parameter (deg = 180./pi)

20	read (5, '(t82, f3.0, f3.0, f4.0, t111, f5.1, 5x, 2f5.1)',
     *    end = 99) dd1, da1, sa1, y, x, z
c	print *, dd1, da1, sa1, y, x, z
c
c compute the P- and T-axis vectors
	call dddr2pt (dd1, da1, sa1, dum1, dum2, dum3,
     *    paz, pdip, taz, tdip)
c	print *, 'paz, pdip, taz, tdip ', paz, pdip, taz, tdip

c compute plen and tlen
	plen = cos(rad*pdip)
	tlen = cos(rad*tdip)

c compute north-looking azimuth (clockwise from vertical) and dip (into section)
	pzaz = 
     *    90. + atan2(sin(pdip*rad), cos(pdip*rad)*sin(paz*rad))*deg
	pzdip = asin(cos(pdip*rad)*cos(paz*rad))*deg
	pzlen = cos(pzdip*rad)

	tzaz = 
     *    90. + atan2(sin(tdip*rad), cos(tdip*rad)*sin(taz*rad))*deg
	tzdip = asin(cos(tdip*rad)*cos(taz*rad))*deg
	tzlen = cos(tzdip*rad)

c pass the new information along
	write (6, '(11f11.2)') x, y, z, paz, plen, taz, tlen,
     *                        pzaz, pzlen, tzaz, tzlen

	goto 20

99	stop
	end
