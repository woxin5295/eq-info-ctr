	subroutine dddr2pt (dd1, da1, sa1, dd2, da2, sa2, 
     *    paz, pdip, taz, tdip)

c given dip-direction, dip, and rake, computer the dip-direction,
c dip, and rake of the other plane, and the P and T vectors.
c jcl 6/18/95

c angle of incidence measured from downward vertical

      	real              da1       ! dip angle in degrees
      	real              dd1       ! dip directions in degrees
      	real              sa1       ! slip angle in degrees
      	real              da2       ! dip angle of auxilliary plane
      	real              dd2       ! dip direction of auxilliary plane
      	real              sa2       ! slip angle of auxillary plane
      	real              ain1      ! angle of incidence of p/t axis
      	real              ain2      ! angle of incidence of t/p axis
      	real              ainp      ! angle of incidence of p axis
      	real              aint      ! angle of incidence of t axis
      	real              az1       ! azimuth of p/t axis
      	real              az2       ! azimuth of t/p axis
	real              pdip      ! dip of p axis
	real              tdip      ! dip of t axis
	real              paz       ! azimuth of p axis
	real              taz       ! azimuth of t axis

	parameter (pi = 3.14159265)
	parameter (rad = pi/180.)

	call auxpln (dd1, da1, sa1, dd2, da2, sa2)
	call tandp (ain1, ain2, az1, az2, da1, da2, dd1, dd2, pi,rad)
      	if (ain1 .gt. 90.) then
          ain1 = 180. - ain1
          az1 = 180. + az1
	  if (az1 .ge. 360.) az1 = az1 - 360.
      	endif

      	if (ain2 .gt. 90.) then
          ain2 = 180. - ain2
          az2 = 180. + az2
	  if (az2 .ge. 360.) az2 = az2 - 360.
      	endif

      	if (sa1 .lt. 0.) then
	  paz = az1
	  ainp = ain1
	  taz = az2
	  aint = ain2
      	else
	  paz = az2
	  ainp = ain2
	  taz = az1
	  aint = ain1
      	end if

	pdip = 90. - ainp
	tdip = 90. - aint

	return
	end
