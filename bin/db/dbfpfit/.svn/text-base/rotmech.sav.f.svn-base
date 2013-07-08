	subroutine rotmech (dd1, da1, sa1, rotaz, rotdip, rotdeg,
     *    ddnew, danew, sanew)

c rotate a focal mechanism

        real              da1       ! dip angle in degrees
        real              dd1       ! dip directions in degrees
        real              sa1       ! slip angle in degrees
	real              rotaz     ! azimuth of rotation vector
	real              rotdip    ! dip of rotation vector
	real              rotdeg    ! amount of rotation of mechanism
c				        (following right-hand rule)
        real              danew     ! dip angle of rotated mechanism
        real              ddnew     ! dip direction of rotated mechanism
        real              sanew     ! slip angle of rotated mechanism
	real		  wvect(3)  ! vector representation of rotaz, rotdip
	real		  paz, pdip ! original P azimuth and dip
	real		  taz, tdip ! original T azimuth and dip
	real		  pvec(3)   ! P vector
	real              tvec(3)   ! T vector
	real		  paznew, pdipnew ! new P azimuth and dip
	real		  taznew, tdipnew ! new T azimuth and dip
	real		  transt(3,3) ! rotation tensor
	real		  pvecnew(3)  ! rotated P vector
	real		  tvecnew(3)  ! rotated T vector
c
        parameter (pi = 3.14159265)
        parameter (rad = pi/180.)

c 	print *, 'rotmech input: dd1, da1, sa1, rotaz, rotdip, rotdeg'
c 	print *,  dd1, da1, sa1, rotaz, rotdip, rotdeg
c
c compute the P- and T-axis vectors
	call dddr2pt (dd1, da1, sa1, dum1, dum2, dum3,
     *    paz, pdip, taz, tdip)
c	print *, 'paz, pdip, taz, tdip ', paz, pdip, taz, tdip

c convert the P and T axes into cartesian coordinates
	call pole2cart(paz, pdip, pvec)
c	print *, 'P vector (north, east, down) ', pvec
	call pole2cart(taz, tdip, tvec)
c	print *, 'T vector (north, east, down) ', tvec

c convert rotaz, rotdip into cartesian coordinates (north, east, down)
	call pole2cart(rotaz, rotdip, wvect)
c	print *, 'rotation vector (north, east, down) ', 
c    *    (wvect(i), i=1,3)

c compute the rotation tensor, transt(3,3)
c note that to cause the mechanism to rotate by rotdeg we must
c change to a new coordinate system that is rotated by -rotedg!
c (this is expected, if you think about it for a while!)
	call arbrot(wvect, -rotdeg, transt)
c	print *, 'rotation tensor ', (transt(1,i),i=1,3)
c	print *, '                ', (transt(2,i),i=1,3)
c	print *, '                ', (transt(3,i),i=1,3)

c rotate the P and T axes
	call tenxvec(transt, pvec, pvecnew)
c	print *, 'rotated P vector ', (pvecnew(i), i=1,3)
	call tenxvec(transt, tvec, tvecnew)
c	print *, 'rotated T vector ', (tvecnew(i), i=1,3)

c convert the P and T axes from cartesian to polar coordinates
	call cart2pole(pvecnew, paznew, pdipnew)
c	print *, 'rotated P vector azimuth and dip ', paznew, pdipnew
	call cart2pole(tvecnew, taznew, tdipnew)
c	print *, 'rotated T vector azimuth and dip ', taznew, tdipnew	

c compute the focal mechanism dip-direction, dip, and rake from P and T
	call getmech(taznew, tdipnew, paznew, pdipnew, 
     *    ddnew, danew, sanew)
c	print *, 'rotated dd da sa ', ddnew, danew, sanew

	return
	end
