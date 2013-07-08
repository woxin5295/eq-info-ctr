c test rotmech subroutine
        real              da1       ! dip angle in degrees
        real              dd1       ! dip directions in degrees
        real              sa1       ! rake angle in degrees
        real              rotaz     ! azimuth of rotation vector
        real              rotdip    ! dip of rotation vector
        real              rotdeg    ! amount of rotation
c                                       (following right-hand rule)
        real              danew     ! dip angle of rotated mechanism
        real              ddnew     ! dip direction of rotated mechanism
        real              sanew     ! slip angle of rotated mechanism
	
	dd1 = 105.
	da1 = 42.
	sa1 = -90.
	rotaz = 15.4
	rotdip = 0.
	rotdeg = 3.2
    
20	dd1 = raskk('dip direction of nodal plane', dd1)
	da1 = raskk('dip of nodal plane', da1)
	sa1 = raskk('rake of motion', sa1)

	rotaz = raskk('azimuth of rotation vector', rotaz)
	rotdip = raskk('dip of rotation vector', rotdip)
	rotdeg = raskk('rotation about rotation vector', rotdeg)

	call  rotmech (dd1, da1, sa1, rotaz, rotdip, rotdeg,
     *    ddnew, danew, sanew)

	print *, 'rotated mechanism dip-direction, dip, and rake'
	print *, ddnew, danew, sanew

	if(da1 .lt. 90.) goto 20
	stop
	end
