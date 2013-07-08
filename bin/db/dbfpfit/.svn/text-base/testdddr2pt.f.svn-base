c testdddr2pt
	dd1 = 90.
	da1 = 45.
	sa1 = 90.
20	dd1 = raskk("dip direction", dd1)
	da1 = raskk("dip", da1)
	sa1 = raskk("rake", sa1)
	call dddr2pt (dd1, da1, sa1, dd2, da2, sa2, 
     *    paz, pdip, taz, tdip)
	print *, 'Auxiliary dip-direction, dip, rake = ', dd2, da2, sa2
	print *, 'P axis azimuth and dip = ', paz, pdip
	print *, 'T axis azimuth and dip = ', taz, tdip
	goto 20
99	stop
	end

