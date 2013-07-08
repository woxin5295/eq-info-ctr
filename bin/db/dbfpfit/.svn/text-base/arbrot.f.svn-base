	subroutine arbrot(wvect, phirot, transt)
c  arbitrary rotation
c  Given a rotation of coordinates specified by a rotation of phirot
c  degrees about the vector wvect, find the transformation tensor, transt.
c  The sense of phirot follows the right-hand rule.

	dimension wvect(3), transt(3,3)

	parameter (pi = 3.14159265)
	parameter (rad = pi/180.)
	parameter (deg = 1./rad)

	phirad = phirot*rad

c  transt(1,i) specifies the rotated x-axis
	transt(1,1) = cos(phirad) + (1.-cos(phirad))*wvect(1)**2
	transt(1,2) = wvect(1)*wvect(2)*(1.-cos(phirad))
     *    + wvect(3)*sin(phirad)
	transt(1,3) = wvect(1)*wvect(3)*(1.-cos(phirad))
     *    - wvect(2)*sin(phirad)
	alen = transt(1,1)**2 + transt(1,2)**2 + transt(1,3)**2
	alen = sqrt(alen)
c       print *, 'x-axis rotates to ', transt(1,1), transt(1,2),
c    *    transt(1,3)
c	print *, 'length of rotated axis is ', alen

c  transt(2,i) specifies the rotated y-axis
	transt(2,1) = wvect(1)*wvect(2)*(1.-cos(phirad))
     *    - wvect(3)*sin(phirad)
	transt(2,2) = cos(phirad) + (1.-cos(phirad))*wvect(2)**2
	transt(2,3) = wvect(2)*wvect(3)*(1.-cos(phirad))
     *    + wvect(1)*sin(phirad)

c  transt(3,i) specifies the rotated z-axis
	transt(3,1) = wvect(1)*wvect(3)*(1.-cos(phirad))
     *    + wvect(2)*sin(phirad)
	transt(3,2) = wvect(2)*wvect(3)*(1.-cos(phirad))
     *    - wvect(1)*sin(phirad)
c	print *, 'wvect = ', wvect
c	print *, 'phirad = ', phirad*deg
c	print *, 'transt(3,2) = ', transt(3,2)
	transt(3,3) = cos(phirad) + (1.-cos(phirad))*wvect(3)**2

	return
	end
	
