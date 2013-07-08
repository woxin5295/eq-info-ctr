      	subroutine gg2gc(alatgg, alatgc)
c ggtogc - convert from geographic to geocentric latitude
      	parameter (pi = 3.14159265)
      	parameter (twopi = 2.0*pi)
      	parameter (halfpi = 0.5*pi)
      	parameter (rad = pi/180.)
      	parameter (deg = 1.0/rad)
      	parameter (equrad = 6378.2064)
      	parameter (polrad = 6356.5838)
      	parameter (flat = (equrad - polrad)/equrad)
      	parameter (c1 = (1.0 - flat)**2)
      	parameter (c2 = halfpi*(1.0/c1 - 1.0))
c
        alatggr = alatgg*rad

        if (halfpi-abs(alatggr) .ge. 0.02) then
          alatgc = deg*atan(c1*tan(alatggr))
        else
          alatgc = deg*(alatggr/c1-sign(c2,alatggr))
        endif
 
	return
	end
