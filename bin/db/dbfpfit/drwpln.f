c drwpln reads dip direction and plunge from standard input and
c generates azimuth and dip values for gmt
      	parameter (pi = 3.14159265)
      	parameter (twopi = 2.0*pi)
      	parameter (halfpi = 0.5*pi)
      	parameter (rad = pi/180.)
      	parameter (deg = 1.0/rad)
	logical torf, add

	character*80 fname, aask

	cx = 0.0
	cy = 0.0
	rmax = 1.0
	prot = 0.0

	dd = 90.
	plunge = 45.


	read(5, *) dd, plunge
c 180 added so lower hemisphere will be plotted
	strike = dd - 90. + 180.
	if(strike .gt. 360.0) strike = strike - 360.
	call comppln(plunge, pi, rad, strike, prot)

	stop
	end
