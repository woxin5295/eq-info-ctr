c drawplanes draws a stereo net circle and adds planes specified.
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

	print *, 'draws a stereo net circle and'
	print *, 'adds planes specified.'

20	fname = aask('name for stereo circle file', 
     *    'drawcirc.cmp', -80)
	call openit('write', fname, 1, ierr)
	if(ierr .ne. 0) goto 20

	call strnetcmp(cx, cy, rad, rmax) 

30	add = torf('add a plane', .false.)
	if(add) then
35	  fname = aask('name for plane', 'drawpl1.cmp', -80)
	  call openit('write', fname, 1, ierr)
	  if(ierr .ne. 0) goto 35

	  dd = raskk('dip direction', dd)
	  strike = dd - 90.
	  if(strike .lt. 0.0) strike = strike + 360.

	  plunge = raskk('plunge', plunge)

	  call plotplcmp(cx, cy, plunge, pi, rad, rmax, strike, prot)

	  goto 30
	endif

	stop
	end
