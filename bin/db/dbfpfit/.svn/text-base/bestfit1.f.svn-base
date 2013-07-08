	character*50 fname, aask, record*130
	real misfit, mismax
	logical skip

	print *, 'Welcome to bestfit1!  This program reads'
	print *, 'a summary file with focal mechanism '
	print *, 'parameters added by reevnum and selects'
	print *, 'the better solutions.  The event number'
	print *, 'is appended to the end.'

	fname = 'fit5.sumfm'
20	fname = aask('name of sumfm file', fname, -50)
	call openit('read', fname, 1, ierr)
	if (ierr .ne. 0) goto 20

	fname = 'fit5.sumfm.best'
30	fname = aask('name of output file', fname, -50)
	call openit('write', fname, 2, ierr)
	if (ierr .ne. 0) goto 30

	ipmax = iaskk('maximum acceptable p area (% of total)',
     *    10)
	pmax = ipmax/100.
	itmax = iaskk('maximum acceptable t area (% of total)',
     *    10)
	tmax = itmax/100.
	mismax = raskk('maximum acceptable misfit', 0.05)
	minnum = iaskk('minimum acceptable number of first motions',
     *    20)
	mincs = raskk('minimum cs (distance nnw along WBZ)', 0.)
	maxcs = raskk('maximum cs', 75.)
   	minsd = raskk('minimum sd (distance up along WBZ)', 0.)
	maxsd = raskk('maximum sd', 75.)
	minsz = raskk('minimum sz (distance down through WBZ)', 0.)
	maxsz = raskk('maximum sz', 30.)

	nfound = 0
	nevt = 0
	nin = 0
	nmis = 0
	npar = 0
	ntar = 0
	nnob = 0
40      read(1, '(a)', end = 60) record
	read(record, 42, err=99) misfit, nobs, par, tar, cs, sd, sz
42	format(t92, f5.3, i4, 2f5.3, f5.1, 5x, 2f5.1)
c	print *, 'misfit nobs par tar cs sd sz'
c	print *,  misfit,nobs,par,tar,cs,sd,sz 
	nevt = nevt + 1

c check location
	if((cs .lt. mincs) .or. (cs .ge. maxcs)) then
c	  print *, 'cs lt ', mincs, ' or ge ', maxcs
	  goto 40
	endif
	if((sd .lt. minsd) .or. (sd .ge. maxsd)) then
c	  print *, 'sd lt ', minsd, ' or ge ', maxsd
	  goto 40
	endif
	if((sz .lt. minsz) .or. (sz .ge. maxsz)) then
c	  print *, 'sz lt ', minsz, ' or ge ', maxsz
	  goto 40
	endif
	nin = nin + 1
c	write(2, '(2a, i5)') 'in area: ', record, nevt

c check focal mechanism parameters
	skip = .false.
	if(misfit .gt. mismax) then
c	  print *, 'misfit larger than ', mixmax
	  nmis = nmis + 1
	  skip = .true.
	endif
	if(par .gt. pmax) then
c	  print *, 'parea greater than ', pmax
	  npar = npar + 1
	  skip = .true.
	endif
	if(tar .gt. tmax) then
c	  print *, 'tarea greater than ', tamx
	  ntar = ntar + 1
	  skip = .true.
	endif
	if(nobs .lt. minnum) then
c	  print *, 'nobs less than ', minnum
	  nnob = nnob + 1
	  skip = .true.
	endif
	if(skip) goto 40

	write(2, '(a, i5)') record, nevt
	nfound = nfound + 1

	goto 40

60	continue
	print *, 'reviewed ', nevt, ' events'
	print *, nin, ' were within the selected volume'
	print *, 'of these, ', nfound, ' met quality limits'
	print *, ' '
	print *, ' of those that were within the volume:'
	print *, ' the fit was too large for ', nmis 
	print *, ' parea was too large for ', npar
	print *, ' tarea was too large for ', ntar
	print *, ' observations were too few for ', nnob
	close(1)
	close(2)
	stop
99	print *, 'decode error on this record, so stop'
	print *, '(t92, f5.3, i4, 3f5.3, 5x, 2f5.1)'
	print *, record
	close(1)
	close(2)
        stop
	end

