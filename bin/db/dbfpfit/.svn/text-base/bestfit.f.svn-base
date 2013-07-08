	character*50 fname, aask, record*126
	real misfit, mismax
	integer par, tar

	print *, 'Welcome to bestfit!  This program reads'
	print *, 'a focal mechanism table grnerated by fppage'
	print *, 'and selects the better solutions.'

	fname = 'fppage.out'
20	fname = aask('name of input table', fname, -50)
	call openit('read', fname, 1, ierr)
	if (ierr .ne. 0) goto 20

	fname = 'fppage.best'
30	fname = aask('name of output table', fname, -50)
	call openit('write', fname, 2, ierr)
	if (ierr .ne. 0) goto 30

	ipmax = iaskk('maximum acceptable p area (% of total)',
     *    19)
	itmax = iaskk('maximum acceptable t area (% of total)',
     *    19)
	mismax = raskk('maximum acceptable misfit', 0.09)
	minnum = iaskk('minimum acceptable number of first motions',
     *    16)

	nfound = 0
40      read(1, '(a)', end = 60) record
	read(record, 42, err=40) misfit, par, tar, nobs
42	format(t105, f5.3, i5, i4, i8)

	if(misfit .gt. mismax) goto 40
	if(par .gt. ipmax) goto 40
	if(tar .gt. itmax) goto 40
	if(nobs .lt. minnum) goto 40

	write(2, '(a)') record
	nfound = nfound + 1

	goto 40

60	print *, 'selected ', nfound, ' events'
	close(1)
	close(2)
        stop
	end

