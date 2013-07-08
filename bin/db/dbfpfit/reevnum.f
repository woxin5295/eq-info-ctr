c program to add new event numbers to ray file
c written by kent fogleman, sep 19, 1992
c modified 12/13/92  jcl

	parameter (recsiz = 132)
	character*132 record, sumrec
	character*132 dnstrng
	character*80 fname,aask, fitn*19, parea*4, tarea*4
	logical torf, select, skip, done
c --- open input/output files

	print *, 'Welcome to reevnum!  This program reads a'
	print *, 'ray file that may not have sequential event'
 	print *, 'numbers.  A new ray file is created with'
	print *, 'sequential numbers starting with 1, and'
	print *, 'a new summary file is generated for this'
	print *, 'ray file.  The new summary file includes'
	print *, 'nobs, fit, parea, tarea, dip-direction,'
	print *, 'dip, and rake at the end.'
	print *, ' ' 
10	fname = aask('Input filename',' ',-80)
	call openit('read', fname, 4, ierr)
	if(ierr .ne. 0) goto 10

20	fname = aask('Output ray filename', ' ', -80)
	call openit('write', fname, 8, ierr)
	if(ierr .ne. 0) goto 20

21      fname = aask('Output summary filename', ' ', -80)
	call openit('write', fname, 7, ierr)
	if(ierr .ne. 0) goto 21

	call openit('write', 'delete.me', 9, ierr)

	print *, 'Events with large P and T areas may be excluded.'
	select = torf('Do you wish to exclude events on this basis', 
     *    .false.)

	if(select) then
	  maxp = iaskk(
     *    'Maximum P-area percentage for alternate solutions', 15)
	  maxt = iaskk(
     *    'Maximum T-area percentage for alternate solutions', 15)
	else
	  maxp = 100
	  maxt = 100
	endif

c --- transfer one event to temp file 

        numb = 0

28  	skip = .false.
	nrec = 0
	rewind(9)

30	continue
  	read(4,'(a)',end=40) record
	nrec = nrec + 1
	write(9, '(a)') record
        if(dnstrng(record(5:11)) .eq. 'p and t') then
          read(record(38:41), '(f4.3)') rparea
          read(record(48:51), '(f4.3)') rtarea
	  iparea = 100.*rparea + .5
	  itarea = 100.*rtarea + .5
	  if((iparea .gt. maxp) .or. (itarea .gt. maxt))
     *      skip = .true.
c	  print *, 'rparea, rtarea, iparea, itarea, maxp, maxt'
c	  print *, rparea, rtarea, iparea, itarea, maxp, maxt
c	  print *, 'skip = ', skip
 	endif
	if(record .ne. ' ') goto 30
	goto 42

c --- end of file found
40	done = .true.
	if((nrec .eq. 0) .or. (skip)) goto 90

c --- end of one event
42	rewind(9)
	if(skip) then
	  skip = .false.
	  goto 30
	endif

c --- read in this selected event again
50  	read(9,'(a)',end=60) record

c --- write out all records which aren't event number records

	if (dnstrng(record(4:15)) .eq. 'event number') then
	  numb = numb + 1
	  write(record(17:21), '(i5)') numb
	endif

	if(dnstrng(record(1:3)) .eq. ' s*') then
	  sumrec = record(4:83)
	endif

	if(dnstrng(record(1:3)) .eq. ' f*')
     *    fitn = record(5:15)//record(17:24)

	if(dnstrng(record(5:11)) .eq. 'p and t') then
          parea = record(38:41)
	  tarea = record(48:51)
	  if((fitn .eq. ' ') .or. (sumrec .eq. ' ')) then
	    print *, 'structural error in original ray file'
	    stop
	  endif
          write(7, '(7a)') 
     *      sumrec(1:80), '/', fitn, ' ', parea, ' ', tarea
	  sumrec = ' '
	  fitn = ' '
	endif

 	write(8,'(a)') record(1:lentru(record))
        go to 50

c --- end of one selected event
60	if(done) then
	  goto 90
	else
	  goto 28
	endif
		
c --- end of processing

90 	print *, numb, ' events processed.'
	end
