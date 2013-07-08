c program to extract events from ray file by event number
c written by kent fogleman, sep 19, 1992
c modified 12/13/92  jcl

	parameter (recsiz = 132)
	character*132 record, targ*4
	character*132 dnstrng
	character*80 fname,aask
c --- open input/output files

	print *, 'Welcome to extray!  This program reads a'
	print *, 'ray file that is sequential because'
 	print *, 'reevnum has been run.  A new ray file is'
	print *, 'created with only those events indicated'
	print *, 'by a list of event numbers.'
	print *, ' ' 
10	fname = aask('Input ray filename','fit5.ray',-80)
	call openit('read', fname, 4, ierr)
	if(ierr .ne. 0) goto 10

	print *, 'Bestfit1 adds the event number to the selected'
	print *, 'events, so these selected sumfm records are'
	print *, 'used to specify which events to select.'
21      fname = aask('Input sumfm filename', ' ', -80)
	call openit('read', fname, 7, ierr)
	if(ierr .ne. 0) goto 21

22	fname = aask('Output ray filename', ' ', -80)
	call openit('write', fname, 8, ierr)
	if(ierr .ne. 0) goto 22


c --- read the first target number
	ntarg = 0
  	read(7, '(t132, a4)', end=95) targ
	ntarg = ntarg + 1

c --- skip to an 'event number' record 

30	read(4,'(a)',end=90) record
	if(dnstrng(record(2:15)) .ne. 'c*event number') goto 30
31	if(record(18:21) .eq. targ) then
 	  write(8,'(a)') record(1:lentru(record))
32	  read(4, '(a)', end=90) record
	  if(dnstrng(record(2:15)) .ne. 'c*event number') then
 	    write(8,'(a)') record(1:lentru(record))
	    goto 32
	  else
  	    read(7, '(t132, a4)', end=95) targ
	    ntarg = ntarg + 1
	    goto 31
	  endif
	else
	  goto 30
	endif
90	read(7, '(t132, a4)', end=95) targ
	print *, 'ERROR, could not find event number ', targ
	stop
95	print *, 'Found ', ntarg, ' target events.'    
	stop
	end
