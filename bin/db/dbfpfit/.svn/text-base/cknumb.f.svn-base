c program to check for nonsequential numbers in ray file
c 12/13/92  jcl

	parameter (recsiz = 132)
	character*132 record
	character*132 dnstrng
	character*80 fname,aask
c --- open input/output files

	print *, 'Welcome to cknumb!  This program reads a'
	print *, 'ray file that may not have sequential event'
 	print *, 'numbers and lets you know which are out'
	print *, 'of order.'
	print *, ' ' 
10	fname = aask('Input filename',' ',-80)
	call openit('read', fname, 4, ierr)
	if(ierr .ne. 0) goto 10

c --- process next record

        numbold = 0
30	read(4,'(a)',end=90) record

c --- write out all records which aren't sequential

	if (dnstrng(record(4:15)) .eq. 'event number') then
	  read(record, '(t17, i5)') numb
	  if(numb .ne. numbold + 1) then
	    print *, 'following ', numbold, ' is ', 
     *         record(1:lentru(record))
	  endif
	endif
	numbold =  numb
        go to 30
		
c --- end of processing

90 	stop
	end
