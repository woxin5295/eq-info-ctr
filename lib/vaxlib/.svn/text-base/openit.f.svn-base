c-----   rorw     'read' or 'write' or 'writef'
c-----   fname    name of file
c-----   iunit    unit number of file, < 0 to surpress open message
c-----   ierr     normally 0, but set to 1 on open error.
      subroutine openit(rorw, fname, iunit, ierr)
      dimension rcont(2)
      character fname*(*), astop, rorw*(*), ans*1, aask*1, string*1
      ierr = 0
      junit = iabs(iunit)
      close(unit=junit) 
      lenf = len(fname)
      lenn = index(fname,' ')
      if (lenn .eq. 0) lenn = lenf
      call locase(rorw)
c---- open to read
      if (rorw .ne. 'read') goto 100
      open(unit=junit, file=fname, blank='zero', status='old', err=500) 
      if (iunit .gt. 0) write(unit=6, fmt=11) fname(1:lenn), junit
   11 format(1x,8h opened ,a,19h for read on unit: ,i2)
c---- open to write with list format
      return 
  100 if (rorw .ne. 'write') goto 200
      open(unit=junit, file=fname, status='unknown', err=600) 
      if (iunit .gt. 0) write(unit=6, fmt=120) fname(1:lenn), junit
  120 format(1x,8h opened ,a,20h for write on unit: ,i2)
c---- open to write with fortran carriage control format
      return 
  200 if (rorw .ne. 'writef') goto 2000
      open(unit=junit, file=fname, status='unknown', err=600) 
      if (iunit .gt. 0) write(unit=6, fmt=120) fname(1:lenn), junit
      return 
  500 write(unit=6, fmt=510) fname(1:lenn)
  510 format(20h error opening file ,a)
      call bell
      ierr = 1
      return 
600	call perror(string)
	ie = ierrno()
	write(unit=6, fmt=510) fname(1:lenn)
	call bell
	if(ie .eq. 117) then
	  ans = aask('Do you wish to delete this file (y/n)', 'y', -1)
	  if (ans .eq. 'y') then
	    call system('rm '//fname)
	    if(ierrno() .ne. 117) then
	      call perror(string)
	      ierr = 1
	      return
	    else
	      goto 100
	    endif
	  endif
	endif 
	ierr = 1
	return
 2000 write(unit=6, fmt=2100) rorw
 2100 format(33h error using subroutine openit.  /,
     &45h first argument must be read or write, not:  ,a)
      call bell
      ierr = 1
      stop
      end
