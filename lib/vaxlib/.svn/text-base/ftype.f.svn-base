	integer function ftype(fname)

c  on return, ftype = 0 --> regular file
c		      1 --> directory
c		      2 --> other
c		     -1 --> error

c  system functions called

	integer stat
c --
	character*(*) fname
	integer statb(13)
	logical exists

	ftype = -1

	inquire(file=fname,exist=exists)
	if (.not. exists) return

	ires = stat(fname,statb)
	if (ires .ne. 0) return

	ftype = 2
	if (btest(statb(3),15)) then
	  ftype = 0
	else if (btest(statb(3),14)) then
	  ftype = 1
	endif

	return
	end
