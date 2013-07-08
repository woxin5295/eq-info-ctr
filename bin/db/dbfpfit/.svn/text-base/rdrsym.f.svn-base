      subroutine rdrsym(xr, yr, ikey)
c read regulal symbols into arrays
      integer ierr         ! returned from openit - not equal 0 if openit failed
      integer ikey(99)     ! ikey(yz) is the root symbol number that
      real    xr(30, 20)   ! xr(i, j) is the j'th x value for symbol # i
      real    yr(30, 20)   ! yr(i, j) is the j'th y value for symbol # i
c
c	write(95,*) '---> called rdrsym with:'
c	write(95,*) 'xr, yr, ikey ='
c	write(95,*) xr, yr, ikey
c
      call openit('read', 
     + '/usr/local/aeic/5.2-64/data/fpfit_lahr/regsym',
     +  20, ierr)
      if(ierr .ne. 0) then
        write(6, *) 'couly not open symbol definition file, so stop'       
        stop
      endif
      do 20 i = 1, 30
        read(20, *) iv
        if(iv .ne. i) then
          write(6, *) 'logic not correct ', iv, ' not equal to ', i
          stop
        endif
        do 18 j = 1, 20
          read(20, *) xr(i, j), yr(i, j)
          if(xr(i, j) .eq. 9999.) go to 20
18      continue
20    continue
c read in ikey pointers
30    read(20, *, end=40) i, j
      ikey(i) = j
      go to 30
40    close (20)
c
c     write(6, *) '---> returned from rdrsym'
      return
      end
