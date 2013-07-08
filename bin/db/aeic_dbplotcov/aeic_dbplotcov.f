

      character*1 chr
      character*4 display
      character*80 str, file, sta, chan, sc, arg
      real*8 tstart, tend, ttmp, t1, t2, t0, t2old, t1old
      real*8 tdate
      real*8 str2epoch
      real*4 tmin, tmax, tmind, tmaxd
      parameter (NDMAX = 100000)
      parameter (NBMAX = 10000)
      real*4 data(NDMAX)
      real*4 bhist(NBMAX), bwhist(NBMAX), yhist(NBMAX)
c
      itran = 1
c
      print*,'iargc() = ',iargc()
      if (iargc() .lt. 4) then
	print *,
     + 'usage: aeic_dbplotcov dbname stachan ',
     + 'tstart tend [-h [ntrigs]] [-wftar] [-nodisplay]'
	stop
      end if
      call getarg (1, file)
      print*,'file = ',file
      call getarg (2, sc)
      print*,'sc = ',sc
      call getarg (3, str)
      print*,'str1 = ',str
      tstart = str2epoch(str) 
      call getarg (4, str)
      print*,'str2 = ',str
      tend = str2epoch(str) 
      display = " " 
      ihist = 0
      iwftar = 0
      ntrigs = 10
      if (iargc() .eq. 5 .or. iargc() .eq. 6 .or. iargc() .eq. 7
     +    .or. iargc() .eq. 8) then
	  i = 5 
   2      if ( i .le. iargc() ) then      
	      call getarg (i, arg)
      print*,'arg = ',arg
	      if (arg .eq. '-h') then
		ihist = 1
		if (i+1 .le. iargc()) then
		  call getarg (i+1, arg)
		  if (arg(1:1) .ne. '-') then
		    i = i + 1
		    read (arg, *, err=999) ntrigs
		  end if
		end if
	      else if (arg .eq. '-wftar') then
		iwftar = 1
	      else if (arg .eq. '-nodisplay') then
		display = "none"
	      else
		print *,'dbplotcov: Unrecognized option'
		print *,
     + 'usage: dbplotcov dbname stachan ',
     + 'tstart tend [-h [ntrigs]] [-wftar] [-nodisplay]'
		stop
	      end if
	    i = i + 1 
            go to 2 
	  end if
      end if
      tmin = 0.0
      ttmp = tend - tstart
      tmax = ttmp
      call readcov (file, sc, tstart, tend, iwftar, ierr)
      if (ierr .ne. 0) stop
      write (str, '(a,a)') 'dbplotcov:',file(1:ilen(file))
      call initt (itran, 'aeic_dbplotcov.ps', display, str,
     + 0.8, 0.0, 0.0)
      write (str, '(a)') file(1:ilen(file))
c     call setbac (240., 0.2, 1.0)
      call clear
      call setfor (0.0, 0.0, 0.0)
      call setbac (0.0, 1.0, 0.0)
      if (itran .eq. 0) then
	xdim = 6.0
	ydim = 8.5
        call setdim (7.5, 10.0, 0.0, 0.0)
      else
	xdim = 8.5
	ydim = 6.0
        call setdim (10.0, 7.5, 0.0, 0.0)
      end if
      call setscl (0.,1.,0.,1.)
      call box (0.,1.,0.,1.,0.,0,0)
      call setaxf (132)
      call getnsta (nsta)
      nctot = 0
      do 10  i = 1, nsta
	call getnchan (i, nchan)
	nctot = nctot + nchan + 1
   10 continue
      ymin = 0.0
      ymax = nctot
      thick = 0.7 * ydim / nctot
      if (thick .gt. 0.08) thick = 0.08
      size = 0.8 * ydim / nctot
      if (size .gt. 0.08) size = 0.08
      call axis (xdim, ydim, 0.9, 0.9, 1.0, 1.0,
     1           tmax, tmin, ymin, ymax, 0.0, 0.0,
     2           0.0, 0.0, '(none)', '(none)',
     3           ' ', ' ', str, 1)
      call chrsiz (0.08, 1.0, 0.0)
      call fe2h (tstart, iyear, iday, ihour, iminute, sec)
      idates = iyear*1000 + iday
      call fh2e (iyear, iday, 0, 0, 0.0, t0)
      tmind = (tstart - t0)/86400.0d0
      tmaxd = (tend - t0)/86400.0d0
      call fe2h (tend, iyear, iday, ihour, iminute, sec)
      idatee = iyear*1000 + iday
      ndays = (tend - tstart) / 86400.0 + 0.5
      do 50  i = idates, idates+ndays
        tdate = t0 + (i-idates)*86400.0d0 + 1.0
        call fe2h (tdate, iyear, idoy, ihour, iminute, sec)
	if (idoy .le. 365) then
	  call fdoy2mday (idoy, iyear, imonth, iday)
	  if (iday .eq. 1) then
	    call fh2e (iyear, idoy, 0, 0, 0.0, ttmp)
	    ttmp = ttmp - tstart
            call setscl (tmin, tmax, 0.0, ydim)
	    y = -0.1
	    x = ttmp
	    if (iyear .lt. 2000) then
	      iyr = iyear - 1900
      	    else
      	      iyr = iyear - 2000
	    end if
	    write (str, '(i2.2,a,i2.2)') iyr, '/', imonth
            call chrsiz (size, 1.0, 0.0)
	    call text (x, y, 90.0, 7, str, 1)
            call setscl (tmin, tmax, ymax, ymin)
	    x1 = ttmp
	    x2 = ttmp
	    y1 = ymax
	    y2 = ymin
	    call line (x1, y1, x2, y2, 0.01, 0, 0)
	    go to 50
	  end if
	end if
	if (ndays .lt. 100) then
 	  call fh2e (iyear, idoy, 0, 0, 0.0, ttmp)
 	  ttmp = ttmp - tstart
          call setscl (tmin, tmax, 0.0, ydim)
	  y = -0.1
	  x = ttmp
c         Mitch changed so compile in Antelope 5.2_64
c	  write (str, '(i)') iyear*1000+idoy
	  write (str, '(i7.0)') iyear*1000+idoy
          call chrsiz (0.09, 1.0, 0.0)
	  call text (x, y, 90.0, 7, str, 1)
          call setscl (tmin, tmax, ymax, ymin)
 	  x1 = ttmp
 	  x2 = ttmp
 	  y1 = ymax
 	  y2 = ymin
 	  call line (x1, y1, x2, y2, 0.0, 0, 0)
	end if
   50 continue
      ictot = 0
      call chrsiz (size, 1.0, 0.0)
      do 20  i = 1, nsta
	call getnchan (i, nchan)
	call getsta (i, sta)
	do 30  j = 1, nchan
	  call getchan (i, j, chan)
	  ictot = ictot + 1
	  if (ihist .eq. 1) then
	    call setdim (xdim, ydim, 1.0, 1.0)
            call setscl (0.0, xdim, ymax, ymin)
	    x = -0.3
	    y = ictot + 0.4
	    if (j .eq. 1) then
	      write (str, '(a,a,a)') sta(1:4), ' ', chan(1:3)
	    else
	      write (str, '(a,a,a)') '    ', ' ', chan(1:3)
	    end if
            call chrsiz (size, 1.0, 0.0)
	    call text (x, y, 0.0, 7, str, 1)
	    call setdim (xdim, 2.0*ydim/nctot, 1.0, 
     +                   1.0+ydim-((y+0.2)*ydim/nctot))
            call setscl (0.0, xdim, 0.0, 5.0)
            call chrsiz (0.8*size, 1.0, 0.0)
            x = -0.1
            call text (x, 0.0, 0.0, 7, '0', 1)
            write (arg, *) ntrigs
            call text (x, 2.5, 0.0, 7, arg, 1)
	    call line (-0.08, 0.0, xdim, 0.0, 0.0, 0, 1)
	    call line (-0.08, 2.5, xdim, 2.5, 0.0, 0, 1)
            call setscl (tmind, tmaxd, 0.0, 5.0)
	    call getnseg (i, j, nseg)
	    ndata = 0
	    do 45  k = 1, nseg
	      call getseg (i, j, k, t1, t2)
              if (ndata .eq. NDMAX) then
                print *,'Too many data points'
                stop
              end if
              ndata = ndata + 1
              data(ndata) = (t1 - t0)/86400.0d0
   45       continue
            call nhistbin (ndata, data, 0, 1.0, 0,
     +                     0.5, 1.0, NBMAX, nhist,
     +                     bhist, bwhist, yhist)
            do 46  k = 1, nhist
              if (yhist(k) .le. 1000000.5) then
                yhist(k) = 2.5*yhist(k)/ntrigs
              else if (yhist(k) .lt. 0.5) then
                yhist(k) = 0.0
	      else
	        yhist(k) = 0.5 + 2.0*alog10(yhist(k))
	      end if
   46       continue
            call setfor (0.0, 0.5, 0.0)
            call nhistplot (nhist, bhist, bwhist, yhist,
     +                     0, 1, 0.0)
            call setfor (0.0, 0.0, 0.0)
	  else
            call setscl (0.0, xdim, ymax, ymin)
	    x = -0.1
	    y = ictot
	    if (j .eq. 1) then
	      write (str, '(a,a,a)') sta(1:4), ' ', chan(1:3)
	    else
	      write (str, '(a,a,a)') '    ', ' ', chan(1:3)
	    end if
	    call text (x, y, 0.0, 7, str, 1)
            call setscl (tmin, tmax, ymax, ymin)
	    call getnseg (i, j, nseg)
	    do 35  k = 1, nseg
	      call getseg (i, j, k, t1, t2)
	      if (k .eq. 1) then
	        t1old = t1
	      else
	        if (t1-t2old .gt. 0.5d0) then
	          ttmp = t1old - tstart
	          x1 = ttmp
	          ttmp = t2old - tstart
	          x2 = ttmp
	          y1 = ictot
	          y2 = ictot
	          call line (x1, y1, x2, y2, thick, 0, 0)
	          t1old = t1
	        end if
	      end if
	      t2old = t2
   35       continue
	    ttmp = t1old - tstart
	    x1 = ttmp
	    ttmp = t2old - tstart
	    x2 = ttmp
	    y1 = ictot
	    y2 = ictot
	    call line (x1, y1, x2, y2, thick, 0, 0)
          end if
   30   continue
        ictot = ictot + 1
   20 continue
      call finitt
      stop
  999 write (6, '(a,a)') 'Read error on ',arg
      stop
      end

c $Id: aeic_dbplotcov.f,v 1.1.1.1 2000-05-23 23:27:54 kent Exp $ 
