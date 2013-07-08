      program beampat
      parameter (MAXELE = 100, MAXP = 501)
c      include "scv2.i"
#include "db.i"
c
      real*8 rx(MAXELE),ry(MAXELE)
      real*4 px(MAXP),py(MAXP)
      real*4 gain(MAXP*MAXP)
      real*4 rrx(MAXELE),rry(MAXELE)
      integer*4 npx, npy, ncont
      integer*4 mydb(4), mydbt(4)
c
      real*4 tau(MAXELE), a(MAXELE), argy(MAXELE)
      character*80 title, ctype, ctype2, ptype, dbnam, carg
      character*80 expr
      character*20 net
c
      common /pfile/ ipunit
c
c    ARCESS array geometry
c
      data  nele / 25 /
      data  (rx(i),i=1,25) / 0.004, 0.049, 0.108, -0.143,
     +      0.070, 0.334, 0.143, -0.228, -0.272,
     +      0.109, 0.603, 0.647, 0.208, -0.396, -0.687, -0.447,
     +   0.192, 1.098, 1.493, 1.189, 0.335, -0.681, -1.368,
     +  -1.336, -0.802 /
c
      data  (ry(i),i=1,25) / 0.003, 0.146, -0.103, -0.030,
     +      0.321, 0.030, -0.298, -0.217, 0.163,
     +      0.687, 0.341, -0.238, -0.657, -0.569, -0.048, 0.548,
     +   1.480, 1.015, 0.076, -0.901, -1.451, -1.326, -0.566,
     +   0.414, 1.257 /
c
      data  (tau(i),i=1,MAXELE) / MAXELE*0.0 /
      data  (a(i),i=1,MAXELE) / MAXELE*1.0 /
c
      if (iargc() .gt. 0) then
        karg = 0
        karg = karg + 1
        call getarg (karg, title)
        dbnam = title
        karg = karg + 1
	call getarg (karg, net)
	karg = karg + 1
        call getarg (karg, carg)
        read (carg, *) npx
        karg = karg + 1
        call getarg (karg, carg)
        read (carg, *) npy
        karg = karg + 1
        call getarg (karg, carg)
        read (carg, *) pxmin
        karg = karg + 1
        call getarg (karg, carg)
        read (carg, *) pxmax
        karg = karg + 1
        call getarg (karg, carg)
        read (carg, *) pymin
        karg = karg + 1
        call getarg (karg, carg)
        read (carg, *) pymax
        karg = karg + 1
        call getarg (karg, carg)
        read (carg, *) freq
        karg = karg + 1
        call getarg (karg, ctype2)
        go to 111
      end if
c
      print *,'enter db name: '
      read (5, '(a)') title
      dbnam = title
      print *,'enter affiliation-table sifting string '
      print *,'("none" uses whole site table): '
      read (5, '(a)') net
      print *,'enter npx and npy: '
      read (5,*) npx,npy
      print *,'enter min and max px: '
      read (5,*) pxmin, pxmax
      print *,'enter min and max py: '
      read (5,*) pymin, pymax
      print *,'enter frequency in Hz: '
      read (5,*) freq
      print *,'enter contour type (log, lin): '
      read (5, '(a)') ctype2
c  111 call db30create (title, idb)
  111 rc = dbopen( title, 'r', mydb )
      call dblookup( mydb, mydb, '', 'site', '', '' )
      if( net .ne. 'none' ) then 
        call dblookup( mydbt, mydb, '', 'affiliation', '', '' )
        call strtbl( itbl, 'sta', 0 )
        call dbjoin( mydbt, mydbt, mydb, itbl, itbl, 0,0,'' )
        call dbquery( mydbt, dbRECORD_COUNT, njoined )
        if( njoined .le. 0 ) then
	  print *, 'No records joined to affiliation table'
	  call exit(1)
        else
	  write( expr, '(a,a,a)' ) 'net == "',net,'"'
	  irc = ideblank( expr )
	  call dbsubset( mydbt, mydbt, expr, '' )
	  call dbquery( mydbt, dbRECORD_COUNT, njoined )
	  if( njoined .le. 0 ) then
	    print *,'No stations matched expression given'
	    call exit(1)
	  else
	    mydb(1) = mydbt(1)
	    mydb(2) = mydbt(2)
	    mydb(3) = mydbt(3)
	    mydb(4) = mydbt(4)
	  end if
        end if
      end if 
      call dbquery( mydb, dbRECORD_COUNT, nsite )
      nele = nsite
      if (nele .gt. MAXELE) nele = MAXELE
      rxmin = 1.e30
      rxmax = -1.e30
      rymin = 1.e30
      rymax = -1.e30
      print*,'nsite=',nsite
      do 5  i = 1, nsite
c	call dbgetattrs (idb, 'site', i, 
c     +                         'deast', rx(i),
c     +                         'dnorth', ry(i), 0)
      mydb(4) = i-1
      rc = dbgetv( mydb, '', 'deast', rx(i),
     +                           'dnorth', ry(i), 0)
      call clear_register(1)
        if (rx(i) .lt. rxmin) rxmin = rx(i)
        if (ry(i) .lt. rymin) rymin = ry(i)
        if (rx(i) .gt. rxmax) rxmax = rx(i)
        if (ry(i) .gt. rymax) rymax = ry(i)
        rrx(i) = rx(i)
        rry(i) = ry(i)
        print*,'rrxrry&rc=',rx(i),ry(i),rc
    5 continue
      dpx=(pxmax-pxmin)/(npx-1)
      dpy=(pymax-pymin)/(npy-1)
      do 10  i = 1, npx
      px(i) = pxmin + (i-1)*dpx
   10 continue
      do 11  i = 1, npy
      py(i) = pymin + (i-1)*dpy
   11 continue
      omega = freq*2.*3.14159265
      xmul=-0.2*(pxmax-pxmin)
      do 15  k = 1, nele
	tau(k) = omega*tau(k)
	rx(k)  = omega*rx(k)
	ry(k)  = omega*ry(k)
   15 continue
      n = 0
      gmin = 1.e30
      gmax = -1.e30
      do 20  i = 1, npy
	do 25  k = 1, nele
	  argy(k) = - tau(k) + ry(k)*py(i)
   25   continue
	do 20  j = 1, npx
	  gainr = 0.
	  gaini = 0.
	  do 30  k = 1, nele
	    arg = argy(k) + rx(k)*px(j)
	    gainr = gainr + a(k)*cos(arg)
	    gaini = gaini + a(k)*sin(arg)
   30     continue
	  n = n + 1
	  gain(n) = sqrt(gainr*gainr + gaini*gaini) / nele
	  if (ctype2 .eq. 'log') gain(n) = alog10(gain(n))
	  if (gain(n) .gt. gmax) gmax = gain(n)
	  if (gain(n) .lt. gmin) gmin = gain(n)
   20 continue
      nx = npx
      ny = npy
c     call wgrid('antara.dat', freq, nx, ny, px, py, gain)
c
c 200 continue
  200 print *,'minimum and maximum gains = ',gmin,gmax
      if (iargc() .gt. 0) then
        karg = karg + 1
        call getarg (karg, carg)
        read (carg, *) zstart
        karg = karg + 1
        call getarg (karg, carg)
        read (carg, *) dz
        karg = karg + 1
        call getarg (karg, carg)
        read (carg, *) ncont
        karg = karg + 1
        call getarg (karg, ptype)
        karg = karg + 1
        call getarg (karg, ctype)
      else
        print *,'enter min, increment, and number of gain contours: '
        read (5,*) zstart, dz, ncont
        print *,'enter plot type (portrait, landscape): '
        read (5, '(a)') ptype
        print *,'enter contour type (lines, colors, both): '
        read (5, '(a)') ctype
      end if
      if (ptype .eq. 'write') then
        call wgrid (ctype, npx, npy, px, py, gain)
        stop
      else if (ptype .eq. 'landscape') then
	itran = 1
      else
	itran = 0
      end if
c
      if (ctype .eq. 'colors') then
        call initt (itran, 'beampat.ps', ' ', 'beampat', 0.9, 0.0, 0.0)
      else
        call initt (itran, 'beampat.ps', ' ', 'beampat', 0.9, 0.0, 0.0)
      end if
c     if (ctype .eq. 'colors') then
c       call setbac (240.0, 0.1, 0.8)
c       call setfor (0.0, 0.8, 0.0)
c     end if
      call clear
      call setdim (3.5, 3.5, 1.0, 1.0)
      call setscl (pxmin, pxmax, pymin, pymax)
c     if (ctype .eq. 'colors') then
c       call setfor (0.0, 0.2, 0.0)
c     end if
      call ncontour (npx, npx, npy, px, py, gain, ctype,
     +               zstart, zstart+dz*ncont, dz)
      if (ctype2 .eq. 'log') then
        write (title, '(a,f4.1,a)') 'Array Log Gain at ',freq, ' Hz'
      else
        write (title, '(a,f4.1,a)') 'Array Gain at ',freq, ' Hz'
      end if
      call setaxf (132)
c     if (ctype .eq. 'colors') then
c       call setfor (0.0, 1.0, 1.0)
c     end if
      call axis (3.5, 3.5, 0.8, 0.8, 1., 1., pxmax, pxmin,
     +           pymax, pymin, 0.01, 0.1, 0.01, 0.1, '(f6.1)', 
     +           '(f6.1)',
     +           'East slowness (sec/km)',
     +           'North slowness (sec/km)', title, 0)
      if (ctype .eq. 'lines') then
        call finitt
        stop
      end if
      if (itran .eq. 1) then
        call setdim (3.5, 0.5, 1.0, 6.0)
        call setscl (zstart, zstart+dz*ncont, 0.0, 1.0)
        do 222  i = 1, ncont
	  px(i) = zstart * (i-1)*dz
	  gain(i) = px(i)
	  gain(i+ncont) = gain(i)
  222   continue
        py(1) = 0.0
        py(2) = 1.0
        call ncontour (ncont, ncont, 2, px, py, gain, ctype,
     +               zstart, zstart+dz*ncont, dz)
      else
        call setdim (6.0, 0.5, 1.0, 8.5)
        call setscl (zstart, zstart+dz*ncont, 0.0, 1.0)
        do 220  i = 1, ncont
	  px(i) = zstart * (i-1)*dz
	  gain(i) = px(i)
	  gain(i+ncont) = gain(i)
  220   continue
        py(1) = 0.0
        py(2) = 1.0
        call ncontour (ncont, ncont, 2, px, py, gain, ctype,
     +               zstart, zstart+dz*ncont, dz)
      end if
      if (ctype2 .eq. 'log') then
        if (itran .eq. 1) then
          call axis (3.5, 0.5, 0.5, 0.8, 1.0, 6.0,
     +           20.0*(zstart+dz*ncont), 20.0*zstart, 
     +           1.0, 0.0, 0.0001, 0.001, 0.0, 0.0, 
     +           '(*)', '(f6.1)', 
     +           'Gain DB', '', '', 0)
	else
          call axis (6., 0.5, 0.8, 0.8, 1., 8.5, 
     +           20.0*(zstart+dz*ncont),
     +           20.0*zstart, 1.0, 0.0, 0.0001, 0.001, 0.0, 0.0, 
     +           '(*)', '(f6.1)',
     +           'Gain DB', '', '', 0)
	end if
      else
        if (itran .eq. 1) then
          call axis (3.5, 0.5, 0.5, 0.8, 1., 6.0, (zstart+dz*ncont),
     +           zstart, 1.0, 0.0, 0.0001, 0.001, 0.0, 0.0, 
     +           '(*)', '(f6.1)',
     +           'Gain', '', '', 0)
	else
          call axis (6., 0.5, 0.8, 0.8, 1., 8.5, (zstart+dz*ncont),
     +           zstart, 1.0, 0.0, 0.0001, 0.001, 0.0, 0.0, 
     +           '(*)', '(f6.1)',
     +           'Gain', '', '', 0)
	end if
      endif
      rxr = rxmax - rxmin
      ryr = rymax - rymin
      if (rxr .gt. ryr) then
      else
        rxr = ryr
      end if
      rxmid = 0.5*(rxmin + rxmax)
      rymid = 0.5*(rymin + rymax)
      rxmin = rxmid - 0.55*rxr
      rxmax = rxmid + 0.55*rxr
      rymin = rymid - 0.55*rxr
      rymax = rymid + 0.55*rxr
      if( net .eq. 'none' ) then 
        write (title, '(a,a)') 'Array geometry for ',
     +               dbnam(1:ilen(dbnam))
      else
        write (title, '(a,a)') 'Array geometry for ',
     +               net
      end if
      call axis (3.5, 3.5, 0.8, 0.8, 6., 1., rxmax, rxmin,
     +           rymax, rymin, 0.01, 0.1, 0.01, 0.1, '(f6.2)', 
     +           '(f6.2)',
     +           'Relative east (km)',
     +           'Relative north (km)', title, 0)
      call setbac (0.0, 0.8, 1.0)
      do 800  i = 1, nele
        print*,rxmin,rxmax,rymin,rymax,rrx(i),rry(i)
        call symbol ('box', rrx(i), rry(i), 0.1, 0.0, 0, 1)
  800 continue
      call finitt
      stop
      end
      subroutine symbol (type, x, y, size, thick, iclip, ifill)
      character*(*)      type
c
c     common /pscl/ xmin,xmax,ymin,ymax,xscale,yscale,xrange,yrange
c
      real*4 xplt(10), yplt(10)
      character*8 xtype, ytype
c
      call nsymbol (type, x, y, size, thick, iclip, ifill)
c
      return
      end
