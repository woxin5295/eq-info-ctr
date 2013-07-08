      real*4 xlat1, xlon1, xlat2, xlon2, xmb, ymb
      real*8 evtime
      character*80 dbnam, sta, arg, auth(20), sym(20)
      character*80 tstart, tend, atext, title, custom_title
      character*80 lblfile(20),bp_lbl_file, temp
      character*512 filename_in 
      character*512 special_files
      character*512 prof_coord
      character*80 prof_title
      real*4 symhue(20), symlit(20), symsat(20)
      parameter  (MAXEVENTS = 100000)
c     integer*4 ievs(MAXEVENTS)
      integer*8 ievs(MAXEVENTS)
      integer*4 dep1,dep2,dep3
      character*1 ans
      character*1 prf
      character*1 caca
      character*2 prf2
      character*20 profile
      character*80  psfile
      real*4      x11(10),y11(10),x22(10),y22(10)
      real*4      xr1(10),yr1(10),xr2(10),yr2(10)
      real*4      xxp(1000),yyp(1000)
      external setans
c     common /setanss/ ans
c
#include "EV_f.i"
#include "dbl2.i"
c
      data  pi / 3.14159265358979323846d0 /
      data  tstart / ' ' /
      data  tend / ' ' /
c
      aspect = 9.8/7.3
      dep1 = 30
      dep2 = 75
      dep3 = 125
c
c     kint = signal (2,setans,-1)
      if (iargc() .lt. 3) then
	print *,
     +'usage: ak_dbmapevents dbname {sta | orid | lat:lon} range ',
     +'                   [-c] [-l] [-o outfile] ',
     +'[auth1 sym1 hue1 lit1 sat1 ...]'
	stop
      end if
c     call set_ieeehandlers
c
c    Parse command line
c
      call getarg (1, dbnam)
      call getarg (2, sta)
      do 1  i = 1, len(sta)
	if (sta(i:i) .eq. ':') go to 2
    1 continue
      xlat1 = -100.0d0
      xlon1 = -100.0d0
      go to 3
    2 continue
      read (sta(1:i-1), *, err=58) xlat1
      read (sta(i+1:ilen(sta)), *, err=58) xlon1
      if (xlon1 .gt. 180.d0) xlon1 = xlon1 - 360.0d0
      if (xlon1 .lt. -180.d0) xlon1 = xlon1 + 360.0d0
    3 continue
      call getarg (3, arg)
      do 5  i = 1, len(arg)
	if (arg(i:i) .eq. ':') go to 6
    5 continue
      read (arg, *) range
      xmax = range
      xmin = -range
      ymax = range
      ymin = -range
      go to 7
    6 continue
      read (arg(1:i-1), *, err=58) xlat2
      read (arg(i+1:ilen(arg)), *, err=58) xlon2
      if (xlon2 .gt. 180.d0) xlon2 = xlon2 - 360.0d0
      if (xlon2 .lt. -180.d0) xlon2 = xlon2 + 360.0d0
      if (xlat1 .gt. -100.0d0) then
        xmax =  0.5d0*(xlon2-xlon1)
        xmin = -0.5d0*(xlon2-xlon1)
        ymax =  0.5d0*(xlat2-xlat1)
        ymin = -0.5d0*(xlat2-xlat1)
        xlat1 = 0.5d0*(xlat2+xlat1)
        xlon1 = 0.5d0*(xlon2+xlon1)
      end if
    7 continue
      n = iargc()
      nauth = 0
      i = 4
      inter = 0
      itran = 0
      iprof = 0
      psfile = 'ak_dbmapevents.ps'
   66 continue
	if (i .gt. n) go to 67
          call getarg (i, arg)
	  if (arg .eq. '-c') then
	    inter = 1
	    i = i + 1
	    go to 66
	  end if
	  if (arg .eq. '-l') then
	    itran = 1
            xmax = range*9.8/6.0
            xmin = -range*9.8/6.0
	    i = i + 1
	    go to 66
	  end if
	  if (arg .eq. '-p') then
	    iprof = 1
	    i = i + 1
	    go to 66
	  end if
	  if (arg .eq. '-o') then
	    i = i + 1
	    call getarg( i, psfile )
	    i = i + 1
	    go to 66
	  end if
	if (n-i .lt. 4) then
	  print *,'ak_dbmapevents: Too few arguments'
	  stop
	end if
	nauth = nauth + 1
	call getarg (i, auth(nauth))
	call getarg (i+1, sym(nauth))
	call getarg (i+2, arg)
	read (arg, *) symhue(nauth)
	call getarg (i+3, arg)
	read (arg, *) symlit(nauth)
	call getarg (i+4, arg)
	read (arg, *) symsat(nauth)
	i = i + 5
      go to 66
   67 continue
c
c    Open database
c
      call db30create (dbnam, idb)
      if (idb .eq. 0) then
	print *,'ak_dbmapevents: db30create error'
	stop
      end if
c
c    Read in site data and find reference station
c
      jorid = -1
      call dbgetntuples (idb, RELID_SITE, ntuples)
      if (ntuples .lt. 1) then
	go to 64
      end if
      do 55  i = 1, ntuples
	call dbgetattrs (idb, RELID_SITE, i,
     +                   ATTRID_LON, xlon,
     +                   ATTRID_LAT, xlat,
     +                   ATTRID_STA, arg,
     +			 0)
	if (sta .eq. arg) then
	  xlat1 = xlat
	  xlon1 = xlon
	  if (xlon1 .gt. 180.d0) xlon1 = xlon1 - 360.0d0
	  if (xlon1 .lt. -180.d0) xlon1 = xlon1 + 360.0d0
	  go to 56
	end if
   55 continue
   64 do 65  i = 1, len(sta)
	if (sta(i:i) .eq. ':') go to 68
   65 continue
      read (sta, *) iorid
      call dbgetntuples (idb, RELID_ORIGIN, ntuples)
      if (ntuples .lt. 1) then
	print *,'ak_dbmapevents: No origin tuples'
	stop
      end if
      do 57  i = 1, ntuples
	call dbgetattrs (idb, RELID_ORIGIN, i,
     +                   ATTRID_LON, xlon,
     +                   ATTRID_LAT, xlat,
     +                   ATTRID_ORID, jorid,
     +                   ATTRID_TIME, evtime,
     +			 0)
	if (jorid .eq. iorid) then
	  write (sta, '(a,i4)') 'orid = ',iorid
	  xlat1 = xlat
	  xlon1 = xlon
	  if (xlon1 .gt. 180.d0) xlon1 = xlon1 - 360.0d0
	  if (xlon1 .lt. -180.d0) xlon1 = xlon1 + 360.0d0
	  go to 56
	end if
   57 continue
   58 print *,'ak_dbmapevents: reference station or origin not found'
      stop
   68 continue
   56 continue
c
c    Set up plotting
c
c     print*,'About to read in parameters with get_params'
c
c    Read in parameters
c
      filename_in = 'ak_dbmapevents'
      call get_params(filename_in,
     &  custom_title,special_files,nsf,
     &  istaplt,istnam,ipdeplt,ipdepth,
     &  iporid,icities,ipipe,
     &  iblue,ipmag,idcirc,ititl,
     &  ipumps,iflt,icol,itran,ilegnd)
c     print*,'nsf: ',nsf
c     print*,'special files: ',special_files
      if(ititl.eq.1) then
        write(title,'(a)') custom_title
      endif
      if(itran.eq.1) then
        xmax = range*9.8/6.0
        xmin = -range*9.8/6.0
        xdim = 9.8
        ydim = 6.0
        xlow = 0.1
        ylow = 1.3
      else
        xdim = 7.3
        ydim = 7.3
        xlow = 0.1
        ylow = 2.0
      endif
      nevs = 0
      x = 0.5*(xmin+xmax)
      y = 0.5*(ymin+ymax)
      inset = 0
      j1=1
      do i=1,nsf
        do j = j1,512
          if(special_files(j:j).eq.' ') then
            j2 = j-1
            lblfile(i) = special_files(j1:j2)
            j1 = j+1
            go to 303
          endif
        enddo
 303    continue
      enddo
c     nsf = 2
c     lblfile(1) = 'fort.54'
c     lblfile(2) = 'junk.bp'
c
c    Make plot
c
  100 call plotit (itran, xdim, ydim,  xlow, ylow,
     +                   xmax, xmin, ymax, ymin, dbnam,
     +                   sta, jorid, xlat1, xlon1, evtime,
     +                   nauth, auth, sym, symhue, symlit, symsat,
     +                   idb, ilegnd, nevs, ievs, iporid, ipdepth,
     +                   istaplt, istnam, ipdeplt,
     +                   ipmag, idcirc, icities, ipipe, ipaths,
     +                   iblue, icol, ititl, iflt, inset, title, 
     +                   tstart, tend, dep1, dep2, dep3, psfile)
      if (nsf.gt.0) then
        if(ilegnd.eq.1)then
          ybp = ydim
        else
          ybp = 0.
        endif
        ibp = 0
        do i=1,nsf
        temp = ' '
        temp = lblfile(i)
        call get_data_path(temp,bp_lbl_file)
        open(33,file=bp_lbl_file,err=237)
        read(33,'(a)',end=237)atext
        if(atext(1:5).eq.'label') then
  238 read(33,'(5f10.4,i4,i2,a80)',end=237)alat,alon,alat1,alon1,size,
     +                               ifnt,ipos,atext
	  call latlon2xydel (xlat1, xlon1, alat, alon,
     +                     xplt1, yplt1)
          if(alon1.lt.-900.0) then
            angle = alat1
          else
	    call latlon2xydel (xlat1, xlon1, alat1, alon1,
     +                       x2, y2)
            angle = (180./3.14159)*atan2((y2-yplt1),(x2-xplt1))
          endif
          if(ifnt.ne.0) call cfont(ifnt)
          call chrsiz(size,1.0,0.0)
          call text (xplt1, yplt1, angle, ipos, atext, 0)
          if(ifnt.ne.0) call cfont(132)
          go to 238
        else
          call setscl(0.,xdim,0.,ydim)
  241     read(33,'(a80)',end=239)atext
          ibp = ibp + 1
          if(ibp.gt.1) then
            call chrsiz(0.08,1.0,0.0)
            delbp = .15
          else
            call chrsiz(0.1,1.0,0.0)
            delbp = .19
          endif
          if(ilegnd.eq.1) then
            xbp = (xdim*.34)/2.
            if(itran.eq.0)xbp = xbp * aspect
          else
            xbp = xdim/2.0
          endif
          ybp = ybp - delbp
          call text (xbp, ybp, 0.0, 3, atext, 1)
          go to 241
  239     call setscl(xmin,xmax,ymin,ymax)
        endif
  237   continue
        close (33)
        enddo
  240   continue
      endif
      akmpinch = 111.195*2.0*ymax/ydim
      if(akmpinch.lt.1) then
        alen = 1./akmpinch
        ntics = 10
        atext = ' '
        atext = '1'
      else if(akmpinch.lt.10) then
        alen = 10./akmpinch
        ntics = 10
        atext = ' '
        atext = '10'
      else if(akmpinch.lt.50) then
        alen = 50./akmpinch
        ntics = 5
        atext = ' '
        atext = '50'
      else if(akmpinch.lt.100) then
        alen = 100./akmpinch
        ntics = 10
        atext = ' '
        atext = '100'
      else if(akmpinch.lt.200) then
        alen = 200./akmpinch
        ntics = 2
        atext = ' '
        atext = '200'
      else if(akmpinch.lt.300) then
        alen = 300./akmpinch
        ntics = 3
        atext = ' '
        atext = '300'
      else if(akmpinch.lt.500) then
        alen = 500./akmpinch
        ntics = 5
        atext = ' '
        atext = '500'
      else if(akmpinch.lt.1000) then
        alen = 1000./akmpinch
        ntics = 10
        atext = ' '
        atext = '1000'
      else if(akmpinch.lt.2000) then
        alen = 2000./akmpinch
        ntics = 2
        atext = ' '
        atext = '2000'
      else if(akmpinch.lt.5000) then
        alen = 5000./akmpinch
        ntics = 5
        atext = ' '
        atext = '5000'
      else if(akmpinch.lt.10000) then
        alen = 10000./akmpinch
        ntics = 10
        atext = ' '
        atext = '10000'
      endif
      x1 = xdim-alen-0.5
      x2 = xdim-0.5
      y1 = 0.35
      y2 = 0.35
      dtic = alen/ntics
      call chrsiz(0.08,1.0,0.0)
      call setscl(0.,xdim,0.,ydim)
      call line(x1,y1,x2,y2,0.01,0,0)
      call line(x1,y1,x1,y1+.12,0.01,0,0)
      call line(x2,y2,x2,y2+.12,0.01,0,0)
      call text (x1, y1+.12, 0.0, 3, '0', 1)
      call text (x2, y2+.12, 0.0, 3, atext, 1)
      call text (x1+alen/2.0, y2-.05, 0.0, 5, 'kilometers', 1)
      do i=1,ntics-1
        call line(x1+dtic*i,y1,x1+dtic*i,y1+.06,0.01,0,0)
      enddo
      call setscl(xmin,xmax,ymin,ymax)
c
c    Check for cursor entries
c
      if (inter .eq. 0) then
        call finitt
	if (iprof .eq. 1) then
          prof_title = ' '
          call get_profile('profile',prof_title,prof_coord,nprf,
     +                    width,depp,ang,scal,vex)
          print*,'From pfile',nprf,width,depp,ang,scal,vex,
     +         prof_title,prof_coord
	  go to 996
  998	end if
        stop
      end if
c 110 if(ans.ne.'S') call cursor (x,y,ans)
  110 call cursor (x,y,ans)
      if (ans .eq. 'q') then
        call finitt
	stop
      else if (ans .eq. 'Q') then
        call finitt
	call hdkild
	stop
      else if (ans .eq. '?') then
	write (6, '(a)') 'Key  Action'
	write (6, '(a)') ' ?   help'
	write (6, '(a)') ' R   Redraw'
        write (6, '(a)') ' A   Array paths for distance and azimuth'
	write (6, '(a)') ' d   Select depth intervals'
        write (6, '(a)') ' l   Draw a line from current position'
        write (6, '(a)') ' G   Geographic Lat Lon of cursor position'
        write (6, '(a)') ' N   Set in an inset'
        write (6, '(a)') ' S   Sleep'
	write (6, '(a)') ' F   Draw faults'
	write (6, '(a)') ' T   draw text at lower left corner'
	write (6, '(a)') ' Z   draw text at lower left corner and save'
	write (6, '(a)') ' L   Toggle default title'
	write (6, '(a)') ' D   Toggle Legend Box'
	write (6, '(a)') ' b   Toggle blue of ocean'
	write (6, '(a)') ' g   Plot great circle paths'
	write (6, '(a)') ' c   Plot cities'
	write (6, '(a)') ' C   Toggle circle and hexagon symbols'
	write (6, '(a)') ' P   Plot pipeline'
	write (6, '(a)') ' q   quit and keep window'
	write (6, '(a)') ' Q   quit and destroy window'
	write (6, '(a)') ' i   small zoom in'
	write (6, '(a)') ' I   big zoom in'
	write (6, '(a)') ' o   small zoom out'
	write (6, '(a)') ' O   big zoom out'
	write (6, '(a)') ' p   pan'
	write (6, '(a)') ' E   great circle to closest event'
	write (6, '(a)') ' e   identify closest event'
	write (6, '(a)') ' s   identify closest station'
	write (6, '(a)') ' t   enter time range'
	write (6, '(a)') ' B   Black and White depth coloring on/off'
	write (6, '(a)') ' x   create depth sections of seismicity'
	write (6, '(a)') ' 1   orid labels on/off'
	write (6, '(a)') ' 2   depth coloring on/off'
	write (6, '(a)') ' 3   shallow depth coloring on/off'
	write (6, '(a)') ' 4   shallow depth diff coloring on/off'
	write (6, '(a)') ' 5   sta symbols on/off'
	write (6, '(a)') ' 6   mark pdevs on/off'
	write (6, '(a)') ' 7   mag size on/off'
        write (6, '(a)') ' 8   distance circles on/off'
        write (6, '(a)') ' 9   sta names on/off'
	go to 110
      else if (ans .eq. 'R') then
	go to 100
      else if (ans .eq. 'G') then
        call xydel2latlon (xlat1, xlon1, x, y,xlat0, xlon0)
        print*,'Latitude = ',xlat0,' Longitude = ',xlon0
	go to 110
      else if (ans .eq. 'A') then
        print*,'Enter Lat, Lon, of array site:'
        read(*,*)arrlat,arrlon
c       call xydel2latlon (xlat1, xlon1, x, y,xlat0, xlon0)
        azi = 180.
        nndist = 179
        call setfor (0.,0.5,1.0)
        do ik = 0,350,10
        azi = real(ik)
        do ij = 1,nndist,1
          dist = real(ij)
          call lola(dist,azi,arrlat,arrlon,xlat9,xlon9,num)
          call latlon2xydel (xlat1, xlon1, xlat9, xlon9,
     +                     xxp(ij), yyp(ij))
        enddo
        call nplot(nndist,xxp,yyp,0,0,.01,0,' ')
        enddo
        go to 110
      else if (ans .eq. 'S') then
        call sleep (20)
        nevs = 0
	go to 100
      else if (ans .eq. 'N') then
        if (inset.eq.0) then
          nevs = 0
          inset = 1
          call cursor(x2,y2,ans)
          if (x2.gt.x) then
            xl = x
            xr = x2
          else
            xl = x2
            xr = x
          endif
          if (y2.gt.y) then
            yb = y
            yt = y2
          else
            yb = y2
            yt = y
          endif
          ylow = ylow + (yb-ymin)*ydim/(ymax - ymin)
          xlow = xlow + (xl-xmin)*xdim/(xmax - xmin)
          xdim = (xr - xl) * xdim/(xmax - xmin)
          ydim = (yt - yb) * ydim/(ymax - ymin)
c         xdim = 5.3
c         ydim = 1.3
c         if(itran.eq.0) then
c           xlow = 2.0
c           ylow = 2.1
c         else
c           xlow = 4.0
c           ylow = 1.4
c         endif
c         rangei = 9.0
c         xmax = rangei
c         xmin = -rangei
c         ymax = rangei*1.3/5.3
c         ymin = -rangei*1.3/5.3
          xlatorg = xlat1
          xlonorg = xlon1
          print*,'Enter range (deg), and center lat and lon:'
          read(*,*)rangei,xlat1,xlon1
          if (xdim.gt.ydim) then
            ymin = -rangei
            ymax = rangei
            xmin = -rangei*xdim/ydim
            xmax = rangei*xdim/ydim
          else
            ymin = -rangei
            ymax = rangei
            xmin = -rangei*ydim/xdim
            xmax = rangei*ydim/xdim
          endif
c         xlat1 = 53.0
c         xlon1 = -178.0
          ititl = 1
          title = ' '
        else
          nevs = 0
          inset = 0
          xdim = 7.3
          ydim = 7.3
          xlow = 0.1
          ylow = 2.0
          xmax = range
          xmin = -range
          ymax = range
          ymin = -range
          if(itran.eq.1) then
            xmax = range*9.8/6.0
            xmin = -range*9.8/6.0
            xdim = 9.8
            ydim = 6.0
            xlow = 0.1
            ylow = 1.3
          else
            xdim = 7.3
            ydim = 7.3
            xlow = 0.1
            ylow = 2.0
          endif
          xlat1 = xlatorg
          xlon1 = xlonorg
          ititl = 0
        endif
        go to 100
      else if (ans .eq. 'd') then
	  write (6, '(a)') 'Enter three depth values:'
          read(5,*)dep1,dep2,dep3
          print*,'Using depth ranges - ',dep1,dep2,dep3
	go to 110
      else if (ans .eq. 'D') then
        if(ilegnd.eq.1) then
          write (6, '(a)') 'Turning default legend on'
          ilegnd = 0
        else
          write (6, '(a)') 'Custom legend for newsminer'
          ilegnd = 1
        end if
      else if (ans .eq. 'L') then
	if (ititl .eq. 1) then
	  write (6, '(a)') 'Turning default title on'
	  ititl = 0
	else
	  write (6, '(a)') 'Enter custom title for plot'
          read(5,'(a)')title
          print*,'Using title - ',title
	  ititl = 1
	end if
	go to 110
      else if (ans .eq. 'F') then
	if (iflt .eq. 1) then
	  write (6, '(a)') 'Turning fault plotting off'
	  iflt = 0
	else
	  write (6, '(a)') 'Turning fault plotting on'
	  iflt = 1
	end if
	go to 110
      else if (ans .eq. 'C') then
	if (icol .eq. 1) then
	  write (6, '(a)') 'Turning circle symbol plotting on'
	  icol = 0
	else
	  write (6, '(a)') 'Turning box symbol plotting on'
	  icol = 1
	end if
	go to 110
      else if (ans .eq. 'b') then
	if (iblue .eq. 1) then
	  write (6, '(a)') 'Turning blue ocean plotting off'
	  iblue = 0
	else
	  write (6, '(a)') 'Turning blue ocean plotting on'
	  iblue = 1
	end if
	go to 110
      else if (ans .eq. 'T') then
        Print*,'Enter text to draw:'
        read(5,'(a)')atext
        call text (x, y, 0.0, 1, atext, 0)
	go to 110
      else if (ans .eq. 'Z') then
        Print*,'Enter text to draw:'
        read(5,'(a)')atext
        Print*,'Enter size of text (inches):'
        read(5,*)size
        Print*,'Enter font number (0 for default):'
        read(5,*)ifont
        print*,'Choose angle with second cursor (h=0.0,v=90.0):'
        x1 = x
        y1 = y
        call cursor(x1,y1,ans)
        if(ans.eq.'h') then
          angle = 0.0
          x1 = x + 2.
        else if(ans.eq.'v') then
          angle = 90.0
          y1 = y + 2.
        else
          angle = (180./3.14159)*atan2((y1-y),(x1-x))
        endif
        call xydel2latlon (xlat1, xlon1, x, y,xlat0, xlon0)
        ipos = 0
        if(ifont.ne.0) call cfont(ifont)
        call chrsiz(size,1.0,0.0)
        call text (x, y, angle, ipos, atext, 0)
        call chrsiz(0.08,1.0,0.0)
        if(ifont.ne.0) call cfont(132)
        call xydel2latlon (xlat1, xlon1, x1, y1,xlat01, xlon01)
        if(ans.eq.'h'.or.ans.eq.'v') then
        anum = -999.0
        write(54,'(5f10.4,i4,i2,a80)')xlat0,xlon0,angle,anum,size,
     +                             ifont,ipos,atext
        else
        write(54,'(5f10.4,i4,i2,a80)')xlat0,xlon0,xlat01,xlon01,size,
     +                             ifont,ipos,atext
        endif
	go to 110
      else if (ans .eq. 'g') then
	if (ipaths .eq. 1) then
	  write (6, '(a)') 'Turning great circle plotting off'
	  ipaths = 0
	else
	  write (6, '(a)') 'Turning great circle plotting on'
	  ipaths = 1
	end if
	go to 110
      else if (ans .eq. 'c') then
	if (icities .eq. 1) then
	  write (6, '(a)') 'Turning city plotting off'
	  icities = 0
	else
	  write (6, '(a)') 'Turning city plotting on'
	  icities = 1
	end if
	go to 110
      else if (ans .eq. 'P') then
	if (ipipe .eq. 1) then
	  write (6, '(a)') 'Turning pipeline plotting off'
	  ipipe = 0
	else
	  write (6, '(a)') 'Turning pipeline plotting on'
	  ipipe = 1
	end if
	go to 110
      else if (ans .eq. 'i') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + 0.8*xx
	xmin = x - 0.8*xx
	ymax = y + 0.8*yy
	ymin = y - 0.8*yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'I') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + 0.2*xx
	xmin = x - 0.2*xx
	ymax = y + 0.2*yy
	ymin = y - 0.2*yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'o') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + 1.25*xx
	xmin = x - 1.25*xx
	ymax = y + 1.25*yy
	ymin = y - 1.25*yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'O') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + 5.0*xx
	xmin = x - 5.0*xx
	ymax = y + 5.0*yy
	ymin = y - 5.0*yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'p') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + xx
	xmin = x - xx
	ymax = y + yy
	ymin = y - yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'x') then
        prof_title = ' '
        call get_profile('profile',prof_title,prof_coord,nprf,
     +                    width,depp,ang,scal,vex)
        print*,'From pfile',nprf,width,depp,ang,scal,vex,
     +         prof_title,prof_coord
        if(nprf.eq.0) then
          call xydel2latlon (xlat1, xlon1, x, y,xlat0, xlon0)
          print*,'Latitude = ',xlat0,' Longitude = ',xlon0
          print*,'select second point (use # of profiles desired):'
          x1 = x
          y1 = y
          call cursor (x1 ,y1 ,ans)
          call xydel2latlon (xlat1, xlon1, x1, y1,xlat0, xlon0)
          print*,'Latitude = ',xlat0,' Longitude = ',xlon0
c         call text (x, y, 0.0, 3, 'A', 0)
c         call line (x1, y1, x, y, 0.03, 0, 0)
c         call text (x1, y1, 0.0, 5, "A'", 0)
          read(ans,'(i1)')nprf
          x11(1) = x
          y11(1) = y
          x22(1) = x1
          y22(1) = y1
          if(nprf.gt.1) then
            do i=2,nprf
              print*,'Enter profile number ',nprf
              call cursor (x,y,ans)
              call xydel2latlon (xlat1, xlon1, x, y,xlat0, xlon0)
              print*,'Latitude = ',xlat0,' Longitude = ',xlon0
              print*,'select second point with cursor:'
              x1 = x
              y1 = y
              call cursor (x1 ,y1 ,ans)
              call xydel2latlon (xlat1, xlon1, x1, y1,xlat0, xlon0)
              print*,'Latitude = ',xlat0,' Longitude = ',xlon0
              x11(i) = x
              y11(i) = y
              x22(i) = x1
              y22(i) = y1
c             call setchr(i,prf)
c             prf2(1:1) = prf(1:1)
c             prf2(2:2) = "'"
c             call text (x, y, 0.0, 3, prf, 0)
c             call line (x1, y1, x, y, 0.03, 0, 0)
c             call text (x1, y1, 0.0, 5, prf2, 0)
            enddo
          endif
          print*,'Enter width, depth, angle, scale, vertical ex.:'
          read(*,*)width, depp, ang, scal, vex
        else
  996     j1 = 1
          do i=1,nprf
            k=0
            do j = j1,512
              if(prof_coord(j:j).eq.' ') then
                k=k+1
                j2 = j-1
                print*,'j1 j2 = ',j1,j2
                if(k.eq.1) then
                  read(prof_coord(j1:j2),*) xr1(i)
                elseif(k.eq.2) then
                  read(prof_coord(j1:j2),*) yr1(i)
                elseif(k.eq.3) then
                  read(prof_coord(j1:j2),*) xr2(i)
                elseif(k.eq.4) then
                  read(prof_coord(j1:j2),*) yr2(i)
                endif
                j1 = j+1
                if(k.ge.4) go to 3030
              endif
            enddo
 3030       continue
            print*,xr1(i),yr1(i),xr2(i),yr2(i)
	    call latlon2xydel (xlat1, xlon1, xr1(i), yr1(i),
     +                     x11(i), y11(i))
	    call latlon2xydel (xlat1, xlon1, xr2(i), yr2(i),
     +                     x22(i), y22(i))
            print*,i,x11(i),y11(i),x22(i),y22(i)
          enddo
        endif
        do i=1,nprf
          call setchr(i,prf)
          prf2(1:1) = prf(1:1)
          prf2(2:2) = "'"
          call chrsiz (0.12, 1.0, 0.0)
          call text (x11(i), y11(i), 0.0, 7, prf, 0)
          call line (x11(i), y11(i), x22(i), y22(i), 0.03, 0, 0)
          call text (x22(i), y22(i), 0.0, 1, prf2, 0)
          call chrsiz (0.08, 1.0, 0.0)
        enddo
        if (width.eq.0.) width = 100.
        if (depp.eq.0.) depp = 200.
        if (ang.eq.0.) ang = 90.
	if (iprof.eq.1.) go to 997
        call finitt
        print*,'enter any character to continue:'
        read(5,'(a1)')caca
  997   profile(1:10) = 'profile.ps'
        if(nprf.eq.2) then
          profile(1:10) = 'profile.ps'
          call initt (0, profile, '',
     +                   'ak_dbmapevents', 0.9, 0.0, 0.0)
          ylow = 11.8
          iclear = 0
         do i=1,2
          ylow = ylow - 5.5
          if(i.eq.1)then
            ylow = 6.0
          elseif(i.eq.2)then
            ylow = 1.5
          endif
          call depplt(xlat1,xlon1,x11(i),y11(i),x22(i),y22(i),ang,width,
     +                depp,scal,vex,akmpinch,nevs,ievs,nauth,auth,i,
     +                iclear,ylow,prof_title)
         enddo
         call finitt
        else
         ylow = .8
         iclear = 1
         do i=1,nprf
          call setchr(i,prf)
          profile(8:8) = prf
          profile(9:11) = '.ps'
          call initt (0, profile, '',
     +                   'ak_dbmapevents', 0.9, 0.0, 0.0)
          call depplt(xlat1,xlon1,x11(i),y11(i),x22(i),y22(i),ang,width,
     +                depp,scal,vex,akmpinch,nevs,ievs,nauth,auth,i,
     +                iclear,ylow,prof_title)
          call finitt
         enddo
        endif
	if (iprof .eq. 1 ) go to 998
        go to 110
      else if (ans .eq. 'l') then
        print*,'select second point with cursor:'
        x1 = x
        y1 = y
        call cursor (x1 ,y1 ,ans)
        call line (x1, y1, x, y, 0.01, 0, 0)
        go to 110
      else if (ans .eq. 'E') then
        write (6, '(a,$)') 'Enter offset azimuth: '
        read (5, *) az
        az = az*3.14159/180.0
	call idevent (xlat1, xlon1, x, y, nevs, ievs, 
     +                nauth, auth)
        r = sqrt(x*x+y*y)
        az = az + atan2(x,y)
        x = r*sin(az)
        y = r*cos(az)
        call line (0.0, 0.0, x, y, 0.01, 0, 0)
        go to 110
      else if (ans .eq. 'e') then
	call idevent (xlat1, xlon1, x, y, nevs, ievs, 
     +                nauth, auth)
	go to 110
      else if (ans .eq. 's') then
	call idstation (xlat1, xlon1, x, y, idb,
     +                nauth, auth)
	go to 110
      else if (ans .eq. 't') then
        write (6, '(a,$)') 'Enter start time: '
        read (5, '(a)') tstart
        write (6, '(a,$)') 'Enter end time: '
        read (5, '(a)') tend
        nevs = 0
	go to 110
      else if (ans .eq. '1') then
	if (iporid .eq. 1) then
	  write (6, '(a)') 'Turning orid labeling off'
	  iporid = 0
	else
	  write (6, '(a)') 'Turning orid labeling on'
	  iporid = 1
	end if
	go to 110
      else if (ans .eq. 'B') then
	if (ipdepth .gt. 0) then
	  write (6, '(a)') 'Turning B & W depth coloring off'
	  ipdepth = 0
	else
	  write (6, '(a)') 'Turning B & W depth coloring on'
	  ipdepth = 4
	end if
	go to 110
      else if (ans .eq. '2') then
	if (ipdepth .gt. 0) then
	  write (6, '(a)') 'Turning depth coloring off'
	  ipdepth = 0
	else
	  write (6, '(a)') 'Turning depth coloring on'
	  ipdepth = 1
	end if
	go to 110
      else if (ans .eq. '3') then
	if (ipdepth .gt. 0) then
	  write (6, '(a)') 'Turning shallow depth coloring off'
	  ipdepth = 0
	else
	  write (6, '(a)') 'Turning shallow depth coloring on'
	  ipdepth = 2
	end if
	go to 110
      else if (ans .eq. '4') then
	if (ipdepth .gt. 0) then
	  write (6, '(a)') 'Turning shallow depth diff coloring off'
	  ipdepth = 0
	else
	  write (6, '(a)') 'Turning shallow depth diff coloring on'
	  ipdepth = 3
	end if
	go to 110
      else if (ans .eq. '5') then
	if (istaplt .eq. 2) then
	  write (6, '(a)') 
     +           'Turning unassociated station symbol plotting off'
	  istaplt = 0
	else if(istaplt .eq. 1) then
	  write (6, '(a)') 
     +           'Turning all station symbol plotting on'
	  istaplt = 2
	else if(istaplt .eq. 0) then
	  write (6, '(a)') 
     +           'Turning unassociated active station plotting on'
	  istaplt = 1
	end if
	go to 110
      else if (ans .eq. '6') then
	if (ipdeplt .gt. 0) then
	  write (6, '(a)') 
     +           'Not marking PDE events'
	  ipdeplt = 0
	else
	  write (6, '(a)') 
     +           'Marking PDE events'
	  ipdeplt = 1
	end if
	go to 110
      else if (ans .eq. '7') then
	if (ipmag .gt. 0) then
	  write (6, '(a)') 
     +           'Not marking magnitudes'
	  ipmag = 0
	else
	  write (6, '(a)') 
     +           'Marking magnitudes'
	  ipmag = 1
	end if
	go to 110
      else if (ans .eq. '8') then
        if (idcirc .gt. 0) then
          write (6, '(a)')
     +           'Not plotting distance circles'
          idcirc = 0
        else
          write (6, '(a)')
     +           'Plotting distance circles'
          idcirc = 1
        end if
        go to 110
      else if (ans .eq. '9') then
        if (istnam .gt. 0) then
          write (6, '(a)')
     +           'Not plotting station names'
          istnam = 0
        else
          write (6, '(a)')
     +           'Plotting station names'
          istnam = 1
        end if
        go to 110
      end if
c
      go to 110
      end
      subroutine plotit (itran, xdim, ydim,  xlow, ylow,
     +                   xmax, xmin, ymax, ymin, dbnam,
     +                   sta, jorid, xlatc, xlonc, evtime,
     +                   nauth, auth, sym, symhue, symlit, symsat,
     +                   idb, ilegnd, nevs, ievs, iporid, ipdepth,
     +                   istaplt, istnam, ipdeplt,
     +                   ipmag, idcirc, icities, ipipe, ipaths, iblue,
     +                   icol, ititl, iflt, inset, title, tstrt, tnd, 
     +                   dep1, dep2, dep3, psfile)
c
      parameter  (MAXEVENTS = 100000)
      parameter  (MAXPOINTS = 300000)
      real*4 xlat1, xlon1, xlat2, xlon2, xlatc, xlonc
      real*4 ppltx(10000),pplty(10000)
      real*8 depth, deptho, depthc, evtime, elat, elon, slat,slon,pi
      character*80 dbnam, sta, arg, auth(20), sym(20), title, psfile
      character*80 region, type, loc, tstart, tend, symbl, string
      real*4 symhue(20), symlit(20), symsat(20)
      real*8 slatmndp, slatmxdp, slonmndp, slonmxdp
      integer ondate,offdate
c     integer*4 ievs(MAXEVENTS)
      integer*8 ievs(MAXEVENTS)
      character*(*) tstrt, tnd
      character*30  city
      character*30  charevnts
      character*30  text1
      character*30  text2
      character*30  text3
      character*30  text4
      character*10  frmat
      integer*4 dep1,dep2,dep3
c
#include "EV_f.i"
#include "dbl2.i"
c
      data  pi / 3.14159265358979323846d0 /
      data symsiz / 0.05 /
      data symthk / 0.003 /
      data charevnts / ' ' /
c
c
c    Initialize plot
c
      if(inset.eq.0) then
      call initt (itran, psfile, '', 
     +                   'ak_dbmapevents', 0.9, 0.0, 0.0)
      endif
      if (itran .eq. 0) then
        call setdim (7.5, 10.0, 0.0, 0.0)
      else
        call setdim (10.0, 7.5, 0.0, 0.0)
      end if
      call setscl (0.,1.,0.,1.)
c     call box (0.,1.,0.,1.,0.,0,0)
      if(ititl.eq.0) then
      title = 'Earthquakes in Alaska in ' // dbnam(1:ilen(dbnam))
crah +        // ', centered at ' // sta
      endif
c
c    Plot map
c
ctest     ilegnd = 1
      if (idcirc .eq. 1) then
        dcircle = 0.0
      else
        dcircle = -1.0
      end if
      if (iblue.eq.1) call setbac (200.0, 0.95, 1.0)
      call edprojmap (xdim, ydim, xlow, ylow,
     +                      xlatc, xlonc, xmin, xmax,
     +                      ymin, ymax, dcircle, title)

      aspect = 9.8/7.3
      if(itran.eq.0) then
        ymn = ydim*0.7369863
      else
        ymn = ydim*0.68
      endif
      ymx = ydim
      if ( ilegnd .eq. 1) then
        xmn = 0.0
        xmx = xdim*0.34
        if(itran.eq.0) then
          xmx = xmx * aspect
        endif
	call setscl (0.0, xdim, 0.0, ydim)
        call clrrgn(xmn,xmx,ymn,ymx)
        call box(xmn,xmx,ymn,ymx,.03,0,1)
        call setscl (xmin, xmax, ymin, ymax)
      endif
c
c    Open event view
c
      call delbox2llbox (xlatc, xlonc, xmin, xmax, 
     +                         ymin, ymax,
     +                         xlatmn, xlatmx, xlonmn, xlonmx)
      slatmndp = xlatmn - 5.0
      slatmxdp = xlatmx + 5.0
      slonmndp = xlonmn - 5.0
      slonmxdp = xlonmx + 5.0
      if(abs(slonmndp).gt.180.0.or.abs(slonmxdp).gt.180.0) then
        slonmndp = -180.0
        slonmxdp =  180.0
      endif
      if (jorid .lt. 0) then
        if (nevs .eq. 0) then
          nevs = MAXEVENTS
          tstart = tstrt
          tend = tnd
          call evcreate ('css3.0', 1, dbnam, tstart, tend, 
     +       slatmndp, slatmxdp, slonmndp, slonmxdp, nevs, ievs)
          if (nevs .lt. 0) then
	    print *,'ak_dbmapevents: Error return from evcreate'
          end if
          if (nevs .eq. 0) then
	    print *,'ak_dbmapevents: No events within range'
          end if
	  if (nevs .eq. MAXEVENTS) then
	    print *,'ak_dbmapevents: Attempt to exceed events dimension'
	  end if
        end if
      else
        if (nevs .eq. 0) then
          nevs = MAXEVENTS
	  write (tstart, '(f17.5)') evtime-0.001
	  write (tend, '(f17.5)') evtime+0.001
          slatmndp = xlatc-0.001
          slatmxdp = xlatc+0.001
          slonmndp = xlonc-0.001
          slonmxdp = xlonc+0.001
          call evcreate ('css3.0', 1, dbnam, tstart, tend, 
     +       slatmndp, slatmxdp, slonmndp, slonmxdp, nevs, ievs)
          if (nevs .lt. 0) then
	    print *,'ak_dbmapevents: Error return from evcreate'
          end if
          if (nevs .eq. 0) then
	    print *,'ak_dbmapevents: No events within range'
          end if
	  if (nevs .eq. MAXEVENTS) then
	    print *,'ak_dbmapevents: Attempt to exceed events dimension'
	  end if
        end if
      end if
c
c    Plot symbol legend
c
c       charevents = '                         '
        if (nevs.lt.10) then
          charevnts(2:16) = ' events plotted'
          write(charevnts(1:1),'(i1)')nevs
        elseif (nevs.lt.100) then
          charevnts(3:17) = ' events plotted'
          write(charevnts(1:2),'(i2)')nevs
        elseif (nevs.lt.1000) then
          charevnts(4:18) = ' events plotted'
          write(charevnts(1:3),'(i3)')nevs
        elseif (nevs.lt.10000) then
          charevnts(5:19) = ' events plotted'
          write(charevnts(1:4),'(i4)')nevs
        else
          charevnts(6:20) = ' events plotted'
          write(charevnts(1:5),'(i5)')nevs
        endif
      if (inset.eq.1) then
c       call setscl (0.0, xdim, 0.0, ydim)
c       call text (0.3, 0.2, 0.0, 1, charevnts, 1)
c       call setscl (xmin, xmax, ymin, ymax)
      else
      if (jorid .lt. 0) then
      if (nauth .eq. 0) then
	call setscl (0.0, xdim, 0.0, ydim)
	call setfor (0.0, 0.0, 0.0)
	xplt1 = 0.2
        if(ilegnd.eq.0) then
          yplt1 = -0.2
          ypltnevnt = yplt1
        else
          yplt1 = ymn + 1.2
          ypltnevnt = yplt1 - 1.05
          call line(xmn+0.1,yplt1+0.1, xmx-0.1,yplt1+0.1,0.01,0,1)
          call line(2.07,yplt1-0.1, 2.07,yplt1-1.1,0.01,0,1)
        endif
crah    call text (0.5, -0.2, 0.0, 1, 'All events plotted', 1)
	call text (xplt1, ypltnevnt, 0.0, 1, charevnts, 1)
	if (ipdeplt .eq. 1) then
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (120.0, 0.5, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, 'Event in the PDE', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (240.0, 0.5, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, 'Event not in the PDE', 1)
	end if
	if (ipmag .eq. 1) then
	    if (xmb .gt. -999.0) then
	      ss = symsiz*(2.0 + 2.0*(xmb-2.0)/4.0)
	      symbl = 'hexagon'
            else
	      ss = symsiz
	      symbl = 'box'
            end if
          if (itran.eq.1) then
	    xplt1 = 8.0
          else
	    xplt1 = 6.0
          endif
          if(ilegnd.eq.0) then
	    ypltm = - 0.2
          else
            ypltm = ymn + 1.0
            xplt1 = 2.25
          endif
            if(icol.eq.1) then
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (240.0, 0.5, 1.0)
	  call symbol ('box', xplt1, ypltm, symsiz, symthk, 1, 1)
            else
	      call setfor (240.0, 0.5, 1.0)
	  call symbol ('box', xplt1, ypltm, symsiz, symthk, 1, 0)
            endif
	  call setfor (0.0, 0.0, 0.0)
          if (itran.eq.1) then
	    xplt1 = 8.3
          else
	    xplt1 = 6.3
          endif
          if(ilegnd.eq.1) then
            xplt1 = 2.45
          endif
	  call text (xplt1, ypltm, 0.0, 1, 'No M', 1)
	  do 33  i = 2, 5
            if (itran.eq.1) then
	      xplt1 = 8.0
            else
	      xplt1 = 6.0
            endif
            if(ilegnd.eq.1) then
              xplt1 = 2.25
            endif
	    ypltm = ypltm - 0.2
	    xmb = i
	    ss = symsiz*(2.0 + 2.0*(xmb-2.0)/4.0)
            if(icol.eq.1) then
	      call setbac (240.0, 0.5, 1.0)
	      call symbol ('hexagon', xplt1, ypltm, ss, symthk, 1, 1)
            else
	      call setfor (240.0, 0.5, 1.0)
crah          sss = ss*3*xdim/(xmax-xmin)
crah          call circle (xplt1, ypltm, sss, 90, 1, 1, 0.2, 0)
	      call symbol ('circle', xplt1, ypltm, ss, symthk, 1, 0)
            endif
	    call setfor (0.0, 0.0, 0.0)
	    call setbac (0.0, 1.0, 0.0)
            if (itran.eq.1) then
	      xplt1 = 8.3
            else
	      xplt1 = 6.3
            endif
            if(ilegnd.eq.1) then
              xplt1 = 2.45
            endif
	    write (string, '(a,f3.1)') 'M = ',xmb
	    call text (xplt1, ypltm, 0.0, 1, string, 1)
   33     continue
	end if
        write(text1,'(a9,i2)')'Depth <= ',dep1
        write(text2,'(i2,a12,i2)')dep1,' < Depth <= ',dep2
        write(text3,'(i2,a12,i3)')dep2,' < Depth <= ',dep3
        write(text4,'(a8,i3)')'Depth > ',dep3
	if (ipdepth .eq. 1) then
	  xplt1 = 0.2
c  yplt1 = yplt1 - 0.2
          call setfor (0.0, 0.0, 0.0)
c  call setbac (0.0, 0.8, 0.0)
c  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
c  call setfor (0.0, 0.0, 0.0)
c  xplt1 = 0.5
c  call text (xplt1, yplt1, 0.0, 1, 'Default depth', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (240.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text1, 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (120.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text2, 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (0.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text3, 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (300.0, 0.9, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text4, 1)
	else if (ipdepth .eq. 4) then
	  xplt1 = 0.2
c  yplt1 = yplt1 - 0.2
	  call setfor (0.0, 0.0, 0.0)
c  call setbac (0.0, 0.8, 0.0)
c  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
c  call setfor (0.0, 0.0, 0.0)
c  xplt1 = 0.5
c  call text (xplt1, yplt1, 0.0, 1, 'Default depth', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (240.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text1, 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (120.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text2, 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (60.0, 0.8, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text3, 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (300.0, 1.0, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, text4, 1)
	else if (ipdepth .eq. 2) then
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (240.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, 'Depth <= 5', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (180.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, '5 < Depth <= 10', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (120.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, '10 < Depth <= 15', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (30.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, '15 < Depth <= 20', 1)
	  xplt1 = 0.2
	  yplt1 = yplt1 - 0.2
	  call setbac (0.0, 0.5, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, 'Depth > 20', 1)
	end if
	call setscl (xmin, xmax, ymin, ymax)
      else
	call setscl (0.0, xdim, 0.0, ydim)
	if (ipdepth .eq. 3) then
	  xplt1 = 0.2
	  yplt1 = -0.4
	  call setbac (240.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, 'Delta depth < -1', 1)
	  xplt1 = 0.2
	  yplt1 = -0.6
	  call setbac (180.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1,0.0,1,'-2 < Delta depth < -0.2', 
     +               1)
	  xplt1 = 0.2
	  yplt1 = -0.8
	  call setbac (120.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1,yplt1,0.0,1,'-0.2 < Delta depth < 0.2', 
     +               1)
	  xplt1 = 0.2
	  yplt1 = -1.0
	  call setbac (30.0, 0.4, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, '0.2 < Delta depth < 1', 
     +               1)
	  xplt1 = 0.2
	  yplt1 = -1.2
	  call setbac (0.0, 0.5, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, symthk, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, '1 < Delta depth', 1)
	else
	do 210  i = 1, nauth
	  call setbac (symhue(i), symlit(i), symsat(i))
	  xplt1 = 0.2
	  yplt1 = -0.2 - (i-1)*0.20
	  if (sym(i) .eq. 'NONE') then
	  else
	    call symbol (sym(i), xplt1, yplt1, symsiz, symthk, 1, 1)
	  end if
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.5
	  call text (xplt1, yplt1, 0.0, 1, auth(i), 1)
  210   continue
	end if
	call setscl (xmin, xmax, ymin, ymax)
      end if
      end if
      endif
c
c    Read in great circle paths and plot 
c
      if (ipaths .eq. 1) then
	open(23,file='paths.map')
	i=0
 235    read(23,*,end=232)xlat,xlon,xlat2,xlon2
	  i=i+1
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
	  call latlon2xydel (xlatc, xlonc, xlat2, xlon2,
     +                     xplt2, yplt2)
        call setfor (0.0, 0.0, 0.0)
	call line(xplt1,yplt1,xplt2,yplt2,0.01,0,0)
	  go to 235
 232    continue
	close (23)
      endif
c
c    Read in fault locations and plot
c
      if (iflt .eq. 1) then
        open(23,file='/usr/local/aeic/5.2-64/data/maps/faults.lin')
        read(23,'(a10)')frmat
 245    i=0
 246    read(23,'(2f12.6)',end=247)xlat,xlon
          if(xlat.eq.0.0.and.xlon.eq.0.0) go to 242
          i=i+1
          xlon = -xlon
cRAH      if (xlon.gt.0.0)xlon = -xlon
          call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
          ppltx(i) = xplt1
          pplty(i) = yplt1
          go to 246
 242    continue
        if(i.eq.0) go to 245
        npltp = i
        call setfor (40.0, 0.2, 1.0)
        call nplot(npltp,ppltx,pplty,0,0,.01,0,' ')
        go to 245
 247    continue
        close (23)
      endif
c
c    Read in pipeline locations and plot 
c
      if (ipipe .eq. 1) then
	open(23,file='/usr/local/aeic/5.2-64/data/maps/pipeline.lin')
	i=0
 225    read(23,'(f12.6,f13.6)',end=222)xlat,xlon
	  i=i+1
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
	  ppltx(i) = xplt1
	  pplty(i) = yplt1
	  go to 225
 222    continue
	npltp = i
	close (23)
        call setfor (0.0, 0.0, 0.0)
	call nplot(npltp,ppltx,pplty,0,0,.01,0,' ')
        if (ipumps .eq. 1) then
  	  open(23,file='/usr/local/aeic/5.2-64/data/maps/pipepumps.dat')
 226      read(23,'(2f12.6,1x,a)',end=227)xlat,xlon,city
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
          call setfor (0.0, 0.0, 0.0)
          call chrsiz (0.06, 1.0, 0.0)
          call setbac (0.0, 0.8, 0.0)
	  call symbol ('hexagon', xplt1, yplt1, 2*symsiz, symthk, 0, 1)
          call setbac (0.0, 1.0, 0.0)
            call text (xplt1, yplt1, 0.0, 1, '  ' // city, 0)
	  go to 226
        endif
 227    continue
	close (23)
      endif
c
c    Read in city locations and plot 
c
      if (icities .eq. 1) then
	open(23,file='/usr/local/aeic/5.2-64/data/maps/cities.dat')
 224    read(23,'(2f12.6,1x,a)',end=223)xlat,xlon,city
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
          call setfor (0.0, 0.0, 0.0)
          call chrsiz (0.12, 1.0, 0.0)
          call setbac (0.0, 0.8, 0.0)
	  call symbol ('box', xplt1, yplt1, 2*symsiz, symthk, 0, 1)
          call setbac (0.0, 1.0, 0.0)
          if (icities .eq. 1) then
            call text (xplt1, yplt1, 0.0, 1, '  ' // city, 0)
          endif
	  go to 224
 223    continue
	close (23)
      endif
c
c    Read in station locations and plot 
c
      if (istaplt .ge. 1) then
        call dbgetntuples (idb, RELID_SITE, ntuples)
        call setfor (0.0, 0.0, 0.0)
        call chrsiz (0.08, 1.0, 0.0)
        do 75  i = 1, ntuples
	  call dbgetattrs (idb, RELID_SITE, i,
     +                   ATTRID_LON, xlon,
     +                   ATTRID_LAT, xlat,
     +                   ATTRID_STA, arg,
     +                   ATTRID_OFFDATE, offdate,
     +                   ATTRID_ONDATE, ondate,
     +			 0)
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
          call setbac (0.0, 0.9, 0.0)
          if(offdate.ne.-1) then
            if (istaplt.eq.1) go to 75
            call setbac(40.0,.6,1.0)
            print*,'Ondate = ',ondate,' Offdate = ',offdate
          endif
	  call symbol ('triangle', xplt1, yplt1, 2*symsiz, symthk, 0, 1)
          call setbac (0.0, 1.0, 0.0)
          if (istnam .eq. 1) then
            call text (xplt1, yplt1, 0.0, 1, '  ' // arg, 0)
          endif
   75   continue
      end if
      if (jorid .ge. 0) then
        do 400  i = 1, nevs
	  call evget (ievs(i), EV_NHYPOS, nhypos,
     +                       EV_PREFHYPO, ipref,
     +                       EV_EVID, ievid,
     +			 0)
	  if (ipref .lt. 1) ipref = 1
	  call evget (ievs(i), 
     +                       EV_HYPO_ORID, ipref, iorid,
     +                       EV_HYPO_NSTAS, ipref, nstas,
     +                       0)
	  if (iorid .eq. jorid) go to 410
  400   continue
  410   continue
	if (i .gt. nevs) then
	  write (6, '(a)') 'Orid not found.'
	  stop
	end if
	do 475  j = 1, nstas
	  call evget (ievs(i), 
     +                       EV_HYPO_STA_STA, ipref, j, arg,
     +                       EV_HYPO_STA_LAT, ipref, j, slat,
     +                       EV_HYPO_STA_LON, ipref, j, slon,
     +                       EV_HYPO_STA_NASSOCS, ipref, j, nassocs,
     +                       0)
	  jflg = 0
          xlat = slat
          xlon = slon
	  do 476  k = 1, nassocs
	    call evget (ievs(i), 
     +              EV_HYPO_STA_ASSOC_ASSOCFLAG, ipref, j, k, iflg,
     +              0)
	    jflg = jflg + iflg
  476     continue
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
	  if (jflg .eq. 0) then
	    call setbac (0.0, 0.3, 0.0)
            call chrsiz (0.04, 1.0, 0.0)
	    call symbol ('cross', xplt1, yplt1, 0.8*symsiz, symthk, 0, 1)
	    call setbac (0.0, 1.0, 0.0)
 	    call text (xplt1, yplt1, 0.0, 1, '  ' // arg, 0)
	  else
	    call setbac (0.0, 0.0, 0.0)
            call chrsiz (0.06, 1.0, 0.0)
	    call symbol ('triangle', xplt1, yplt1, symsiz, symthk, 0, 1)
 	    call text (xplt1, yplt1, 0.0, 1, '  ' // arg, 0)
	    call setbac (0.0, 1.0, 0.0)
	  end if
  475   continue
      end if
c
c    Read in hypocentral data and plot it
c
      call setfor (0.0, 0.0, 0.0)
      if (jorid .lt. 0) then
      do 500  i = 1, nevs
	call evget (ievs(i), EV_NHYPOS, nhypos,
     +                       EV_PREFHYPO, ipref,
     +                       EV_EVID, ievid,
     +			 0)
        ipde = 0
        xmb = -999.0
        do 555  j = 1, nhypos
          call evget (ievs(i), EV_HYPO_AUTH, j, arg,
     +                       EV_HYPO_MB, j, ymb,
     +                       EV_HYPO_ML, j, yml,
     +                       EV_HYPO_MS, j, yms,
     +                0)
          if (yml .gt. -999.0) then
             xmb = yml
          elseif (yms .gt. -999.0) then
             xmb = yms
          elseif (ymb .gt. -999.0) then
             xmb = ymb
          endif
          if (arg .eq. 'PDE') ipde = 1
  555   continue
	iplt = 1
	if (nauth .eq. 0) then
	  i1 = ipref
	  i2 = ipref
	else
	  i1 = 1
	  i2 = nhypos
	end if
	if (nauth .gt. 1) then
	  do 491  l = 1, nauth
            do 492 k = 1, nhypos
	      call evget (ievs(i), EV_HYPO_AUTH, k, arg,
     +                       0)
              if (arg .eq. auth(l)) go to 491
  492       continue
            go to 500
  491     continue
	end if
	if (nauth .gt. 0) then
	  deptho = 0.0
	  depthc = 0.0
	  do 490 k = i1, i2
	    call evget (ievs(i), EV_HYPO_LAT, k, elat,
     +                       EV_HYPO_LON, k, elon,
     +                       EV_HYPO_DEPTH, k, depth,
     +                       EV_HYPO_AUTH, k, arg,
     +                       EV_HYPO_MB, k, ymb,
     +                       0)
            if (arg .eq. auth(1)) deptho = depth
            if (arg .eq. auth(2)) depthc = depth
  490     continue
	  dep = depthc-deptho
	end if
 	do 505  k = i1, i2
	call evget (ievs(i), EV_HYPO_LAT, k, elat,
     +                       EV_HYPO_LON, k, elon,
     +                       EV_HYPO_DEPTH, k, depth,
     +                       EV_HYPO_AUTH, k, arg,
     +                       EV_HYPO_ORID, k, iorid,
     +                       EV_HYPO_MB, k, ymb,
     +                       EV_HYPO_ML, k, yml,
     +                       EV_HYPO_MS, k, yms,
     +                       EV_HYPO_NSTAS, k, nstas,
     +                       0)
          if (yml .gt. -999.0) then
             xmb = yml
          elseif (yms .gt. -999.0) then
             xmb = yms
          elseif (ymb .gt. -999.0) then
             xmb = ymb
          endif
c       xmb = nstas
	xlat = elat
	xlon = elon
	if (nauth .eq. 0) then
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
          symbl = 'hexagon'
	  if (ipdepth .eq. 1) then
c    if (depth .eq. 33.0d0) then
c           if(icol.eq.1) then
c      call setbac (0.0, 0.8, 0.0)
c           else
c      call setfor (0.0, 0.8, 0.0)
c           endif
	    if (depth .le. dep1) then
            if(icol.eq.1) then
	      call setbac (240.0, 0.4, 1.0)
            else
	      call setfor (240.0, 0.4, 1.0)
            endif
	    else if (depth .le. dep2) then
            if(icol.eq.1) then
	      call setbac (120.0, 0.4, 1.0)
            else
	      call setfor (120.0, 0.4, 1.0)
            endif
	    else if (depth .le. dep3) then
            if(icol.eq.1) then
	      call setbac (0.0, 0.4, 1.0)
            else
	      call setfor (0.0, 0.4, 1.0)
            endif
	    else
            if(icol.eq.1) then
	      call setbac (300.0, 0.9, 1.0)
            else
	      call setfor (300.0, 0.9, 1.0)
            endif
	    end if
	  else if (ipdepth .eq. 4) then
c    if (depth .eq. 33.0d0) then
c           if(icol.eq.1) then
c      call setbac (0.0, 0.8, 0.0)
c           else
c      call setfor (0.0, 0.8, 0.0)
c           endif
	    if (depth .le. dep1) then
            if(icol.eq.1) then
	      call setbac (240.0, 0.4, 1.0)
            else
	      call setfor (240.0, 0.4, 1.0)
            endif
	    else if (depth .le. dep2) then
            if(icol.eq.1) then
	      call setbac (120.0, 0.4, 1.0)
            else
	      call setfor (120.0, 0.4, 1.0)
            endif
	    else if (depth .le. dep3) then
            if(icol.eq.1) then
	      call setbac (60.0, 0.8, 1.0)
            else
	      call setfor (60.0, 0.8, 1.0)
            endif
	    else
            if(icol.eq.1) then
	      call setbac (300.0, 1.0, 1.0)
            else
	      call setfor (300.0, 1.0, 1.0)
            endif
	    end if
	  else if (ipdepth .eq. 2) then
	    if (depth .le. 5.0d0) then
            if(icol.eq.1) then
	      call setbac (240.0, 0.4, 1.0)
            else
	      call setfor (240.0, 0.4, 1.0)
            endif
	    else if (depth .le. 10.0d0) then
            if(icol.eq.1) then
	      call setbac (180.0, 0.4, 1.0)
            else
	      call setfor (180.0, 0.4, 1.0)
            endif
	    else if (depth .le. 15.0d0) then
            if(icol.eq.1) then
	      call setbac (120.0, 0.4, 1.0)
            else
	      call setfor (120.0, 0.4, 1.0)
            endif
	    else if (depth .le. 20.0d0) then
            if(icol.eq.1) then
	      call setbac (30.0, 0.4, 1.0)
            else
	      call setfor (30.0, 0.4, 1.0)
            endif
	    else
            if(icol.eq.1) then
	      call setbac (0.0, 0.5, 1.0)
            else
	      call setfor (0.0, 0.5, 1.0)
            endif
	    end if
	  else if (ipdeplt .eq. 1) then
	    if (ipde .eq. 1) then
            if(icol.eq.1) then
	      call setbac (120.0, 0.5, 1.0)
            else
	      call setfor (120.0, 0.5, 1.0)
            endif
	    else
            if(icol.eq.1) then
	      call setbac (240.0, 0.5, 1.0)
            else
	      call setfor (240.0, 0.5, 1.0)
            endif
	    end if
	  else
            if(icol.eq.1) then
	    call setbac (240.0, 0.5, 1.0)
            else
	    call setfor (0.0, 0.5, 1.0)
            endif
	  end if
	  if (ipmag.eq.1) then
	    if (xmb .gt. -999.0) then
	      ss = symsiz*(2.0 + 2.0*(xmb-2.0)/4.0)
	      symbl = 'hexagon'
            else
	      ss = symsiz
	      symbl = 'box'
            end if
	  else
	    ss = symsiz
	    symbl = 'box'
	  end if
          if (icol.eq.1) then
	    call symbol (symbl, xplt1, yplt1, ss, symthk, 0, 1)
          else
crah        call circle (xplt1, yplt1, 3*ss, 90, 0, 0, 0.2, 0)
	    call symbol ('circle', xplt1, yplt1, ss, symthk, 0, 0)
          endif
	else
	  do 510  j = 1, nauth
	    if (arg .eq. auth(j)) go to 511
  510     continue
	  go to 500
  511     continue
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
	  if (ipdepth .eq. 3) then
	    if (dep .lt. -1.0) then
	      call setbac (240.0, 0.4, 1.0)
	    else if (dep .le. -0.2) then
	      call setbac (180.0, 0.4, 1.0)
	    else if (dep .le. 0.2) then
	      call setbac (120.0, 0.4, 1.0)
	    else if (dep .le. 1.0) then
	      call setbac (30.0, 0.4, 1.0)
	    else
	      call setbac (0.0, 0.5, 1.0)
	    end if
	  else
	    call setbac (symhue(j), symlit(j), symsat(j))
	  end if
          call symbol (sym(j), xplt1, yplt1, symsiz, symthk, 0, 1)
          call setbac (0.0, 1.0, 0.0)
	end if
	if (iplt .gt. 1) then
          call line (xplt1, yplt1, xplt2, yplt2, 0.01, 0, 0)
	else
	  if (iporid .eq. 1) then
	    write (title, '(i15)') iorid
	    do 513 l = 1, len(title)
	      if (title(l:l) .ne. ' ') go to 514
  513       continue
  514       l = l - 5
            call chrsiz (0.04, 1.0, 0.0)
 	    call text (xplt1, yplt1, 0.0, 1, title(l:len(title)), 
     +               0)
	  end if
	end if
	iplt = iplt + 1
	xplt2 = xplt1
	yplt2 = yplt1
  505 continue
  500 continue
      call setfor (0.0, 0.0, 0.0)
      call setbac (0.0, 1.0, 0.0)
      end if
c
      return
      end
      subroutine idevent (xlat0, xlon0, x, y, nevs, ievs,
     +                    nauth, auth)
c
      real*4 xlat0, xlon0
      real*4  x, y
c     integer nevs
c     integer ievs(nevs)
      integer*8 nevs
      integer*8 ievs(nevs)
      integer nauth
      character*(*) auth(nauth)
c
#include "EV_f.i"
      real*8 elat, elon, time, depth, sec
      character*80 author, alg
c
      call xydel2latlon (xlat0, xlon0, x, y,
     +                         xlat2, xlon2)
      amin = 1.e30
      do 100  i = 1, nevs
	call evget (ievs(i), EV_NHYPOS, nhypos,
     +                       EV_PREFHYPO, ipref,
     +                       EV_EVID, ievid,
     +			 0)
	do 200  j = 1, nhypos
	  call evget (ievs(i), EV_HYPO_LAT, j, elat,
     +                       EV_HYPO_LON, j, elon,
     +                       EV_HYPO_AUTH, j, author,
     +                       0)
          xlat = elat
          xlon = elon
	  if (nauth .eq. 0) then
	  else
	    do 510  k = 1, nauth
	      if (author .eq. auth(k)) go to 511
  510       continue
	    go to 200
  511       continue
	  end if
          call latlon2xydel (xlat, xlon, xlat2, xlon2,
     +                         xdel, ydel)
	  dist = sqrt(xdel**2 + ydel**2)
	  if (dist .lt. amin) then
	    amin = dist
	    ihold = i
	    jhold = j
	  end if
  200   continue
  100 continue
      i = ihold
      j = jhold
      call evget (ievs(i), EV_HYPO_LAT, j, elat,
     +                       EV_HYPO_LON, j, elon,
     +                       EV_HYPO_DEPTH, j, depth,
     +                       EV_HYPO_TIME, j, time,
     +                       EV_HYPO_MB, j, xmb,
     +                       EV_HYPO_MS, j, xms,
     +                       EV_HYPO_ML, j, xml,
     +                       EV_HYPO_AUTH, j, author,
     +                       EV_HYPO_ORID, j, iorid,
     +                       0)
      call e2h(time, iyear, iday, ihour, imin, sec)
      write (6, '(a)') ' '
      write (6, '(a,a)')    'AUTH:   ',author(1:ilen(author))
      write (6, '(a,i8)')   'ORID:   ',iorid
      write (6, '(a,i4,i3,1x,i2.2,a,i2.2,a,f5.2)')   
     +                      'TIME:   ',iyear,iday,ihour,':',
     +                   imin,':',sec
      write (6, '(a,f9.4)') 'LAT:    ',elat
      write (6, '(a,f9.4)') 'LON:    ',elon
      write (6, '(a,f9.4)') 'DEPTH:  ',depth
      write (6, '(a,f9.4)') 'MB:     ',xmb
      write (6, '(a,f9.4)') 'MS:     ',xms
      write (6, '(a,f9.4)') 'ML:     ',xml
      xlat2 = elat
      xlon2 = elon
      call latlon2xydel (xlat0, xlon0, xlat2, xlon2,
     +                         x, y)
c    
      return
      end
      subroutine idstation (xlat0, xlon0, x, y, idb,
     +                    nauth, auth)
c
      real*4 xlat0, xlon0
      real*4  x, y
      integer idb
      integer nauth
      character*(*) auth(nauth)
c
#include "EV_f.i"
#include "dbl2.i"
      real*8 elat, elon
      character*80 sta, staname
      real*4 xlat4, xlon4, elev
c
      call xydel2latlon (xlat0, xlon0, x, y,
     +                         xlat2, xlon2)
      amin = 1.e30
      call dbgetntuples (idb, RELID_SITE, ntuples)
      do 100  i = 1, ntuples
	call dbgetattrs (idb, RELID_SITE, i,
     +                   ATTRID_LON, xlon4,
     +                   ATTRID_LAT, xlat4,
     +			 0)
        xlat = xlat4
        xlon = xlon4
        call latlon2xydel (xlat, xlon, xlat2, xlon2,
     +                         xdel, ydel)
        dist = sqrt(xdel**2 + ydel**2)
	if (dist .lt. amin) then
	  amin = dist
	  ihold = i
	end if
  100 continue
      i = ihold
      call dbgetattrs (idb, RELID_SITE, i,
     +                   ATTRID_STA, sta,
     +                   ATTRID_LON, xlon4,
     +                   ATTRID_LAT, xlat4,
     +                   ATTRID_ELEV, elev,
     +			 0)
      write (6, '(a)') ' '
      write (6, '(a,a)')    'STA:    ',sta(1:ilen(sta))
      write (6, '(a,f9.4)') 'LAT:    ',xlat4
      write (6, '(a,f9.4)') 'LON:    ',xlon4
      write (6, '(a,f9.4)') 'ELEV:   ',elev
      xlat2 = xlat4
      xlon2 = xlon4
      call latlon2xydel (xlat0, xlon0, xlat2, xlon2,
     +                         x, y)
c    
      return
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
c     subroutine setans
c     character*1 ans
c     common /setanss/ ans
c
c     ans = 'R'
c     return
c     end
