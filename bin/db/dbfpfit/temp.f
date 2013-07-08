c     purpose:       plot earthquake ray polarities and fault planes on a 
c                    lower or side hemisphere equal area projection.
c                    makes multiple plots per page.
c
c add side view and plot from .extsum file
c
c     input file:    a file of the type "*.ray" or ".extsum", which is 
c                    generated by the program "fpfit" (see "fpfit and 
c                    plotfm: fortran computer programs for calculating 
c                    and displaying earthquake fault-plane solutions, 
c                    by p. reasenberg and d. oppenheimer, u.s. geological 
c                    survey open-file rep. 85-???)
c
c     required routines: calcomp style plot routines plots, plot, newpen, 
c                    symbol.
c
c     departures from fortran-77 standard:
c                    1. keyword "readonly" in open statement
c                    2. embedded comments prefaced with an exclamation mark (!) following variable declarations
c                    3. calls to vax system routines "sys$bintim", "sys$schdwk", "sys$hiber"  (subroutine delay)
c
c      output:       graphic output only
c
c      authors:      paul reasenberg and david oppenheimer, u.s.g.s. in menlo park.  some of the routines
c                    were adopted from code written by john lahr, bruce julian, and fred klein.
c
c                    modified 3/20/87  j. c. lahr

      character*10      dnstrng
      character*50      aask       ! character function to ask question
      real              ain        ! ray angle of incidence
      character*132     aline      ! input line with alternate solutions
      real              amaxx      ! height of paper
      real              amaxy      ! width of paper
      logical           answer     ! answer to continue question
      real              azm        ! ray azimuth
      real              charht     ! character size - title
      real              charhtl    ! character size - subtitle
      real              cx         ! x position of stereo net center
      real              cy         ! y position of stereo net center
      real              cx2        ! x position of p/t stereo net center
      real              cy2        ! y position of p/t stereo net center
      real              da1        ! dip angle of principle plane
      real              da2        ! dip angle of auxilliary plane
      real              dd1        ! dip direction of principle plane
      real              dd2        ! dip direction of auxilliary plane
      real              dan(11)    ! dip angle of nearby solutions
      real              ddn(11)    ! dip direction of nearby solutions
      character*8       disc       ! flag: if "*" then first motion discrepant with solution
c                                    discrepant with solution
      real              dist       ! epicentral distance
      logical           doplt      ! .true. to plot this symbol
      character*132     event      ! fpfit solution record
      character*132     evsum      ! hypo summary record
      character*80      evsz       ! input record    
      character*12      extname    ! station name with disc tacked on
      character*50      filnam     ! file name of data
      character*50      filnamo    ! file name of output
      character*50      filfmt     ! file name of page format control file
      real              fit        ! misfit of this event
      character*1       fm         ! first motion prmk(3:3)
      character*50      fmt        ! format for reading nearby solutions
c                                    d=event date
      character*50      fmt3       ! format for reading christensen data
      real              hite       ! height of title characters
      integer           i          ! loop index over number of lines of 
c                                    nearby solutions
      integer           iline      ! input line number
      integer           inftyp     ! 1 for extended summary file, 
c                                    2 for full ray file
c                                    3  christensen
      integer           ios        ! io status descriptor
      integer           ipt(11)    ! 0/1/2 plot p and t/ plot p/ plot t
      character*40      line       ! scratch variable for plot output
      character*40      line1      ! scratch variable for plot output
      character*40      line2      ! scratch variable for plot output
      integer           mxevnt     ! maximum number of events per page
      integer           mxplot     ! maximum number of events to plot
      character*4       name       ! station name
      integer           nchar      ! number of characters to be plotted
      integer           ncol       ! number of columns of fp solutions
      integer           nepsph     ! pen size for spheres
      integer           neptit     ! pen size for title
      integer           neppctc    ! pen size for alternate p&t symbols
      integer           nev        ! event number
      integer		nfirst	   ! number of first motions used in solution
      integer           nlabel     ! current event label number
      integer           nline      ! number of lines of nearby solutions
      integer           npline     ! number of alternate axes per line
      integer           nread      ! number of nearby solutions on next line
      integer           nrow       ! number of rows of fp solutions
      integer           nstar      ! number of solutions with fit within
c                                    5% of best solution
      integer           num        ! number of events plotted in a frame
      integer           numtot     ! total number of events plotted
      real              parea      ! area covered by alternate p axes
      character*1       pc         ! character for p axis in alt. sphere
      real              pctcht     ! char height of alt. shpere char.
      real              pi         ! pi
      real              plotln     ! plot length in x direction
      logical           pltpol     ! flag: do (not) plot first motion data
      logical           pltpl      ! if .true. plot nearby nodal planes
      character*4       prmk       ! first motion description (eg. ipu0)
      real              prot       ! paper rotation: 0.00 --> +y up
c                                  ! paper rotation:-pi/2 --> -x up
      character*1       psym       ! symbol p or blank
      real              psymb      ! symbol to be used for p axes
      real              rad        ! pi/180
      real              rmax1      ! radius of stereo net
      real              sa1        ! rake of auxilliary plane
      real              sa2        ! rake of auxilliary plane
      real              san(11)    ! rake of nearby solutions
      real              sizbst     ! size of equivalent solution in p&t
      real              sizrst     ! size of rest of p&t symbols
      character*6       svfont     ! save current font
      real              spacep     ! space between beach ball pairs
      real              spacenp    ! space between beach ball non-pairs
      character*1       star       ! multiple indicator
      character*1       sym        ! first motion direction
      character*1       syml1      ! p or t
      character*1       syml2      ! p or t
      real              tarea      ! area covered by alternate t axes
      character*1       tc         ! character for t axis in alt. sphere
      character*132     tenline    ! line of output for fpplot2.out
      character*80      title      ! data set descriptor
      logical           torf       ! subroutine to get y or n answer
      character*1       tsym       ! symbol t
      real              tsymb      ! symbol to be used for t axes
      real              unam(16)   ! 1 or 2 for do or don't plot station names
      logical           uniq       ! if true, then only consider alternate 
      real              upen(16)   ! symbol pen size
      real              uplt(16)   ! for wt > 0, sets largest wt to exclude,
c                                    for wt = 0, 1 or 2 for plot or no-plot
      character*1       uq(11)     ! equals * for unique solution
      real              uscal(8)   ! 1 or 2 for do or don't scale by weight
      real              usiz(16)   ! symbol size (inches)
      real              usym(16)   ! symbol type 
      real              uspen      ! symbol pen size
      real              ussiz      ! symbol size (inches) to plot
      real              ussym      ! symbol type to plot
      real              vd         ! view direction az for horiz fm proj,
c                                    set to 999. for "vertical" view.
      real              wt         ! wt assigned to pick qual in pgm fpfit
      real              xpos       ! x plot position for title
      real              ypos       ! y plot position for title
      real              yskip      ! y space between adjacent fp plots
c
c begin code
      pi = atan(1.0)*4.0
      rad = pi/180.0
      tsym = 'T'
      psym = ' '
      prot = -pi/2.
      parea = 0.
      tarea = 0.
c
      print *, 'Welcome to fppage2!  this is the alaska version'
      print *, 'of fppage.'
      print *, ' '

42    print *, 'Is the input file an extended summary file (1)'
      print *, '                      a complete .ray file (2)'
      inftyp = iaskk('       or a file from christensen and ruff (3)',
     * 1)
      if ((inftyp .lt. 1) .or. (inftyp .gt. 3)) then
        print *, '***** please answer "1", "2", or "3"; try again *****'
        go to 42
      endif

10    filnam = aask('Name of input .ray, .extsum, or christensen file',
     * 'fault.extsum', -50)
      call openit('read', filnam, 3, ierr)
      if(ierr .ne. 0) go to 10

      pltpl = .false.
      if(inftyp .eq. 1) then
        pltpol = .false.
      else if(inftyp .eq. 2) then
        pltpol = torf('Do you want to plot first motion data', .false.)
        if (pltpol) then
          psym = 'P'          
        end if
        pltpl = torf('Do you want to plot nearby nodal planes', .false.)
      else if (inftyp .eq. 3) then
        pltpol = .false.
        print *, 'Read list is strike, dip, and rake'
        fmt3 = aask('Format', '(a80, f4.0, f3.0, f4.0)', -50)
      endif


11    print *, 'Name of output file with computed parameters'
      filnamo = aask('for each event', 'fppage2.out', -50)
      call openit('write', filnamo, 10, ierr)
      if(ierr .ne. 0) go to 11
      write(10, '(a)')' Table 1.  Hypocenter and focal mechanism parameters'
     *' mechanism parameters'
      write(10, '(2a,/)') ' fppage2 read focal parameters from:  ', 
     *filnam(1:lentru(filnam))
      if (inftyp .ne. 3) write(10,400)
400   format(1h ,'Event   Date    Time  Latitude, Longitude, ',
     *'Depth, Mag     Plane 1        Plane 2      P-axis',
     *'  T-axis  Misfit  Conf Area  Numb'/
     *          '  no.  yr mo da hr mn  deg min N  deg min W ',
     *'  km    ML  strk dip rake  strk dip rake    az pl',
     *'   az pl   score    P   T    data'/)
      nevst = 0
      nev = 0
      if(inftyp .ne. 3) then
30      nevst = iaskk('Event number of first mechanism to plot (0 for fi
     1rst in file)', 0)
        if (nevst .lt. 0) then
          print *, '***** invalid number; try again *****'
          goto 30
        else if (nevst .eq. 0) then
          goto 35
        else
31        iline = iline + 1
          read (3, '(1x, a)', end = 315) event
          if (dnstrng(event(3:7)) .ne. 'event') goto 31
          read(event(16:20), '(i5)') nev
          if(nev .lt. nevst) then
            goto 31
          else if(nev .gt. nevst) then
            print *, 'Found event number ', nev, ' which is after ', 
     1      nevst, ', so stop'
          else if(nev .eq. nevst) then
            goto 35
          endif
          goto 35
315       print *, 'event ', nevst, ' not found.'
          stop
        end if
      end if
35    mxplot = iaskk('Enter number of mechanisms to plot (0=all)',
     *         0)
      if (mxplot .lt. 0) then
        print *, '***** invalid number; try again *****'
        goto 35
      end if

d     print *, '/',psym,'/',tsym,'/ psym and tsym'
      print *, 'Focal mechanisms may be projected as if seen from the'
      print *, ' side.  The "back plane" projection is used.'
      print *, ' If the azimuth of view is set to 999. then the normal'
      print *, ' vertical view of the lower hemisphere is plotted.'
      vd = raskk ( 'Azimuth of view for projection', 999.)
c
50    print *, 'Name of file with format and character'    
      filfmt = aask('parameters', '/we/src/fpfit/fppage2.ctl',
     *-50)
      call openit('read', filfmt, 8, ierr)
      if(ierr .ne. 0) goto 50
      call gettab2 (amaxx, amaxy, rmax1, nepsph, ncol, nrow, 
     *       spacep, spacenp, spacev, charht, neptit,
     *  psymb, tsymb, sizbst, sizrst, pc, tc, pctcht, neppctc,
     *  unam, upen, uplt, usiz, uscal, usym)
      close(8)
      goto 52

504   print *, 'Error reading 10 variables from page format file'
      print *, filfmt
      print *, aline
      stop

52    charhtl = 0.8*charht

c check page format
      xskip = 2.*rmax1 + charht + 2.3*charhtl + .06*rmax1 + spacev
      xmarg = .45 + (amaxx - xskip*nrow + spacev)/2.
      if(spacep .eq. 0.) then
        print *, 'Fault plane solutions will be plotted'
        print *, 'Distributions of p and t axes will not be plotted'
        yskip = 2.*rmax1 + spacenp
      else
        print *, 'Fault plane solution will be plotted along with'
        print *, 'Distribution of p and t axes'
        yskip = 4.*rmax1 + spacep + spacenp
      endif      
      ymarg = 0.8 + (amaxy - yskip*ncol + spacenp)/2.
      print *, 'Computed top and side margins to be ', xmarg, ymarg
      if(ymarg .le. 0.) then 
        print *, 'Must make beach balls smaller'
        if(spacep .eq. 0.) then
          rmax1 = rmax1 + 2.*(-spacenp + ymarg)/(2.*ncol)
          print *, 'Radius adjusted to ', rmax1
        else
          rmax1 = rmax1 + 2.*(-spacenp + ymarg)/(4.*ncol)
          print *, 'Radius adjusted to ', rmax1
        endif
        if(rmax .lt. 0.) stop
        goto 52
      endif
      if(xmarg .le. 0.) then 
        print *, 'Must make beach balls smaller'
        rmax1 = rmax1 + 2.*(-spacev + xmarg)/(2.*nrow)
        print *, 'Radius adjusted to ', rmax1
        if(rmax .lt. 0.) stop
        goto 52
      endif
      answer = torf('Do you wish to continue', .true.)
      if (answer .eq. .false.) stop
      mxevnt = nrow*ncol
      nlabel = 0
      num = 0
      numtot = 0
      iline = 0
      title = ' '
c
c initialize plot program
c
      call plots (0., 0., 0)
c initialize symbol subroutine
      call symb( 30., 1., 1., 1., 0., 0., 0.)
c     call erase
c     call delay ('0000 00:00:01.00')
c     call plot (.1, .1, -3)
      if(nev .ne. 0) goto 70
c 
c test section
c	ussiz = 1.0
c        x = 3.
c        y = 3
c	prot = -pi/2.
c        call symbol (x - sin(prot)*ussiz/3.6 - cos(prot)*ussiz/3.58,
c     *   y + sin(prot)*ussiz/3.58 - cos(prot)*ussiz/3.6,
c     *   ussiz, 'P', -prot/rad, 1)
c        call symb ( 90., ussiz, x, y, 0., 0., prot/rad)
c	
c	x = x + 2.
c	prot = 0.
c        call symbol (x - sin(prot)*ussiz/3.6 -cos(prot)*ussiz/3.58,
c     *   y +sin(prot)*ussiz/3.58 - cos(prot)*ussiz/3.6,
c     *   ussiz, 'P', -prot/rad, 1)
c        call symb ( 90., ussiz, x, y, 0., 0., prot/rad)
c
c
c	call plot(x, y, 3)
c	call plot(x-2, y, 2)
c        call plot (0., 0., 999)
c	if(ussiz .eq. 1.0) stop
c end of test section

c
c position file for next event and get event number
c
605   if(inftyp .eq. 2) then
61      continue
        iline = iline + 1
        read (3, 62, end = 1000, err = 2000) event
62      format (1x, a)
        if(dnstrng(event(1:7)) .ne. 'c*event') goto 61
        read(event(16:20), '(i5)') nev
      endif
c
c read event 
c
70      if(inftyp .eq. 2) then
c         read summary

71        iline = iline + 1
          read (3, 80, end = 1000, err = 2000) evsum
80        format (1x, a132)
          if(dnstrng(evsum(1:2)) .ne. 's*') goto 71

81        iline = iline + 1
          read (3, 80, end = 1000, err = 2000) event
          if(dnstrng(event(1:2)) .ne. 'f*') goto 81
          read (event, 815, err = 2000) dd1, da1, sa1, fit, nfirst, star
815       format (2x, f4.0, f3.0, f4.0, f6.3, 1x, i3, t55, a1)
          tenline = ' '
          if(evsum(11:11) .eq. ' ') evsum(11:11) = '0'
c          write(tenline(1:19), '(1x, i3, 10a)') nev,
c     *    ' ', evsum(3:4),   ' ', evsum(5:6),
c     *    ' ', evsum(7:8),   ' ', evsum(9:10),  ':', evsum(11:12)
c          write(tenline(20:38), '(12a)')
c     *    ' ', evsum(13:14), '.', evsum(15:16), ' ', evsum(17:18), 
c     *    ' ', evsum(20:21), '.', evsum(22:23), ' ', evsum(24:26)
c          write(tenline(39:55), '(12a)')
c     *    ' ', evsum(28:29), '.', evsum(30:31), ' ', evsum(32:34),
c     *    '.', evsum(35:36), ' ', evsum(37:37), '.', evsum(38:38)
          read(evsum,817) iyr, imo, idy, ihr, imn, latd, rlatm,
     *       lond, rlonm, depth, rmag
817       format(2x,5i2,4x,i2,1x,f4.2,i3,1x,f4.2,f5.2,f2.1)
          write(tenline(1:54),818) nev, iyr, imo, idy, ihr, imn,
     *    latd, rlatm, lond, rlonm, depth, rmag
818       format(2x,i3,i4,'/',i2,'/',i2,i3,':',i2,i5,f5.1,i6,f5.1,
     *    f7.1,f5.1)
          if (tenline(11:11) .eq. ' ') tenline(11:11) = '0'
          if (tenline(14:14) .eq. ' ') tenline(14:14) = '0'
          if (tenline(20:20) .eq. ' ') tenline(20:20) = '0'
        else if(inftyp .eq. 1) then
          iline = iline + 1
          read (3, 82, end = 1000, err = 2000) event
82        format (a132)
d         print *, event
          evsum(3:132) = event
          tenline = ' '
          if(evsum(11:11) .eq. ' ') evsum(11:11) = '0'
c          write(tenline(1:19), '(1x, i3, 10a)') nev,
c     *    ' ', evsum(3:4),   ' ', evsum(5:6),
c     *    ' ', evsum(7:8),   ' ', evsum(9:10),  ':', evsum(11:12)
c          write(tenline(20:38), '(12a)')
c     *    ' ', evsum(13:14), '.', evsum(15:16), ' ', evsum(17:18), 
c     *    ' ', evsum(20:21), '.', evsum(22:23), ' ', evsum(24:26)
c          write(tenline(39:55), '(12a)')
c     *    ' ', evsum(28:29), '.', evsum(30:31), ' ', evsum(32:34),
c     *    '.', evsum(35:36), ' ', evsum(37:37), '.', evsum(38:38)
          read(evsum,817) iyr, imo, idy, ihr, imn, latd, rlatm,
     *       lond, rlonm, depth, rmag
          write(tenline(1:54),818) nev, iyr, imo, idy, ihr, imn,
     *    latd, rlatm, lond, rlonm, depth, rmag
          if (tenline(11:11) .eq. ' ') tenline(11:11) = '0'
          if (tenline(14:14) .eq. ' ') tenline(14:14) = '0'
          if (tenline(20:20) .eq. ' ') tenline(20:20) = '0'
          read (event, 90, err = 2000) dd1, da1, sa1, fit, nfirst, star
90        format (t81, f4.0, f3.0, f4.0, f6.3, 1x, i3, t132, a1)
        else
c inftyp = 3
          fit = 0.
          iline = iline + 1
	  nfirst = 0
          read (3, fmt3, end = 1000, err = 2000) evsum, dd1, da1, sa1
901       format (a)
d         print *, evsum
c add 90 degrees to get direction of dip from strike
          star = ' '
          write(tenline(1:57), '(1x, i3, (a))') nev, evsum(1:52)
          dd1 = dd1 + 90.
        endif 
        if (star .eq. ' ') nlabel = nlabel + 1
          num = num + 1
          numtot = numtot + 1
c
c stop if more than mxplot events 
c
	  if (mxplot .gt. 0 .and. numtot .gt. mxplot) then
            goto 1000
          end if
c
c end plot frame if more than mxevnt events 
c
          if (num .gt. mxevnt) then
            num = 1
            call plot (0., 0., -999)
c           call erase
c           call delay ('0000 00:00:01.00')
c           call plot (.1, .1, -3)
	  end if
c center of ball is at cx, cy
          cy = ymarg + rmax1 + yskip*float(jmod(num - 1, ncol))
          cx = xmarg + 2.*charht + rmax1 + xskip*float((num-1)/ncol)
c center of p/t ball is at cx2, cy2
          if (spacep .ne. 0) then
             cy2 = cy + 2.*rmax1 + spacep
             cx2 = cx
          endif
          xpos = cx - rmax1 - 0.06*rmax1 - .5*charhtl
c
c write heading
c
          call newpen (neptit)
c         call getfon (svfont)
c         call setfon ('helvet')
          if(inftyp .ne. 3) then
            write (line2, 91) evsum(32:34), evsum(35:36),
     1      evsum(37:37), evsum(38:38)
91          format ('Z=', a3, '.', a2, '   M=', a1, '.', a1)
            nchar2 = 16
c           ypos = cy - 0.5*nchar2*charht*12.1/14.
            ypos = cy - 0.5*nchar2*charht*0.59
            if(spacep .ne. 0) ypos = ypos + rmax1 + spacep*.5
c           call text (xpos, ypos, charhtl, line2,
c    1      90., nchar2)
            call symbol (xpos, ypos, charhtl, line2,
     1      90., nchar2)
            if(evsum(11:11) .eq. ' ') evsum(11:11) = '0'
            if(evsum(12:12) .eq. ' ') evsum(12:12) = '0'
            write (line1, 905) evsum(3:8), evsum(9:10), evsum(11:12), 
     1      nev, star
905         format(a6, ' ', a2, ':', a2, ' (', i4, ')', a)
            nchar1 = 20
c           ypos = cy - 0.5*nchar1*charht*12.1/14.
            ypos = cy - 0.5*nchar1*charht*0.59
            if(spacep .ne. 0) ypos = ypos + rmax1 + spacep*.5
c           call text (xpos - 1.8*charhtl, ypos, charht, line1, 
c    1      90., nchar1)
            call symbol (xpos - 1.4*charhtl, ypos, charht, line1, 
     1      90., nchar1)
          else
c inftyp = 3
            nchar = 26
c           ypos = cy - 0.5*nchar*charht*12.1/14.
            ypos = cy - 0.5*nchar*charht*0.59
c           call text (xpos, ypos, charhtl, evsum(20:45),
c    1      90., nchar)
            call symbol (xpos, ypos, charhtl, evsum(20:45),
     1      90., nchar)
            nchar = 18
c           call text (xpos - 1.7*charhtl, ypos, charht, evsum(1:18), 
c    1      90., nchar)
            call symbol (xpos - 1.3*charhtl, ypos, charht, evsum(1:18), 
     1      90., nchar)
          endif
c         call setfon (svfont)
c
c plot stereo net perimeters
c
          call newpen(nepsph)
          call strnt1 (cx, cy, rad, rmax1)
          if(spacep .ne. 0.) then
            call strnt1 (cx2, cy2, rad, rmax1)
          endif
          call auxpln (dd1, da1, sa1, dd2, da2, sa2)
          str1 = dd1 - 90.
          if(str1 .lt. 0.) str1 = str1 + 360.
          str2 = dd2 - 90.
          if(str2 .lt. 0.) str2 = str2 + 360.
          if (inftyp .eq. 3) then
            write(tenline(59:88), '(2f4.0, f6.0, 2x, 2f4.0, f6.0)') 
     *      str1, da1, sa1, str2, da2, sa2
          else
            write(tenline(55:84), '(2(i6,i4,i5))') 
     *      nint(str1), nint(da1), nint(sa1),
     *      nint(str2), nint(da2), nint(sa2)
          endif
c
c plot "p" and "t" axes 
c
d         call transp      
          call newpen(neppctc)
          call tpplot2 (cx, cy, da1, dd1, 90., 
     &    pi, 'p', rad, rmax1, sa1, 't', vd, ain1, az1, syml1, 
     &    ain2, az2, syml2, prot, pctcht)
          if(spacep .ne. 0.)
     &     call tpplot2 (cx2, cy2, da1, dd1, 90.,
     &     pi, pc, rad, rmax1, sa1, tc, vd, ain1, az1, syml1, 
     &     ain2, az2, syml2, prot, pctcht)
          call aintodp(az1, ain1, az1p, dip1)
          call aintodp(az2, ain2, az2p, dip2)
          if(dnstrng(syml1) .eq. 'p') then
            iaz1 = nint(az1p)
            idip1 = nint(dip1)
            iaz2 = nint(az2p)
            idip2 = nint(dip2)
          else
            iaz1 = nint(az2p)
            idip1 = nint(dip2)
            iaz2 = nint(az1p)
            idip2 = nint(dip1)
          endif
          if (inftyp .eq. 3) then
            write(tenline(89:114), '(4f5.0, f6.3)') 
     *      iaz1,idip1,iaz2,idip2, fit
          else
            write(tenline(86:109), '(2(i5,i3),f8.3)') 
     *      iaz1,idip1,iaz2,idip2, fit
          endif
c
c convert nodal planes to horizontal view looking at azimuth of vd degrees
c
          if (vd .ne. 999.) then
            call flippl ( +1, da1, dd1, vd, pi, rad)
            call flippl ( +1, da2, dd2, vd, pi, rad)
          endif
c
c plot nodal planes 
c
          call plotpl (cx, cy, da1, pi, rad, rmax1, dd1 - 90., prot)
          call plotpl (cx, cy, da2, pi, rad, rmax1, dd2 - 90., prot)
c
c go on back now if reading summary record file
c
        if (inftyp .eq. 1) then
          write(10, '(a)') tenline(1:121)
          go to 70
	else if (inftyp .eq. 3) then
          write(10, '(a)') tenline
          go to 70
        endif
c
c read number of nearby solutions and format
c
92      iline = iline + 1
        read (3, '(1x, a)') evsz
        if(dnstrng(evsz(1:2)) .ne. 'm*') goto 92
        read (evsz, 93) nstar, npline, fmt
93      format (2x, 2i6, 1x, a)
931     iline = iline + 1
        read (3, 80, end = 1000, err = 2000) event
        if(dnstrng(event(1:10)) .ne. 'c* p and t') goto 931
        read(event, '(30x, 2f10.3)') parea, tarea
        if (inftyp .eq. 3) then
          write(tenline(115:126), '(2f6.3)') 
     *    parea, tarea
          write(10, '(a)') tenline(1:126)
        else
          write(tenline(110:126), '(i5, i4, 4x, i4)') 
     *    nint(parea*100.), nint(tarea*100.), nfirst
          write(10, '(a)') tenline(1:126)
	endif
c
c find beginning of nearby solutions
94      iline = iline + 1
        read (3, '(1x, a)') line
        if(dnstrng(line(1:2)) .ne. 'b*') goto 94
c
c read and, if necessary, plot alternate p and t axes
c
        if (nstar .gt. 0) then
          nline = (nstar - 1)/npline + 1
          do 943 i = 1, nline
941         iline = iline + 1
            read (3, '(a)') aline
            if (aline(2:2) .eq. '#') goto 941
            if (i .lt. nline) then
              nread = npline
            else
              nread = mod(nstar-1, npline) + 1
            end if
            if (spacep .ne. 0.) then
              read (aline(2:132), fmt, err = 2000) (uq(n), ddn(n), 
     &        dan(n), san(n), ipt(n), n = 1, nread)
              call newpen(neppctc)
              call ptnear2 ( cx, cx2, cy, cy2, dan, ddn, sizbst,
     &        sizrst, ipt, 
     &        nread, pi, pltpl, rad, rmax1, rmax1, san, uniq, uq, vd,
     &        psymb, tsymb, prot)
            endif
943       continue
        end if
c
c read phase record
c
120     iline = iline + 1
        read (3, 130, end = 1000, err = 2000) name, dist, azm, ain,
     &  prmk, wt, disc
130     format (1x, a4, 11x, 3f6.1, 3x, a4, e12.2, 2x, a8)
        if (name .ne. '    ') then
          if(pltpol) then
c
c plot first motion
c
            fm = prmk(3:3)
9876        call defsym (ain, doplt, fm, name, unam, upen, uplt, usiz,
     &      uscal, uspen, ussiz, ussym, usym, wt)
            if (vd .ne. 999.) 
     &      call flippl ( -1, ain, azm, vd, pi, rad)
            extname = name//disc(1:2)
            call newpen (int(uspen + .1))
            if(doplt) call pltsym (ain, azm, cx, cy, ussiz, 
     &      extname, pi, rad, rmax1, ussym, prot)
          end if
          goto 120
        end if
      goto 605
c
c end of file
c
1000  call plot (0., 0., 999)
      if (inftyp .ne. 3) write(10,1010)
1010  format(1h ,'_______________________________________',
     *           '___________________________'//
     *           ' Strike (strk), dip, rake, azimuth (az),',
     *           ' and plunge (pl) in degrees.'/
     *           ' Misfit score:  weighted measure of the agreement',
     *           ' between predicted and observed 1st motions,',
     *           ' 0.000 = perfect agreement.'/
     *           ' Conf Area: :  percentage of focal sphere covered by ',
     *           '90-percent confidence region of P- and T-axes.'/
     *           ' Numb data:  number of 1st motions used in solution.')
      close (3)
	goto 2001
c
c  read error
c
2000  print *, ' read error on line', iline
2001  stop
      end
