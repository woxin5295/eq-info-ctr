c dbfpplot
c Modifications by N.Ratchkovski, September 2003
c Added parameter file and one argument instead of interactive input from command line.
c
c     purpose:       plot earthquake ray polarities and fault planes on a lower hemisphere equal area projection.
c
c     input file:    a file of the type "*.pol", which is generated by the program "fpfit" (see "fpfit, fpplot, and fppage: fortran 
c                    computer programs for calculating and displaying earthquake fault-plane solutions,
c                    by p. reasenberg and d. oppenheimer, u.s. geological survey open-file rep. 85-???)
c
c     required routines: calcomp style plot routines plots, plot, newpen, symbol.
c
c     departures from fortran-77 standard:
c                    1. keyword "readonly" open statement
c                    2. embedded comments prefaced with an exclamation mark (!) following variable declarations
c                    3. calls to vax system routines "sys$bintim", "sys$schdwk", "sys$hiber"  (subroutine delay)
c
c      output:       graphic output only
c
c      authors:      paul reasenberg and david oppenheimer, u.s.g.s. in menlo park.  some of the routines
c                    were adopted from code written by john lahr, bruce julian, and fred klein.
c
      include "db.i"
      
      character*80      aask      ! character function to get character input
      character*132     aline     ! input line with alternate solutions
      character*8       atime     ! time from system
      character*7       dnstrng
      real              ain       ! ray angle of incidence
      real              azm       ! ray azimuth
      real              cx1       ! x position of large circle center
      real              cx2       ! x position of small circle center
      real              cy1       ! y position of large circle center
      real              cy2       ! y position of small circle center
      real              da        ! dip angle
      real              dan(11)   ! dip angle of nearby solutions
      real              dd        ! dip direction
      real              ddn(11)   ! dip direction of nearby solutions
      character*8       disc      ! flag: if "*" then first motion discrepant with solution
      real              dist      ! epicentral distance
      logical           doplt     ! .true. to plot this symbol
      character*132     event     ! hypo71 summary card
      character*12      extname   ! station name with disc tacked on
      character*80      filnam    ! file name of data
      logical           first     ! flag: true prior to plotting discrepant first motion
      character*1       fm        ! first motion prmk(3:3)
      character*50      fmt       ! format for reading ray file marginal solutions
      integer  		getenv_s  ! gets environment variable
      character*20      head      ! event header- one per event, even if multiple solutions
      real              hite1     ! height of P,T symbol in large circle
      real              hite2     ! height of P,T symbol in small circle, dscrpncy rprt
      real              hite3     ! height of title, extended summary card, first motion legend
      integer           i         ! loop index over number of lines of nearby solutions
      integer           ios       ! i/o status descriptor
      integer           ipt(11)   ! 0/1/2 plot p and t/ plot p/ plot t
      character*132     line      ! scratch variable for plot output
      character*4       name      ! station name
      integer           n         ! loop index over number of nearby solutions
      integer           ndisw     ! number of discrepant readings added to list
      integer           nevst     ! event number of first event to plot
      integer           nline     ! number of lines of nearby solutions
      integer           nread     ! number of nearby solutions per line
      integer           nstar     ! number of solutions with fit within  5% of best solution
      real              pi        ! pi
      logical           pltpl     ! if .true. plot nearby nodal planes
      character*4       prmk      ! first motion description (eg. ipu0)
      character*132     ps_file   ! filename for post script file
      real              rad       ! pi/180
      real              rmax1     ! radius of large circle
      real              rmax2     ! radius of small circle
      real              sa        ! rake
      real              san(11)   ! rake of nearby solutions
      integer  		status    ! return check for subroutine getenv_s
      character*1       sym       ! first motion direction
      character*50      symfil    ! symbol table file
      character*80      title     ! data set descriptor
      logical           torf      ! logical function to get y or n answer
      real              unam(16)  ! 1 or 2 for do or don't plot station names
      logical           uniq    ! if true, then only consider alternate 
c                                   solutions that have unique discrepancy pattern
      real              upen(16)  ! symbol pen size
      real              uplt(16)  ! for wt > 0, sets largest wt to exclude,
c                                   for wt = 0, 1 or 2 for plot or no-plot
      character*1       uq(11)    ! equals * for unique solution
      real              uscal(8)  ! 1 or 2 for do or don't scale by weight
      real              usiz(16)  ! symbol size (inches)
      real              usym(16)  ! symbol type 
      real              uspen     ! symbol pen size
      real              ussiz     ! symbol size (inches) to plot
      real              ussym     ! symbol type to plot
      real*8            vd        ! view direction
      real              wt        ! weight assigned to pick quality in program fpfit
      real              xpos1     ! leftmost x position of title, summary card, symbol legend
      real              xpos2     ! leftmost x position of discrepancy report
      real              ydiflg    ! large shift down between lines of text
      real              ydifsm    ! small shift down between lines of text
      real              ypos      ! y plot position 
      real              ypos1     ! y plot position of title
      real              ypos2     ! y plot position of symbol legend
      real              ypos3     ! y plot position of top of discrepancy report
c     Mitch added the below fixed length strings
c     fortran will pass string until null, if compiler does not know string length
c     in MAX OS X
      character*12	ch_symbol_table / 'symbol_table' /
      character*12	ch_event_number / 'event_number' /
      character*14	ch_view_direction / 'view_direction' /
      character*10	ch_all_planes / 'all_planes' /
      character*20	ch_equivalent_solutions / 'equivalent_solutions' /
      
      integer pf, iargc
      character*3 plapl, plesol 

c     parameter (cx1 = 3.0, cx2 = 7.25, cy1 = 3.0, cy2 = 1.25)
c     parameter (cx1 = 3.4, cx2 = 7.65, cy1 = 5.4, cy2 = 3.65)
      parameter (cx1 = 3.06, cx2 = 6.88, cy1 = 3.86, cy2 = 2.29)

c changed radius from 2.65 to 2.8.  jcl 11/28/93
c     parameter (hite1 = 0.2, hite2 = 0.07, hite3 = 0.1, rmax1 = 2.95)
c     parameter (hite1 = 0.31, hite2 = 0.11, hite3 = 0.15, rmax1 = 2.95)
      parameter (hite1 = 0.22, hite2 = 0.075, hite3 = 0.10, rmax1 = 2.8)
c     parameter (rmax2 = 1.25, xpos1 = 0.1, xpos2 = 6.5, ydifsm = .15)
c     parameter (rmax2 = 1.25, xpos1 = 0.5, xpos2 = 6.45, ydifsm = .15)
      parameter (rmax2 = 1.12, xpos1 = 0.35, xpos2 = 5.71, ydifsm = .17)
c     parameter (ypos1 = 8.2, ypos2 = 0.1, ypos3 = 6.0, ydiflg = .25)
c     parameter (ypos1 = 10.6, ypos2 = 2.5, ypos3 = 8.4, ydiflg = .25)
      parameter (ypos1 = 10.6, ypos3 = 9.4, ydiflg = .30)
      parameter (ypos2 = cy1 - rmax1 - ydiflg)
c
c begin code
c
      pi = atan(1.0)*4.0
      rad = pi/180.0
c

c********start Natasha's modifications****************
      call pfread('dbfpplot', pf, result)
      if (result .eq. 0) then
        call die (0,'Can not read parameter file')
      endif
      print*,'Opened parameter file.'

      print *, ' Welcome to dbfpplot!'
      
c10    filnam = aask('Enter the name of the .ray file', 
c     *  'fault.ray', -80)

c get name of the input *.ray file
      m = iargc()
      if (m .eq. 1) then
        call getarg(1, filnam)
      else
        call die(0,"Usage: dbfpplot [file].ray")
      endif
	call openit('read', filnam, 4, ierr)
	if(ierr .ne. 0) call die(0,"Input .ray file does not exist")
c
c12    symfil = aask('Enter name of symbol table file      ', 
c     &  '/usr/local/aeic/5.2-64/data/fpfit_lahr/symtable', -50)
c get name of symbol table file  
c     call pfget_string(pf,'symbol_table',symfil)
c     ch_symbol_table = 'symbol_table'
      print *, 'Mitch 1 '
      call pfget_string(pf, ch_symbol_table, symfil)
      print *, 'Mitch 2 '
     	call openit('read', symfil, 8, ierr)
	if(ierr .ne. 0) call die(0,"Symbol table file does not exist")
	call gettab (unam, upen, uplt, usiz, uscal, usym)
      close(8)
      print *, ' '
c
c read header card (first line in ray file)
c
      print *, 'Mitch 3 '
      iline = 1
      read (4, '(1x, a)', err = 2000) title
c     write(95, '(a)') title
      print *, 'Mitch 4 '
c
c25    print *, ' '
c      print *, ' Event number of first mechanism to plot'
c      nevst = iaskk('(answer 0 to start with first in file)', 0)

c     call pfget_int(pf,"event_number", nevst)
c     ch_event_number = 'event_number'
      call pfget_int(pf, ch_event_number, nevst)
c      if (result) nevst = 0
c      print*,'First event to plot ',nevst

      if (nevst .lt. 0) then
      call die(0,"Invalid number for first mechanism to plot")
      end if

      if (nevst .eq. 0) then
	print*,'Start with 1st event'
	goto 29
      else
27      iline = iline + 1
	read (4, 50, end = 285) event
c	write(95,'(i, 1x, a)') lentru(event), event
	if (dnstrng(event(3:7)) .ne. 'event') goto 27          
	read(event(16:20), '(i5)') nev
	if(nev .lt. nevst) then
	  goto 27
	else if(nev .gt. nevst) then
	  print *, 'found event number ', nev, ' which is after ', 
     1    nevst, ', so stop'
	else if(nev .eq. nevst) then
	  goto 29
	endif
	goto 29
285     print *, 'event ', nevst, ' not found.'
	stop
      end if
29    print *, ' '


c      print *, ' A side projection may optionally be plotted.'
c      print *, ' A horizontal view direction of 999. will give',
c     * ' a vertical view instead.'
c      vd = raskk('Horizontal direction of view for side projection',
c     & 999.)

c     call pfget_double(pf,"view_direction", vd, result)
c     ch_view_direction = 'view_direction'
      call pfget_double(pf, ch_view_direction, vd, result)
      if (result .eq. 0) vd = 999.

      print*,'Horizontal direction of view for side projection:',vd
      print*,''

c     call pfget_string( pf, "all_planes", plapl)
c     ch_all_planes = 'all_planes'
      call pfget_string( pf, ch_all_planes, plapl)
      if(plapl.eq.'no' .or. plapl.eq.'No') then
      print*,'Plot only best fitting solution'
      print*,''
        pltpl = .false.
      else
        print *, 'Plot all nearby planes'
      print*,''
      end if
c

c     call pfget_string( pf, "equivalent_solutions", plesol)
      call pfget_string( pf, ch_equivalent_solutions, plesol)
      if(plesol.eq.'no' .or. plesol.eq.'No') then
        uniq = .false.
      else
       print *, 'Plot all equivalent solutions'      
      print*,''
      end if

c      print *, ' If desired, only nearby solutions with just as '
c      print *, '  good a fit as the best solution will be plotted.'
c      uniq = torf('Plot only equivalent nearby solutions', .false.)
c
c get name of post script file
c
      status = getenv_s ('PS_FILE', ps_file)
      if(status .ne. 1) then
        ps_file = 'fppage.ps'
      endif
      call report_s ('PS_FILE', ps_file)
      call openit('write', ps_file(1:lentru(ps_file)), 88, ierr)
      if(ierr .ne. 0) then
        print *, 'Error opening ', ps_file
        stop
      endif

c
c initialize plot program
c
      call plots(0, 0, 0)
c     call scale (.9)
!      call setfon ('romsim')
c switch to courier font
      call newfont(0)
      call symb(0., 0., 0., 0., 0., 0., 0.)
      if (nevst .eq. 0) goto 49
      goto 51


c
c position file for next event 
c
49      iline = iline + 1
	read (4, 50, end = 1000, err = 2000) event
50      format (1x, a)
c	write(95,'(i, 1x, a)') lentru(event), event
	if(dnstrng(event(1:7)) .ne. 'c*event') goto 49
c
c start new frame
c
51      ndisw = 0
	first = .true.
 	call newpen (2)
c
c plot title xpos1 = 0.1; ypos1 = 8.2; ydiflg = 0.25
c
	ypos = ypos1

        call symbol (xpos1, ypos, hite3, (title), 0., lentru(title))
        ypos = ypos - ydiflg
c
c plot event # and date
c
          call jdate(irmo, irdy, iryr, ihr, imn, isec)
          write(line, 65) event(3:59), iryr, irmo, irdy, ihr, imn
65        format(1x, a, 1x,'plotted',1x, i2.2, '/', i2.2, '/',
     &           i2.2, ' - ', i2.2, ':', i2.2, ' ')
          call symbol (xpos1, ypos, hite3, line, 0., 
     &                 lentru(line))
          ypos = ypos - ydiflg

c
c read and write other records
c
651     continue
        iline = iline + 1
        read (4, 50, end = 1000, err = 2000) event
c       write(95, '(i2, 1x, a)') iline, event
	if (dnstrng(event(1:7)) .eq. 'c*event') goto 651
	if (dnstrng(event(1:2)) .eq. 'm*') goto 68
        if (dnstrng(event(1:2)) .eq. 'b*') goto 644
        line = event(3:90)
!        call text (xpos1, ypos, hite3, line, 0., lentru(line))
        call symbol (xpos1, ypos, hite3, line, 0., lentru(line))
        ypos = ypos - ydifsm
	if (dnstrng(event(1:2)) .eq. 's*') ypos = ypos - ydifsm/2.
c
        if((dnstrng(event(1:2)) .eq. 'c*') .or. 
     *     (dnstrng(event(1:2)) .eq. 's*')) goto 651
        if(dnstrng(event(1:2)) .eq. 'f*') then
          read (event, 63) dd1, da1, sa1
63        format (2x, f4.0, f3.0, f4.0)
c63       format (t81, f4.0, f3.0, f4.0)
c         call transp
c         print *, 'initial reading/ da1, dd1, sa1 ', da1, dd1, sa1
          ypos = ypos - ydifsm/2.
          goto 651                        
        endif
68      if(dnstrng(event(1:2)) .eq. 'm*') then
          read (event(3:80), 70, err = 2000) nstar, npline, fmt
70        format (2i6, 1x, a50) 
          ypos = ypos - ydifsm/2.
          goto 651
        endif

c
c plot out file names
c
644       ypos = ypos - ydifsm/2.
          write(line, 66) filnam(1:lentru(filnam)), 
     &    symfil(1:lentru(symfil))
66        format(a, ' | ', a)
!          call text (xpos1, ypos, hite3, line, 0., lentru(line))
          call symbol (xpos1, ypos, hite3, line, 0., lentru(line))
          ypos = ypos - ydiflg
          if(vd .ne. 999.) then
            write(line, 67) filnam, vd
67          format(a15, 'azimuth of horizontal view = ', f5.0)
!            call text (xpos1, ypos, hite3, line, 0., lentru(line))
            call symbol (xpos1, ypos, hite3, line, 0., 
     &                   lentru(line))
          endif
c         call transp
c
c write out small key to symbols
c
98765     continue
          ypos = ypos2
          call newpen (1)
          call symb ( usym(1), .65*hite2, xpos1, 
     &      ypos + hite2/4., 0., 0., 0.)
          call symb ( usym(9), .65*hite2, xpos1 + .14,
     &      ypos + hite2/4., 0., 0., 0.)
          line(1:11) = 'COMPRESSION'
          call newpen (2)
          call symbol (xpos1 + .3, ypos, hite2, line, 0., 11)
          ypos = ypos + .16
          call newpen (1)
          call symb ( usym(5), .65*hite2, xpos1, 
     &      ypos + hite2/4., 0., 0., 0.)
          call symb ( usym(13), .65*hite2, xpos1 + .14,
     &      ypos + hite2/4., 0., 0., 0.)
          line(1:11) = 'DILATATION '
          call newpen (2)
!          call text(xpos1 + .3, ypos, hite2, line, 0., 11)
          call symbol (xpos1 + .3, ypos, hite2, line, 0., 11)
c
c plot big & little stereo net perimeters
c

          call newpen (4)
          call strnet (cx1, cy1, rad, rmax1)
          call strnet (cx2, cy2, rad, rmax2)
c
c  plot "p" and "t" axes in big net
c
          name = '    '
          wt = 1.0
          call newpen (3)

c     subroutine tpplot (cx, cy, da1, dd1, dipmx, hite, pi, psym,
c    &     rad, rmax, sa1, tsym, vd)
c     subroutine tpplot2 (cx, cy, da1, dd1, dipmx, pi, pc,
c    &     rad, rmax, sa1, tc, vd, ain1, az1, syml1, ain2, az2, syml2,
c    &     prot, hite)

c         call tpplot (cx1, cy1, da1, dd1, 90., hite1, pi, 1., 
c    &      rad, rmax1, sa1, 2., vd)

          call tpplot2 (cx1, cy1, da1, dd1, 90., pi, 'p', 
     &    rad, rmax1, sa1, 't', vd, ain1, az1, syml1, ain2, az2, syml2,
     &    prot, hite1)
c
c plot the p and t axes in small net 
c
          call newpen (2)
c         call tpplot (cx2, cy2, da1, dd1, 90., 2.*.85*hite2, pi, 1., 
c    &      rad, rmax2, sa1, 2., vd)
 
          call tpplot2 (cx2, cy2, da1, dd1, 90., pi, 'p',
     &    rad, rmax2, sa1, 't', vd, ain1, az1, syml1, ain2, az2, syml2,
     &    prot, 2.*.85*hite2)
c
c
c compute auxiliary plane orientation
c
c         call transp
c         print *, 'compute auxiliary plane orientation'
          call auxpln (dd1, da1, sa1, dd2, da2, sa2)
c
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
          call newpen (3)
          call plotpl (cx1, cy1, da1, pi, rad, rmax1, dd1 - 90., 0.)
          call plotpl (cx1, cy1, da2, pi, rad, rmax1, dd2 - 90., 0.)
c
c plot nearby solutions in small net
c
        if (nstar .gt. 0) then
          call newpen (2)
          nline = (nstar - 1)/npline + 1
          do 90 i = 1, nline
71          iline = iline + 1
            read (4, 72) aline
72          format (a)
c            if (uniq) then
c            else
            if (aline(2:2) .eq. '#') goto 71
            if (i .lt. nline) then
              nread = npline
            else
              nread = mod(nstar-1, npline) + 1
            end if
c	     do 73 n = 1, nread
c		read (aline(2:132),fmt, err = 2000) uq(n), 
c     &            ddn(n),dan(n), san(n), ipt(n)
c73	     continue
            read (aline(2:132), fmt, err = 2000) 
     &       (uq(n), ddn(n), dan(n), san(n), ipt(n), 
     &        n = 1, nread )
            call ptnear2 ( cx1, cx2, cy1, cy2, dan, ddn, hite2, 
     &      .65*hite2, ipt, 
     &      nread, pi, pltpl, rad, rmax1, rmax2, san, uniq, uq, vd,
     *      42., 40., 0.)
90        continue
        end if
c
c read phase card
c
100     iline = iline + 1
        read (4, 72) aline
        if(aline(2:2) .eq. '#') goto 100
        if(dnstrng(aline(1:2)) .eq. 'c*') goto 100
        read (aline, 110, end = 1000, err = 2000) name, dist, 
     &  azm, ain, prmk, wt, disc
110     format (1x, a4, 11x, 3f6.1, 3x, a4, e12.2, 2x,
     &          a8)
c	print *, 'name = ', name
        if (name .ne. '    ') then
c
c report discrepant observations
c
            call newpen(2)
            if ( ((disc(1:1) .ne. ' ') .or. (wt .eq. 0)) .and.
     *           (ndisw .lt. 30)) then
              if (first) then
                call newpen(3)
                line = 'DISCREPANT OR'
                ypos = ypos3
                call symbol (xpos2, ypos, hite2, line, 0., 
     *          lentru(line))
                ypos = ypos - hite2*1.5
                line = 'EXCLUDED OBSERVATIONS'
                call symbol (xpos2, ypos, hite2, line, 0., 
     *          lentru(line))
                ypos = ypos - hite2*1.5
                line = 'stat  dist   azm  ain  prmk code'
                call symbol (xpos2, ypos, hite2, line, 0., 
     *          lentru(line))
                ypos = ypos - hite2*.1
                line = '--------------------------------'
                call symbol (xpos2, ypos-hite2*.7, hite2, line,0.,
     *          lentru(line))
                ypos = ypos - hite2*1.75 
                first = .false.
                call newpen(2)
              end if
              ndisw = ndisw + 1
              if (ndisw .gt. 20) then
                line = 'skip remainder of list'
                call symbol (xpos2, ypos, hite2, line, 0.,
     *          lentru(line))        
!               call symbol (xpos2, ypos, hite2, line, 0.,
!    *          lentru(line))        
                goto 121
              endif
              write (line, 120) name, dist, nint(azm), nint(ain), prmk,
     &        disc
120           format (a4, f5.1, 2i5, 1x, a4, 2x, a8)
!              call text (xpos2,        ypos, hite2, line(1:4), 
!     *        0., 4)
!              call text (xpos2 + .36,  ypos, hite2, line(5:19), 
!     *        0., 15)
!              call text (xpos2 + 1.40, ypos, hite2, line(20:25),
!     *        0., 6)
!              call text (xpos2 + 1.85, ypos, hite2, line(26:34),
!     *        0., 9)
c name
              call symbol (xpos2, ypos, hite2, line(1:4), 
     *        0., 4)
c dist, azm, ain
              call symbol (xpos2 + .36,  ypos, hite2, line(5:19), 
     *        0., 15)
c prmk
              call symbol (xpos2 + 1.40, ypos, hite2, line(20:25),
     *        0., 6)
c disc
              call symbol (xpos2 + 1.85, ypos, hite2, line(26:34),
     *        0., 9)
              ypos = ypos - hite2*1.5
            end if                                                     
c
c define symbol for first motion and change name to blank if not wanted
c
121         fm = prmk(3:3)

c	write(90,*)  'ain: ', ain, ' azm: ', azm
c	write(90,*)  'doplt: ', doplt, ' fm: ', fm
c	write(90,*)  'name: ', name, 'disc: ', disc
c	write(90,*)  'wt: ', wt, ' uspen: ', uspen
c	write(90,*)  'ussiz: ', ussiz, ' ussym: ', ussym
c	write(90,*)  'unam: ', (unam(kk),kk=1,16)
c	write(90,*)  'upen: ', (upen(kk),kk=1,16)
c	write(90,*)  'uplt: ', (uplt(kk),kk=1,16)
c	write(90,*)  'usiz: ', (usiz(kk),kk=1,16)
c	write(90,*)  'usym: ', (usym(kk),kk=1,16)
c	write(90,*)  'uscal: ', (uscal(kk),kk=1,8)

            call defsym (ain, doplt, fm, name, unam, upen, 
     &                   uplt, usiz, uscal, 
     &                   uspen, ussiz, ussym, usym, wt)
c
c plot first motion
c
            if (vd .ne. 999.) 
     &         call flippl( -1, ain, azm, vd, pi, rad)

            call newpen (int(uspen + .1))
            extname = name//disc(1:2)
c	write(90,*) 'extname: ', extname, '   ', 'doplt: ',doplt
            if (doplt) call pltsym (ain, azm, cx1, cy1, ussiz, 
     &      extname, pi, rad, rmax1, ussym, 0.)
            goto 100
        endif
c	print *, 'Click in graphics window to continue.'
c	call plot(0., 0., -998)
c	pause
        call plot (0., 0., -999)
        call plots(0, 0, 0)
c switch to courier font
      	call newfont(0)
        goto 49
c
c end of file
c
1000  continue
      close (1)
      stop
c
c read error
c
2000  print *, 'read error on line ', iline
      print *, aline
      stop
      end
