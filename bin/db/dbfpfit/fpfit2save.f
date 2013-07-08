      program fpfit2
c
c
c     october 30, 1986
c
c     purpose: calculate double-couple fault plane solutions from p-wave first 
c              motions (see reasenberg, p.  and oppenheimer, d.,  fpfit, fpplot 
c              and fppage: fortran computer programs for calculating and 
c              displaying earthquake fault-plane solutions, u.s. geological 
c              survey open-file report 85-???).
c
c
c     input file:   1a.  a hypo71 listing file which is read on logical unit iunit (=2), (see lee and lahr, 1975, hypo71 
c                        (revised): a computer program for determining hypocenter, magnitude and first 
c                        motion pattern of local earthquakes, u. s. geological survey open-file rep. 75-311.).
c                or 1b.  a hypoinverse archive file which is read on logical unit iunit (=2), (see f. klein, 1985, users 
c                        guide to hypoinverse, a program for vax and professional 350 computers to solve for earthquake
c                        hypocenters, u. s. geological survey open-file rep. 85-515).
c
c                or 1c.  a hypoellipse archive file.
c
c                    2.  a parameter control file which is read on logical 
c                        unit cunit (=1)
c
c                    3.  a list of trial focal mechanisms
c
c     required routines: all routines are enclosed
c
c     departures from fortran-77 standard:
c                    1. keywords "readonly" and "carriagecontrol = list" in open statements
c                    2. embedded comments prefaced with an exclamation mark (!) following variable declarations
c
c      output:       1. an ascii file of hypo71 summary cards extended with fault plane solution parameters on logical unit sunit 
c                       (=4).  this file serves as input to programs "qplot" and "plotpt".
c                    2. an ascii file consisting, for each earthquake, of the hypo71 extended summary card, followed by
c                       neighboring solutions (within 90% confidence limits), followed by individual p-phase information,
c                       on logical unit punit (=3). this file serves as input to programs "plotfm" and "fppage".
c                    3. an optional ascii file of the minimized fit function about the best solution on logical unit funit (=9)
c                    4. an ascii file describing any errors in the control file, hypo71 file, presence of multiple mechanisms, 
c                       a summary of polarity discrepancies by station and reading quality, and the distribution of strike, dip, and
c                       rake uncertainties on logical unit eunit (=8)
c  
c      authors:      paul reasenberg and david oppenheimer, u.s.g.s. in menlo park.  some of the routines
c                    were adapted from code written by john lahr, bruce julian, and fred klein.
c                    mark matthews, stanford university, provided assistance in the error propagation analysis.
c
c                    this version was modified by j. c. lahr to read a list of
c                    trial focal mechanisms.
c
c
c
      character*4       dnstrng
      integer           cunit      ! logical unit # of input control file
      real              dpr        ! degrees per radian
      integer           eunit      ! logical unit # of output of error messages
      integer           funit      ! logical unit # of output of fit listing for all strikes, dips
      integer           iunit      ! logical unit # of hypo71 listing file (input file)
      integer           kunit      ! logical unit # for indices
      integer           munit      ! logical unit for events used in composite
      integer           mxdip      ! maximum # of dip increments permitted
      integer           mxntc      ! maximum # of t orientations for each p
      integer           mxqual     ! maximum # of qualities permitted
      integer           mxrake     ! maximum # of rake increments permitted
      integer           mxslns     ! maximum # of multiple solutions permitted
      integer           mxstat     ! maximum # of stations permitted
      integer           mxstrk     ! maximum # of strike increments permitted
      integer           npfmt      ! number of characters per eq in ray file
      integer           npline     ! number of solutions per line in ray file
      integer           punit      ! logical unit # of output of extended summary and ray parameters 
                                   ! for use in plotting focal mech. diagrams with plotfm
      real              rad              ! conversion from degrees to radians
      integer           sunit      ! logical unit # of output of extended summary cards
      integer           tunit      ! logical unit # for output of trial mechs
c
c     parameter (cunit = 1, eunit = 8, funit = 9, iunit = 2, mxdip = 23,
c    & mxqual = 8, mxrake = 23, mxslns = 20, mxstat = 5000, mxstrk = 23,
c
      parameter (cunit = 1, eunit = 8, funit = 9, iunit = 2, mxdip = 46,
     & mxqual = 8, mxrake = 46, mxslns = 20, mxstat = 5000, mxstrk = 46,
     & punit = 3, sunit = 4, tunit = 11, mxcase = 414, mxntc = 36, 
     & kunit = 12, npline = 10, npfmt = 13, munit=7)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (dpr = 180./pi)
c
      real              aerr        ! allowable difference between corresponding angles of complimentary planes
      real              afitf       ! use this for fit90 if this is .ge. 0.
      real              ain(mxstat) ! ray incidence angles in degrees 
      real              ainr        ! ain converted to radians
      character*20     	atime      ! time of day
      real              axaz(mxcase) ! p axis location - azimuth
      real              axdp(mxcase) ! p axis location - dip
      real              az(mxstat)  ! ray azimuth angles (corresponding to ain)
      real              avwt        ! mean observational weight of data used in solution
      real              azr         ! az converted to radians
      character*1       bdflag(mxstat) ! signals polarity discrepancy with best fit solution
      logical           best        ! flag: true=best solution for event
      real              bstbot      ! largest bot for ties
      real              botb(mxcase) ! bot is big if data is far from nodes
      real              bots(mxcase) ! 
      real              bot2(mxcase) ! used in search2 only
      character*80      chart(mxcase)  ! chart of reversed stations (up to 80)
      character*80      chartb(mxcase)  ! chart of reversed stations (up to 80)
c                                         for biggest fit of ith case
      character*80      charts(mxcase)  ! chart of reversed stations (up to 80)
c                                         for smallest fit of ith case
      character*80      chartz(mxcase)  ! chart of reversed stations (up to 80)
c                                         for marginal solutions
      character*80      chartbst    ! chart of reversed stations for best solution
      real              coef(6, mxstat) ! coeficients by which moment tensor multiplied to give p radiation pattern
      logical           compl       ! function
      logical           cmpsit      ! true for composite solutions
      character*50      cmpsum      ! file with eqs used in composite
      real              da2         ! dip angle of auxilliary solution
      character*18      dattim      ! date/time of run
      real              dd2         ! dip direction of auxilliary solution
      character*10      date(mxstat)                    ! arrival time date yrmodyhrmn
      real              ddelc       ! fault dip increment in degrees for coarse search
      real              ddelf       ! fault dip increment in degrees for fine search
      real              del(mxdip)  ! fault dip angle in degrees
      real              delc(mxdip) ! fault dip angle in degrees for coarse search
      real              del0c       ! initial fault dip angle in degrees for coarse search
      real              del0f       ! initial fault dip angle in degrees for fine search
      real              del1f       ! terminating fault dip angle in degrees for fine search
      real              dfitc       ! increment to coarse fit function
      real              dip         ! dip angle of best solution
      real              dipp(mxcase) ! dip of nodal plane
      real              dippb(mxcase) ! dip of nodal plane for biggest
c                                       fit with ith p axis
      real              dipps(mxcase) ! dip of nodal plane for smallest
c                                       fit with ith p axis
      real              dippz(mxcase) ! dip of nodal plane for marginal
c                                       solution
      real              dist(mxstat)                    ! epicentral distance
      real              distmx      ! maximum permitted epicentral distance 
      real              dlamc       ! fault rake increment in degrees for coarse search
      real              dlamf       ! fault rake increment in degrees for fine search
      real              dpdir(mxcase) ! down dip azimuth
      real              dpdirb(mxcase) ! down dip azimuth for biggest 
c                                        fit for ith p axis
      real              dpdirs(mxcase) ! down dip azimuth for smallest
c                                        fit for ith p axis
      real              dpdirz(mxcase) ! down dip azimuth for marginal
c                                        solution
      real              dpdsv(mxntc, mxcase) ! p solution associated
c                                              with this axis (dip dr)
      real              dphic       ! fault strike increment in degrees for coarse search
      real              dphif       ! fault strike increment in degrees for fine search
      real              dppsv(mxntc, mxcase) ! p solution associated
c                                              with this axis (dip)
      real              erate(mxqual) ! assumed weighted error rates for each data class
      character*80      event       ! summary card
      character*52      evfit       ! dummy character string to hold fit values on output
      logical           first       ! flag: true=first time into subroutine search
      real              fit90       ! 90 % confidence limit of fit in fine search
      real              fitb(mxcase) ! solution fit; weighted measure 
c                                      of agreement between obs, pred 
c                                      polarities, biggest for ith axis
      real              fits(mxcase) ! solution fit; weighted measure 
c                                      of agreement between obs, pred 
c                                      polarities, smallest for ith axis
      real              fitz(mxcase) ! fit for nstar marginal solutions
      real              fitlim      ! upper bound on "good" solutions; 5% above fitmin
      real              fitmin      ! fit of best solution corresponding to fit(j1, n1, m1)
      real              fmagmn      ! minimum permitted magnitude
      character*50      fmt         ! format for reading trial solutions
      logical           fpinp       ! function
      character*3       ftqual      ! solution quality: concatenated fit quality, range quality
      real              gfit(mxdip*mxstrk*mxrake)       ! fits of "good" solutions found in coarse search
      integer           i           ! loop index
      integer           iamwt       ! code specifying type of amplitude weighting to use
      integer           id          ! loop index over ndst
      integer           idip        ! dip angle of nearby solutions
      integer           idip1       ! dip angle of best fit
      integer           idipdr      ! dip direction of nearby solutions
      integer           idst(mxslns,3)                  ! indices of distinct solutions found by hhog
      integer           idpdr1      ! dip direction of best fit
      integer           idrng       ! dip range variation for solutions with fit<fitlim
      integer           ierr        ! i/o error code on open
      integer           ievp        ! number of events processed
      integer           ievr        ! number of events read
      integer           ifit(mxstrk)                    ! integer conversion of fit*100 for printer output
      integer           ijeff       ! flag: 1(0)=do (not) use phase data weighted out by jeffrey's weighting
      integer           ind(mxstat) ! pointer array to sorted order
      integer           indx        ! bin index into ndrng,nsrng,nrrng,nfit
      integer           iprnt       ! flag: 1(0)=do (not) print out fit parameters
      integer           ipt(mxcase) ! 0/1/2 ith case axis is within limit for
c                                     p/t/both 
      integer           ipwt        ! weight assigned to p arrival
      integer           ires        ! flag: -1 = no coarse search limits set
                                    ! 0(1)=(un)restricted by coarse search limits
      integer           irrng       ! rake range variation for solutions with fit<fitlim
      integer           islip       ! rake of nearby solutions
      integer           islip1      ! rake of best fit
      integer           isrng       ! strike range variation for solutions with fit<fitlim
      integer           j           ! loop index over dip
      integer           j1          ! dip index of best solution
      integer           jjwt(mxstat) ! index for data weight 
      integer           jmax        ! largest dip index of solution with fit <=fitlim about (j1, n1, m1)
      integer           jmin        ! smallest dip index of solution with fit <=fitlim about (j1, n1, m1)
      integer           k           ! loop index
      character*4       kilsta(mxstat)                  ! ignored station names
      integer           l           ! loop index over moment tensor
      character*131     line        ! output string of nearby solutions orientations
      logical           lstall      ! if true then list indices of good solutions
      integer           m           ! loop index over rake
      integer           maxobs      ! maximum number of observations rquired
      integer           m1          ! rake index of best solution
      integer           minobs      ! minimum number of observations required 
      integer           mmax        ! largest rake index of solution with fit <=fitlim about (j1, n1, m1)
      integer           mmin        ! smallest rake index of solution with fit <=fitlim about (j1, n1, m1)
      integer           n           ! loop index of strike
      integer           n1          ! strike index of best solution
      integer           ncht        ! number of unique patterns of reversed stations
      integer           ndelc       ! number of fault dip increments for coarse search
      integer           ndelf       ! number of fault dip increments for fine search
      integer           ndlfdf      ! default number of fault dip increments for fine search
      integer           ndrng(mxdip) ! number of dip solution ranges binned into ddelf degree increments
      integer           ndst        ! number of distinct solutions found by hhog
      integer           nev         ! number of events to process
      integer           nfit(20)    ! number of solutions binned into .025 fit increments
      integer           ng          ! number of "good" solutions found in coarse search
      integer           nkil        ! number of ignored stations
      integer           nlamc       ! number of fault rake increments for coarse search
      integer           nlamf       ! number of fault rake increments for fine search
      integer           nlmfdf      ! default number of fault rake increments for fine search
      integer           nmax        ! largest strike index of solution with fit <=fitlim about (j1, n1, m1)
      integer           nmin        ! smallest strike index of solution with fit <=fitlim about (j1, n1, m1)
      integer           npax        ! number of cells covered by p axes
      integer           nphic       ! number of fault strike increments for coarse search
      integer           nphif       ! number of fault strike increments for fine search
      integer           nphfdf      ! default number of fault strike increments for fine search
      integer           nr          ! -1=eof, 0=skip event, nr>0 => number of stations 
      integer           nrwt        ! number used in fm
      integer           nrev        ! number of reversed stations
      integer           nrrng(mxrake) ! number of rake solution ranges binned into dlamf degree increments
      integer           nsol          ! number of planes stored in array solns
      integer           nsrng(mxstrk) ! number of strike solution ranges binned into dphif degree increments
      integer           nstar         ! number of solutions having fit within 5% of fitmin
      integer           nstat         ! total # of stations reporting for entire data set
      integer           ntax          ! number of cells covered by t axes
      real              ppct          ! percent of cells with p axes
      real              phi(mxstrk)   ! fault strike angle in degrees
      real              phic(mxstrk)  ! fault strike angle in degrees for coarse search
      real              phi0c       ! initial fault strike angle in degrees for coarse search
      real              phi0f       ! initial fault strike angle in degrees for fine search
      real              phi1f       ! terminating fault strike angle in degrees for fine search
      real              plusert     ! error rate for + or - readings
      real              pobs(mxstat) ! observed first motion polarities; .5=compression, -.5=dilatation
      real              prad        ! radiation amplitude corresponding ain, az.  
                                    ! (dilatation) -1.<prad<+1.(compression)
      real              prcntx      ! % of stations that are machine picked
      real              presmx      ! maximum permitted p-residual
      character*4       prmk(mxstat)     ! first motion description (eg. ipu0)
      integer           qcnt(mxqual,2)   ! indx 1=# of dscrpnt plrties for qlity, indx 2=# of observations
      real              qcntwt(mxqual,2) ! index 1=weighted # dscrpnt polrities for quality, index 2=sum of weights
      real              rakk(mxcase)     ! rake (motion on plane)
      real              rakkb(mxcase)     ! rake (motion on plane) for
c                                           biggest fit with ith p axis
      real              rakks(mxcase)     ! rake (motion on plane)
c                                           smallest fit with ith p axis
      real              rakkz(mxcase)     ! rake (motion on plane)
c                                           for marginal solution
      real              rkksv(mxntc, mxcase) ! p solution associated
c                                              with this axis (rake)
      character*4       revsta(mxstat)   ! reversed station names
      character*80      root             ! root name for output files
      real              sa2              ! slip angle of auxilliary solution
      logical           same             ! true if fine search = coarse
      integer           scnt(mxstat,2)   ! index 1=# of dscrpnt polarities for stat, index 2=# of observations
      real              scntwt(mxstat,2) ! index 1=weighted # dscrpnt polarities for stat, index 2=sum of weights
      character*1       sflag            ! flag indicating secondary solutions
      real              sigmaf           ! calculated standard deviation of fit based on data errors
      real              slip             ! slip angle of best solution
      real              smnlow(mxcase)   ! lower limit of sec for 2nd stage search
      real              smnhi(mxcase)    ! upper limit of sec for 2nd stage search
      real              smxlow(mxcase)   ! lower limit of sec for 2nd stage search
      real              smxhi(mxcase)    ! upper limit of sec for 2nd stage search
      real              solns(mxslns,3)  ! array of final solutions used to check for redundancy
      character*4       stat(mxstat)     ! names of all stations reporting
      real              stdr             ! station distibution ratio 
      character*4       stn(mxstat)  ! station names per event
      real              stke(mxcase)   ! strike of nodal plane
      character*13      string       ! scratch variable
      real              strike       ! strike of best solution
      real              sumwt        ! sum of observed first motion weights
      character*80      temp         ! scratch variable for reformatting event card
      character*48      title        ! data set descriptor
      logical           torf         ! logical function to get yes or no answer
      real              tm(6, mxcase) ! moment tensor in upper triangular symetric storage mode
      real              tmsv(6, mxntc, mxcase) ! moment tensor for 
c                                                first coarse search
      real              tmbst(6)     ! moment tensor for best solution
      real              tpct         ! percent of cells with t axes
      real              u(3)         ! cartesian p-wave direction vector (positive up, south, east)
      logical           uniq         ! if true, then identify unique
c                                      reversal patterns.
      character*80      ucht(mxcase) ! unique distribution of reversed stations
      real              weight(mxqual) ! weights associated with qualities
      real              wterat(mxqual) ! error rate for alternate weighting
      character*4       wtkey          ! key to alternate weights - dist, azim angi
      real              wtlim(mxqual) ! limits of alternate variable
      real              wtmax          ! maximum weight
      real              wtobs(mxstat)  ! observed first motions weights
      real              xainmn        ! minimum ang of inc
      real              xainmx        ! maximum ang of inc
      character*7       xcode(mxstat) ! code giving reason for exclusion
c                                       k = kill station; i = angel of inc.;
c                                       a = azimuth; d = distance

      real              xlam(mxrake)   ! fault rake angle in degrees
      real              xlamc(mxrake)  ! fault rake angle in degrees for coarse search
      real              xlam0c         ! initial fault rake angle in degrees for coarse search
      real              xlam0f         ! initial fault rake angle in degrees for fine search
      real              xlam1f         ! terminating fault rake angle in degrees for fine search
      character*80      aask           ! character function to get file name
      character*80      fname          ! name of file
      character*80      fname1         ! name of file with input list of axes
      character*80      fname2         ! name of file with list of fit values
      character*80      ftemp          ! temporary file name
c
c begin code
c
c  initialize statistics arrays to zero
c
      data nstat, nfit /0, 20*0/
      data qcnt, qcntwt /mxqual*0, mxqual*0, mxqual*0.0, mxqual*0.0/
      data ndrng, nsrng, nrrng /mxdip*0, mxstrk*0, mxrake*0/
c
c
c open files
c
      print *, 'welcome to fpfit2!  this version of fpfit finds the'
      print *, 'best focal mechanism, and the distribution of p and t'
      print *, 'axes of solutions that are "almost" as good.  the trial'
      print *, 'p and t axis locations are taken from a list '
      print *, 'generated in advance by the program genax.'
c
c open trial solution file (tunit = 11)
c
115   fname1 = aask('name of file with list of trial mechanisms',
     1     '/Seis/S/we/src/fpfit/ptm10.dat', -80)
      call openit('read', fname1, tunit, ierr)
      if(ierr .ne. 0) goto 115
      read (tunit, 1155) fmt
1155  format (a)
      print *, 'format for reading trial mechanisms is:'
      print *, fmt
c
c open input phase data file name (iunit = 2)
c
116   fname = aask('name of data input file', 
     + 'input.pha', -80)
      call openit('read', fname, iunit, ierr)
      if(ierr .eq. 1) go to 116
      index1 = index(fname, ']') + 1
      index2 = index(fname(index1:80), '.') - 1
      root = fname(index1:index1+index2-1)
      root = aask('root name for output files', root, -80)
c
c     cunit = 1
12    ftemp = aask('name of fpfit control file', 
     &             '/Seis/S/we/src/fpfit/fpfit2.ctl', -80)
      call openit('read', ftemp, cunit, ierr)
      if(ierr .eq. 1) go to 12
      print *, 'composite solutions are now supported for hypoellipse f
     *iles (format number #3)'
      cmpsit = torf('should this be a composite run', .false.)
      if(cmpsit) then
122     print *, 'give the name of a file that will contain summary'
        print *, 'records for those events used in the composite.'
        cmpsum = aask('name:', 'none', -50)
        if(dnstrng(cmpsum) .ne. 'none') 
     *    call openit ('write', cmpsum, munit, 
     *  ierr)
        if(ierr .ne. 0) goto 122
      endif
c
      print *, 'one program option will add to the .ray file a list of'
      print *, ' those nearby solutions that have unique patterns of'
      print *, ' station reversals.  if desired, just these solutions '
      print *, ' may be plotted with fpplot.  this option only '
      print *, ' considers up to the first 80 stations.'
      uniq = torf ('do you want to identify unique reversal patterns',
     *             .false.)
      print *, 'a list of the number of each of the trial solutions '
      print *, ' within the confidence region may be generated.'
      lstall = torf ('do you want to generate a list of indices', 
     *             .false.)
c
      if(lstall) then
c       kunit = 12
        ftemp = root(1:lentru(root))//'.idx'      
124     fname2 = aask('name of file with indices', ftemp, -80)
        call openit('write', fname2, kunit, ierr)
        if (ierr .ne. 0) goto 124
      endif
      print *, 'the resolution of the distribution of p and t axes'
      print *, 'depends upon the increment used in the secondary '
      print *, 'grid search.  the default increment is 1 degree, but it'
      print *, 'could be increased to speed up the program.  if the'
      print *, 'increment is set greater than 9 degrees, no secondary'
      print *, 'search is done.'
      seci = raskk ('increment for secondary grid search', 1.)
c
c     punit = 3
      ftemp = root(1:lentru(root))//'.ray'      
      call openit('write', ftemp, punit, ierr)
      if(ierr .eq. 1) stop
c
c     sunit = 4
      ftemp = root(1:lentru(root))//'.extsum'      
      call openit('write', ftemp, sunit, ierr)
      if(ierr .eq. 1) stop
c
c     eunit = 8
      ftemp = root(1:lentru(root))//'.out'      
      call openit('write', ftemp, eunit, ierr)
      if(ierr .eq. 1) stop
c
      write(eunit, 1154) fname1
1154  format(' input list of grid axes = ', a)
c
c     funit = 9
c     ftemp = root(1:lentru(root))//'.fit'      
c     call openit('write', ftemp, funit, ierr)
c     if(ierr .eq. 1) stop
c
c read in control parameters
c
      print *, 'read in control parameters'
      if (.not. fpinp (afitf, cunit, ddelc, ddelf, del0c, dfitc, 
     & distmx, dlamc, dlamf, dphic, dphif, erate, eunit, fmagmn, iamwt, 
     & ijeff, infmt, iprnt, ires, iunit, kilsta, maxobs, minobs, mxdip,
     & mxqual, mxrake, mxstat, mxstrk, ndelc, ndelf, nev, nkil, nlamc, 
     & nlamf, nphic, nphif, nrev, phi0c, plusert, presmx, revsta, 
     & weight, wterat, wtkey, wtlim, xainmn, xainmx, xlam0c))
     & stop
c                  
c     funit = 9
c     if (iprnt .eq. 1) open (unit = funit, status = 'new', form =
c    & 'formatted', access = 'sequential')
c
      title = aask('title for this run', 'title', -50)
      write (punit, 125) title, seci
      write (eunit, 125) title, seci
125   format(1x, a, ' sec. grid srch. inc. = ', f5.1)
c
      ievp = 0
      ievr = 0
c
c time reading the models
      handle = 0
c      it = lib$init_timer(handle)
c
c read in fault models
c

        read (tunit, *) ncase, ntcase, secinc
        print '(a, i4, a)', ' reading in fault models for ', ncase, 
     *  ' p axes.'
        print '(a, i4, a)', ' for each p axis, ', ntcase, 
     *  ' t axes are tried.'
        if(ncase .gt. mxcase) then
          print *, 'mxcase parameter set to ', mxcase
          print *, 'but the list of trial mechanisms sets ', mxcase
          stop
        endif
        if(ntcase .gt. mxntc) then
          print *, 'mxntc parameter set to ', mxntc
          print *, 'but the list of trial mechanisms sets ', ntcase
          stop
        endif
        do 14 ic = 1, ncase
          read (tunit, *) i, dum1, dum2
          if(ic .ne. i) then
145         print *, 'structural error in trial mechanism file'
            stop
          endif
          axaz(ic) = dum1
          axdp(ic) = dum2
          do 13 it = 1, ntcase
            read (tunit, fmt, end = 145) dpdsv(it, ic), dppsv(it, ic),
     *      rkksv(it, ic), (tmsv(j, it, ic), j = 1, 6) 
13        continue
14      continue
        print *, 'done reading.'
c        it = lib$show_timer(handle)
c
c time finding best mechanism
        handle = 0
c        it = lib$init_timer(handle)
c
c find maximum weight
c
      wtmax = 0.
      do 15 i = 1, mxqual
        if (weight(i) .gt. wtmax) wtmax = weight(i)
15    continue
c
c read next event 
c
20    ievr = ievr + 1
      print *, 'read event number ', ievr
      if (infmt .eq. 1) then
      call readeq (ain, az, dist, distmx, eunit, event, fmagmn, ijeff,
     & iunit, kilsta, minobs, mxqual, mxstat, nkil, nr, nrev, 
     & pobs, prcntx, prmk, revsta, sigmaf, stn, sumwt, weight, wtobs)
      else if (infmt .eq. 2) then
       call rdeq2 (ain, az, dist, distmx, eunit, event, fmagmn,
     & ijeff, iunit, kilsta, minobs, mxqual, mxstat, nkil, nr, nrev,
     & pobs, prcntx, prmk, revsta, sigmaf, stn, sumwt, weight, wtobs)
      else if (infmt .eq. 3) then
      call rdeq3 (ain, az, cmpsit, cmpsum, date, dist, distmx, erate, 
     & eunit, event, fmagmn, ijeff, iunit, jjwt,kilsta, maxobs, minobs, 
     & munit, mxqual, mxstat, nkil, nr, nrev, nrwt, plusert, pobs, 
     & prcntx, presmx,prmk, revsta, sigmaf, stn, sumwt,
     & wterat, wtkey, wtlim, wtmax, wtobs, xainmn, xainmx, xcode)
      end if
      print *, 'read in event with ', nrwt, ' first motions used.'
c
      if (nr .eq. -1 .or. ievp .eq. nev) then
c
c end of data, close files
c
        ievr = ievr - 1
        close (cunit)
        close (iunit)
        close (punit)
        close (sunit)
c       if (iprnt .eq. 1) close (funit)
        if (nstat .gt. 0) then
c
c         generate summary listing of polarity discrepancies as a 
c         function of station and quality, the distribution of fit 
c         parameters, and the distribution of dip, strike, and rake 
c         ranges about best fit solution.
          call fpout (ddelf, dlamf, dphif, erate, eunit, ievp, ievr,
     &    ind, ires, mxdip, mxqual, mxrake, mxstat, mxstrk, ndelf, 
     &    ndrng, nfit, nlamf, nphif, nrev, nrrng, nsrng, nstat, qcnt, 
     &    qcntwt,revsta, scnt, scntwt, stat, wterat, wtkey, wtlim)
c
        else
          write (eunit, *) '***** fpfit error: no events satisfy input
     & criteria *****'
        end if
        close (eunit)
c        it = lib$show_timer(handle)
        stop
      end if
c
c insufficient readings so skip event
c
      if (nrwt .eq. 0) then
        if (.not. cmpsit) goto 20
        write (eunit, *) 'too few first motions, so stop'
        stop
      endif
c
c begin event loop - loop through data, computing
c p-wave direction unit vector  (up, south, east) for each ray 
c
      call idate(irmo, irdy, iryr)
      call time(atime)
      read(atime, '(i2, 1x, i2, 1x, i2)')  ihr, imn, isec
      write(dattim, 1201) iryr, irmo, irdy, ihr, imn
1201  format(i2.2, '/', i2.2, '/', i2.2, ' - ', i2.2, 
     1 ':', i2.2, ' ')
      write (punit, 22) ievr, dattim
22    format (' c*event number ', i5, '    fpfit2 solution of ', a)
      print *, 'begin event number ', ievr
d     print *, 'set up p-wave direction unit vector'
      do 30 i = 1, nr
        ainr = ain(i)*rad
        azr = az(i)*rad
        u(1) = -cos(ainr)
        u(2) = -sin(ainr)*cos(azr)
        u(3) = sin(ainr)*sin(azr)
c
c find excitation coefficients for determining far-field p radiation 
c pattern 
c
        call pexcf1 (coef, i, mxstat, u)
30    continue
c
c loop through ncase p axis locations
c
      ievp = ievp + 1
      fitmnn = 2.
      fitmxx = -1.
      botmnn = 0.
      do 35 ic = 1, ncase
d       print *, 'ic = ', ic
c
c use the tmsv matrix for secinc (10) degree increments of t axis
c
d        print *, '1st coarse -180 to -10 by ', secinc
c9871    call tmgen (axaz(ic), dpdir, dipp, axdp(ic), dpr, mxcase, nsec,
c     *  rad, rakk, sechi, secinc, seclow, tm)
c
c find the best of these nsec+1 trial solutions (tm) 
c
9872    call search2 (bot2, botmn, botmx, chart, coef, fitmx, fitmn, 
     *  iamwt, ic, ntcase, mmn, mmx, mxcase, mxntc, mxstat, nr, pobs, 
     *  secinc, smnlow(ic), smnhi(ic), smxlow(ic), smxhi(ic), tmsv,
     *  uniq, wtobs)
d       print *, '    fitmn,    fitmx,   smnlow,    smnhi,   ',
d    *  'smxlow,     smxhi '
d       print '(6f10.2)',  fitmn, fitmx, smnlow(ic), smnhi(ic), 
d    *  smxlow(ic), smxhi(ic)
c
c
c save the fault dip direction, dip, and rake, and the chart and 
c fit value for this solution - minimum fit value
c
        if (uniq) charts(ic) = chart(mmn)
        dpdirs(ic) = dpdsv(mmn, ic)
        dipps(ic) = dppsv(mmn, ic)
        rakks(ic) = rkksv(mmn, ic)
        fits(ic) = fitmn
        bots(ic) = botmn
        if(fitmn .lt. fitmnn) then
          fitmnn = fitmn
          icmn = ic
          if(ic .eq. 1) botmnn = botmn
        else if ( (fitmn .eq. fitmnn) .and. (botmn .gt. botmnn) ) then
c         use bot to break ties
          fitmnn = fitmn
          icmn = ic
          botmnn = botmn
        endif
c
c save the fault dip direction, dip, and rake, and the chart and 
c fit value for this solution - maximum fit value
c
        if (uniq) chartb(ic) = chart(mmx)
        dpdirb(ic) = dpdsv(mmx, ic)
        dippb(ic) = dppsv(mmx, ic)
        rakkb(ic) = rkksv(mmx, ic) 
        fitb(ic) = fitmx
        botb(ic) = botmx
c       write(33, '(i5, 2(3f7.1, 2g10.4, a))') ic, dpdirb(ic), 
c    1  dippb(ic), rakkb(ic), fitb(ic), botb(ic), chartb(ic)(1:14),
c    1                                             dpdirs(ic), 
c    1  dipps(ic), rakks(ic), fits(ic), bots(ic), charts(ic)(1:14)
        if(fitmx .gt. fitmxx) then
d         print *, 'current max = ', fitmx, ' mmx, ic = ', mmx, ic
d         print *, 'dip dir    dip    rake = ', dpdirb(ic), dippb(ic),
d    *    rakkb(ic)
          fitmxx = fitmx
          icmx = ic
          botmxx = botmx
        else if ( (fitmx .eq. fitmxx) .and. (botmx .gt. botmxx) ) then
c         use bot to break ties
d         print *, 'current max = ', fitmx, ' mmx, ic = ', mmx, ic
d         print *, 'dip dir    dip    rake = ', dpdirb(ic), dippb(ic),
d    *    rakkb(ic)
          fitmxx = fitmx
          icmx = ic
          botmxx = botmx
        endif
35    continue
        print '(a, i4, a)', ' finished first coarse search of ', 
     *  ncase, ' axes.'
c        it = lib$show_timer(handle)
c
c find the best solution based on first coarse search
c
      if(1. - fitmxx .lt. fitmnn) then
c       print *, 'biggest fit wins, with ic = ', icmx
c       print *, 'in this case best solution is flipped'
        botmax = botmxx
        fitmin = 1. - fitmxx
        idpdr1 = iround(dpdirb(icmx))
        idip1 = iround(dippb(icmx))
        islip1 = iround(rakkb(icmx)) + 180
        islip1 = jmod(islip1, 360)
        if (islip1 .gt. 180) islip1 = islip1 - 360
        if (islip1 .lt. -180) islip1 = islip1 + 360
      else if(1. - fitmxx .gt. fitmnn) then
c        print *, 'smallest wins, with ic = ', icmn
        botmax = botmnn
        fitmin = fitmnn
        idpdr1 = iround(dpdirs(icmn))
        idip1 = iround(dipps(icmn))
        islip1 = iround(rakks(icmn))
      else if(botmxx .gt. botmnn) then
c       they are the same, so consider the bot value
        botmax = botmxx
        fitmin = 1. - fitmxx
        idpdr1 = iround(dpdirb(icmx))
        idip1 = iround(dippb(icmx))
        islip1 = iround(rakkb(icmx)) + 180
        islip1 = jmod(islip1, 360)
        if (islip1 .gt. 180) islip1 = islip1 - 360
        if (islip1 .lt. -180) islip1 = islip1 + 360
      else
        botmax = botmnn
        fitmin = fitmnn
        idpdr1 = iround(dpdirs(icmn))
        idip1 = iround(dipps(icmn))
        islip1 = iround(rakks(icmn))
      endif
      if (lstall) then
d       print *, 'coarse grid search solution dip dir, dip, rake, fit:'
d       print *, idpdr1, idip1, islip1, fitmin, botmax
        idum = 0
        write (kunit, '(i6, 2x, g10.4, 3i10, g10.4, a)') 
     1  idum, fitmin, idpdr1, idip1, islip1, botmax,
     1  ' best grid solution'
      endif
c
c  do final fine search centered on best grid solution
c  recompute chart for new best solution
c
d     print *, 'do fine search for the best solution'
d     print *, 'nphif, dphif, ndelf, ddelf, nlamf, dlamf'
d     print *,  nphif, dphif, ndelf, ddelf, nlamf, dlamf
c
c     integer           idip1           ! (input) initial fault dip angle in degrees
c     real              dip2            ! (output) fault dip 
c     integer           idpdr1          ! (input) initial fault dip direction angle in degrees
c     real              dpdr2           ! (output) fault dip direction in degrees
c     integer           islip1          ! (input) initial fault slip angle in degrees
c     real              slip2           ! (output) fault slip angle in degrees
d     print *, 'input to fsearch2: idpdr1, idip1, islip1, nr'   
d     print *, idpdr1, idip1, islip1, nr
c
      call fsearch2 (bstbot, coef, ddelf, dlamf,
     &dphif, fitmin, iamwt, idip1, dip2, idpdr1, dpdr2, islip1, 
     &slip2, mxstat, ndelf, nlamf, nphif, nr,
     &pobs, rad, wtobs, punit)
c
c if lstall is true, write out this solution
c
      if (lstall) then
        idum = 0
        write (kunit, '(i6, 2x, g10.4, 3f10.2, g10.4, a)') 
     1  idum, fitmin, dpdr2, dip2, slip2, bstbot, ' best free solution'
      endif

c
c express solution in terms of dip direction, dip angle, and slip 
c angle. 
c
d     print *, 'express solution in terms of dip etc'
      idpdr2 = iround(dpdr2)
      idip2 = iround(dip2)
      islip2 = iround(slip2)
c
c replace each plane with idip1 = 0 by its auxilliary plane
c
      if (idip2 .eq. 0) then
        call auxpln (float(idpdr2), float(idip2), float(islip2),
     &  dd2, da2, sa2)
        idpdr2 = iround(dd2)
        idip2  = iround(da2)
        islip2 = iround(sa2)
      end if
c
c for cases where plane is vertical and dip direction .ge. 180, 
c flip representation to one with dip direction .le. 180
c
      if (idip2 .eq. 90 .and. idpdr2 .ge. 180.) then
        islip2 = -islip2
        idpdr2 = idpdr2 - 180.
      end if
d     print *, 'fine search solution dip dir, dip, rake, fit:'
d     print *, idpdr2, idip2, islip2, fitmin
c
c set limit for marginal solutions
c
      fit90 = 1.282 * sigmaf
      if(afitf .ge. 0.) fit90 = afitf
      fitlim = fitmin + fit90
c     if(fitlim .gt. .5) fitlim = .5
d     print *, 'fitmin, fitlim ', fitmin, fitlim
      print *, 'finished fine search.'
c      it = lib$show_timer(handle)
c
c loop through axes again and improve those that are just above fitlim
c
      
      if ( (seci .le. 9.) .and. (fitlim .lt. .5) )then
        do 43 ic = 1, ncase
ccc       if ( (fits(ic) .gt. fitlim) .and. (fits(ic) .lt. .5) ) then
          if (fits(ic) .lt. .5) then
d           print *, 'try to improve #', ic, ' fitmin = ', fits(ic)
c
c zero in on the lowest fit values with 1 degree increments
c first get the tm matrix for 1 degree increments of t axis
c
c           first define the set of nsec+1 tm's
d           print *, 'find maximum in ', smxlow(ic), ' to ', smxhi(ic),
d    *      ' by 1.'
9873        call tmgen (axaz(ic), dpdir, dipp, axdp(ic), dpr, mxcase, 
     *      nsec, rad, rakk, smnhi(ic), seci, smnlow(ic), tm)
d           print *, 'now use search3 in 1 degree search for minimum.'
d           print *, 'try ', nsec+1, ' mechanisms'
9874        call search3(botmn, botmx, chart, coef, fitmx, fitmn, 
     *      iamwt, nsec+1, mmn, mmx, mxcase, mxstat, nr, pobs, tm, 
     *      uniq, wtobs)
d           print *, 'mmn, fitmn ', mmn, fitmn
c
c save the fault dip direction, dip, and rake, and the chart and 
c fit value for this solution
c
            if (uniq) charts(ic) = chart(mmn)
            dpdirs(ic) = dpdir(mmn)
            dipps(ic) = dipp(mmn)
            rakks(ic) = rakk(mmn)
d           if ( fitmn .le. fitlim) then
d             print *, 'fitmn improved from ', fits(ic), ' to ', fitmn
d           endif
            fits(ic) = fitmn
          endif
c
c zero in on the highest fit values with 1 degree increments
c first get the tm matrix for 1 degree increments of t axis
c
          if(1. - fitb(ic) .lt. .5) then
ccc       if ( ((1. - fitb(ic)) .gt. fitlim) .and. 
ccc  *         ((1. - fitb(ic)) .lt. .5) ) then
d           print *, 'try to improve #', ic, ' fitmx = ', fitb(ic)
c           first define the set of nsec+1 tm's
d           print *, 'find maximum in ', smxlow(ic), 
d    *      ' to ', smxhi(ic), ' by 1.'
            call tmgen (axaz(ic), dpdir, dipp, axdp(ic), dpr, mxcase, 
     *      nsec, rad, rakk, smxhi(ic), seci, smxlow(ic), tm)
c           now get the highest value of fit for this p axis
            call search3(botmn, botmx, chart, coef, fitmx, fitmn, 
     *      iamwt, nsec+1, mmn, mmx, mxcase, mxstat, nr, pobs, tm, 
     *      uniq, wtobs)
d           print *, 'mmx, fitmx ', mmx, fitmx
c
c save the fault dip direction, dip, and rake, and the chart and 
c fit value for this solution
c
            if (uniq) chartb(ic) = chart(mmx)
            dpdirb(ic) = dpdir(mmx)
            dippb(ic) = dipp(mmx)
            rakkb(ic) = rakk(mmx)
d           if ( (1. - fitmx) .le. fitlim) then
d             print *, 'fitmx improved from ', fitb(ic), ' to ', fitmx
d           endif
            fitb(ic) = fitmx
          endif
c
43      continue
c
      endif
      print *, 'finished secondary grid search of all axes.'
c      it = lib$show_timer(handle)
c
c find the nstar solutions that have fits at least as good as fitlim
c
      nstar = 0
      npax = 0
      ntax = 0
      do 48 ic = 1, ncase
        if ( (1. - fitb(ic)) .le. fitlim + .00001) then
c         this axis may be a t axis
          ntax = ntax + 1
          nstar = nstar + 1
          ipt(nstar) = 2
          fitz(nstar) = 1. - fitb(ic)
          dpdirz(nstar) = dpdirb(ic)
          dippz(nstar) = dippb(ic)
          rakkz(nstar) = rakkb(ic) + 180.
          rakkz(nstar) = amod(rakkz(nstar), 360.)
          if (rakkz(nstar) .gt. 180) rakkz(nstar) = rakkz(nstar) - 360
          if (rakkz(nstar) .lt. -180) rakkz(nstar) = rakkz(nstar) + 360
          if (uniq) then
            do 45 i = 1, max(nr, 80)
              if (chartb(nstar)(i:i) .eq. '0') then
                chartz(ic)(i:i) = '1'
              else
                chartz(ic)(i:i) = '0'
              endif
45          continue
          endif
c          if (ic .eq. m1) then 
c            m1s = nstar
c            print *, 'the index of the best solution is m1s = ', m1s
c          endif
        endif
        if (fits(ic) .le. fitlim + .00001) then
c         this axis may be a p axis
          npax = npax + 1
          nstar = nstar + 1
          ipt(nstar) = 1
          fitz(nstar) = fits(ic)
          dpdirz(nstar) = dpdirs(ic)
          dippz(nstar) = dipps(ic)
          rakkz(nstar) = rakks(ic) 
          chartz(nstar) = charts(ic)
c          if (ic .eq. m1) then 
c            m1s = nstar
c            print *, 'the index of the best solution is m1s = ', m1s
c          endif
        endif        
48    continue
d     print *, 'found ', nstar, ' good solutions'
c
c
c find out if fine search and coarse search results are the same
c
c          same = .false.
c          if( (idpdr1 .eq. idpdr2) .and. (idip1 .eq. idip2)
c     *    .and. (islip1 .eq. islip2) ) same = .true.
c
c find percent of sphere covered by p and t axes
c
           tpct = 1.*ntax/ncase
           ppct = 1.*npax/ncase
           print *, 'coverage for p and t axes is:  ', ppct, tpct
232        format(a, 2f10.3)
c
c assign quality code to solution
d      print *, 'assign quality code to solution'
c
            ftqual(2:2) = '|'
            if (fitmin .le. .025) then
              ftqual(1:1) = 'a'
            else if (fitmin .gt. 0.1) then
              ftqual(1:1) = 'c'
            else
              ftqual(1:1) = 'b'
            end if
            ftqual(3:3) = ' '
c
c write out results
d      print *, 'write out results'
c
            sflag = ' '
            avwt = sumwt/float(nrwt)
            stdr = bstbot/sumwt
            write (evfit, 100) idpdr2, idip2, islip2, fitmin,
     &      nrwt, avwt, stdr, prcntx, isrng, idrng, irrng, ftqual, sflag
100         format (i4, i3, i4, f6.3, i4, 1x, 2f5.2, 1x, f4.2,
     &      1x, 3i3, 1x, a3, a1)
c           if (iprnt .eq. 1) write (funit, 110) event, evfit
110         format ('1', a80, a52)
            write (sunit, 120) event, evfit
120         format (a80, a52)
            write (punit, 130) event, evfit
130         format (
     1' c* date     sec   lat     lon    z   m  n  g d1',
     1'  r  a  d  s  a  d  s x f    s q', /,
     1' s*', a80, /,
     1' c*  dd dp  rk  fit    n  avwt stdr  %m  dst dd dr q', /,
     1' f*', a52)
c get the other nodal plane and rake
            call auxpln (float(idpdr2), float(idip2), float(islip2),
     1      dd2, da2, sa2)

c get the p and t axes
	    call tandp 
     1        (ain3, ain1, az3, az1, float(idip2), da2, 
     1        float(idpdr2), dd2, pi, rad)
c switch p and t if faulting is normal
	    if(islip2 .lt. 0.) then
	      tempv = ain1
	      ain1 = ain3
	      ain3 = tempv
	      tempv = az1
	      az1 = az3
	      az3 = tempv
	    endif
c switch from angle of incidence to dip
	    pl1 = 90. - ain1
	    pl3 = 90. - ain3
            if(pl1 .lt. 0.0) then
              pl1 = abs(pl1)
              az1 = az1 + 180.
              if(az1 .gt. 360.) az1 = az1 - 360.
            endif
	    if(az1 .lt. 0.0) az1 = 360. + az1
            pl3 = 90. - ain3
            if(pl3 .lt. 0.0) then
              pl3 = abs(pl3)
              az3 = az3 + 180.
              if(az3 .gt. 360.) az3 = az3 - 360.
            endif
	    if(az3 .lt. 0.0) az3 = 360. + az3

c cross multiply to get b (null) axis
	    call cromult
     1        (az1, pl1, az3, pl3, az2, pl2)
            write (punit, 132) int(dd2), int(da2), int(sa2), fitlim,
     1        int(az1), int(pl1), int(az2), int(pl2), 
     1        int(az3), int(pl3)
132         format(
     1' c*  dd dp  rk fitlim pbt = az1 pl1  az2 pl2  az3 pl3', /,
     1' c*'i4, i3, i4, f6.3, 7x, 3(i4, i4, 1x))
c
c recompute moment tensor representation for best solution to check
c for polarity discrepancies
c
d           print *, 'recompute moment tensor for best solution'
            call shrflt1 ( idpdr2*rad, idip2*rad, islip2*rad,
     &                     tmbst )

            do 158 k = 1, nr
              if (nstat .ge. 1) then
                do 135 i = 1, nstat
cd                 print *, 'nstat, stn(k), k, stat(i), i ', nstat,
cd    &                             stn(k), k, stat(i), i
                  if (stn(k) .eq. stat(i)) goto 150
135             continue
              end if
              nstat = nstat + 1
              if (nstat .gt. mxstat) then
                write (eunit, *) '***** fpfit error: # of stations have 
     &polarity discepancies exceeds ', mxstat, ' *****'
                write (eunit, *) ' station ', stn(k), ' not reported in 
     &final summary'
                goto 158
              end if
              i = nstat
              stat(nstat) = stn(k)
              scnt(nstat, 1) = 0
              scnt(nstat, 2) = 0
              scntwt(nstat, 1) = 0.
              scntwt(nstat, 2) = 0.
150           ipwt = jjwt(k)
c
c recompute radiation pattern
c
              prad = 0
              do 156 l = 1, 6
                prad = prad + tmbst(l)*coef(l, k)
156           continue
c
c check polarity and update appropriate station count
c
              if (sign(0.5, prad) .ne. pobs(k)) then
                  scnt(i, 1) = scnt(i, 1) + 1
                  scntwt(i, 1) = scntwt(i, 1) +
     & wtobs(k)*sqrt(abs(prad))
                  qcnt(ipwt, 1) = qcnt(ipwt, 1) + 1
                  qcntwt(ipwt, 1) = qcntwt(ipwt, 1) +
     & wtobs(k)*sqrt(abs(prad))
                bdflag(k) = '*'
                if (i .lt. 81) chartbst(i:i) = '1'
              else
                bdflag(k) = ' '
                if (i .lt. 81) chartbst(i:i) = '0'
              end if
              scnt(i, 2) = scnt(i, 2) + 1
              scntwt(i, 2) = scntwt(i, 2) +
     & wtobs(k)*sqrt(abs(prad))
              qcnt(ipwt, 2) = qcnt(ipwt, 2) + 1
              qcntwt(ipwt, 2) = qcntwt(ipwt, 2) +
     & wtobs(k)*sqrt(abs(prad))
158         continue
c
            write (punit, 1581) nstar, npline
1581        format(' m*', 2i6, ' (10(a1, 2f3.0, f4.0, i2))')
c
c print out best solutions
d      print *, 'print out best solutions'
c
            ndx = 0
            ncht = 1
            ucht(1) = chartbst
c
c           if the fine search has the same solution as the coarse, then
c           do not include other solutions with this reversal pattern in
c           list of unique patterns.
c
            write (punit, 1582) ' c* p and t axis coverage is:  ',
     *      ppct, tpct, ' b* begin alternate solutions'
1582        format(a, 2f10.3, /, a)
            do 200 m = 1, nstar
c               if ( (m .eq. m1s) .and. (same) ) go to 200
                ndx = ndx + 1
                rak2 = rakk(m)
                if (lstall) then
                  write (kunit, '(i6, 2x, g10.4, 3f10.2, i5)') 
     1            m, fitz(m), dpdirz(m), dippz(m), rakkz(m), ipt(m)
                endif
                write (string, '(i4, i3, i4, i2)') int(dpdirz(m)), 
     *          int(dippz(m)), int(rakkz(m)), ipt(m)
                if (fitz(m) .le. fitmin + .00001) string(1:1) = 'e'
                if (uniq) then
c look for new unique distributions of reversed stations
                  do 170 k = 1, ncht
                    if(ucht(k) .eq. chartz(m)) goto 175
170               continue
                  ncht = ncht + 1
                  ucht(ncht) = chartz(m)
                  string(1:1) = 'u'
                  if (fitz(m) .le. fitmin + .0001) string(1:1) = '&'
c                  write(punit, 174) int(dpdirz(m)), 
c     *            int(dippz(m)), int(rakkz(m)), ucht(ncht)
c174               format(' #', i3, i3, i4, 1x, a)
                endif
175             if (ndx .eq. 1) then
                  line = string
                else 
                  line = line(1:(ndx - 1)*npfmt)//string
                end if
                if (ndx .eq. npline) then
                  ndx = 0
                  write (punit, 160) line
160               format (' ', a)
                end if
200         continue
            ncht = ncht - 1
            if (uniq) print *, 'found ', ncht, 
     *      ' different discrepancy patterns.'
            if (ndx .ne. 0) write (punit, 160) line
c
c accumulate statistics on fit parameter distribution for best solutions only
c
d           print *, 'accumulate statistics on fit parameter dist.'
            indx = iround(fitmin/.025) + 1
            if (indx .gt. 20) indx = 20
            nfit(indx) = nfit(indx) + 1
c
c write out to .ray file
c
            do 260 k = 1, nr
240           write (punit, 250) stn(k), date(k), dist(k), az(k), 
     &        ain(k), prmk(k), wtobs(k), bdflag(k), xcode(k)
250           format (1x, a4, 1x, a10, 3f6.1, 3x, a4, 1pe12.2, 2x,
     &                a1, a7)
260         continue
            write (punit, *) '              '
c
c end of solution loop
c
270     continue
c
c end of event
c
c      it = lib$show_timer(handle)
      goto 20
      end
c
c
c
c
c
