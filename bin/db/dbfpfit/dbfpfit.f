      program dbfpfit
c
c
c     August-September, 2002
c     Author: Natalia Ratchkovski
c
c     purpose: calculate double-couple fault plane solutions from p-wave first 
c              motions (see reasenberg, p.  and oppenheimer, d.,  fpfit, fpplot 
c              and fppage: fortran computer programs for calculating and 
c              displaying earthquake fault-plane solutions, u.s. geological 
c              survey open-file report 85-???).
c
c
c     input files:   1.  a css database with earthquake location and arrivals 
c                        origin, assoc and arrival tables should be present
c                        
c                    2.  a parameter file dbfpfit.pf with control parameters (cunit)
c
c                    3.  a list of trial focal mechanisms (path is given in dbfpfit.pf file)
c
c     required routines: all routines are enclosed
c
c      output:       1. an ascii file of summary cards extended with fault plane solution 
c                       parameters on logical unit sunit (=4).
c                    2. an ascii file consisting, for each earthquake, of the hypo71 extended 
c                       summary card, followed by neighboring solutions (within 90% confidence limits), 
c                       followed by individual p-phase information, on logical unit punit (=3). 
c                       this file serves as input to programs "fpplot" and "fppage".
c                    3. an ascii file describing any errors in the control file, hypo71 file, presence 
c                       of multiple mechanisms, a summary of polarity discrepancies by station and 
c                       reading quality, and the distribution of strike, dip, and rake uncertainties
c                       on logical unit eunit (=8)
c                    4. fault plane parameters are written into fplane table of the css database
c  
c      authors:      paul reasenberg and david oppenheimer, u.s.g.s. in menlo park.  some of the routines
c                    were adapted from code written by john lahr, bruce julian, and fred klein.
c                    mark matthews, stanford university, provided assistance in the error propagation analysis.
c
c                    this version was modified by j. c. lahr to read a list of
c                    trial focal mechanisms.
c
c                    Natalia Ratchkovski modified this program to read input from a css database and 
c                    write output into fplane table.
c                    Eliminated options:
c                    - no restricted search 
c                    - different scheme for observation weighting. based on either deltim parameter
c                      (uncertainity of a pick) or signal-to-noise ratio and a distance weighting.
c                      all are optionaland controlled through a parameter file
c

      include "db.i"
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
      integer		result
c     Mitch added the below fixed length strings 
c     fortran will pass string until null, if compiler does not know string length 
c     in MAX OS X
      character*19      ch_sec_increment / 'secondary_increment' /
      character*11	ch_trial_mechs / 'trial_mechs' /
      character*9	ch_composite / 'composite' /
      character*15	ch_unique_revesals / 'unique_revesals' /
      character*16	ch_trialsol_confreg / 'trialsol_confreg' /

      parameter (cunit = 1, eunit = 8, funit = 9, iunit = 2, mxdip = 46,
     & mxqual = 8, mxrake = 46, mxslns = 20, mxstat = 10000, 
     & mxstrk = 46,
     & punit = 3, sunit = 4, tunit = 11, mxcase = 414, mxntc = 36, 
     & kunit = 12, npline = 10, npfmt = 13, munit=7)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (dpr = 180./pi)
c
      real              ain(mxstat) ! ray incidence angles in degrees 
      real              ainr        ! ain converted to radians
      real              axaz(mxcase) ! p axis location - azimuth
      real              axdp(mxcase) ! p axis location - dip
      real              az(mxstat)  ! ray azimuth angles (corresponding to ain)
      real              avwt        ! mean observational weight of data used in solution
      real              azr         ! az converted to radians
      character*1       bdflag(mxstat) ! signals polarity discrepancy with best fit solution
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
      logical           cmpsit      ! true for composite solutions
      character*50      cmpsum      ! file with eqs used in composite
      real              da2         ! dip angle of auxilliary solution
      character*18      dattim      ! date/time of run
      real              dd2         ! dip direction of auxilliary solution
      character*10      date(mxstat)                    ! arrival time date yrmodyhrmn
      real              ddelc       ! fault dip increment in degrees for coarse search
      real*8            ddelf       ! fault dip increment in degrees for fine search
      real              del0c       ! initial fault dip angle in degrees for coarse search
      real*8            dfitc       ! increment to coarse fit function
      real              dipp(mxcase) ! dip of nodal plane
      real              dippb(mxcase) ! dip of nodal plane for biggest
c                                       fit with ith p axis
      real              dipps(mxcase) ! dip of nodal plane for smallest
c                                       fit with ith p axis
      real              dippz(mxcase) ! dip of nodal plane for marginal
c                                       solution
      real              dist(mxstat)                    ! epicentral distance
      real*8            distmx      ! maximum permitted epicentral distance 
      real              dlamc       ! fault rake increment in degrees for coarse search
      real*8            dlamf       ! fault rake increment in degrees for fine search
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
      real*8            dphif       ! fault strike increment in degrees for fine search
      real              dppsv(mxntc, mxcase) ! p solution associated
c                                              with this axis (dip)
      real              erate(mxqual) ! assumed weighted error rates for each data class
      character*80      event       ! summary card
      character*51      evfit       ! dummy character string to hold fit values on output
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
      real*8            fmagmn      ! minimum permitted magnitude
      character*50      fmt         ! format for reading trial solutions
      logical           fpinp       ! function
      character*3       ftqual      ! solution quality: concatenated fit quality, range quality
      integer           i           ! loop index
      integer           iamwt       ! code specifying type of amplitude weighting to use
      integer           idip1       ! dip angle of best fit
      integer           idpdr1      ! dip direction of best fit
      integer           idrng       ! dip range variation for solutions with fit<fitlim
      integer           ierr        ! i/o error code on open
      integer           ievp        ! number of events processed
      integer           ievr        ! number of events read
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
      integer           islip1      ! rake of best fit
      integer           isnrwt      ! flag controling signal-to-noise weighting (0=omit, 1=use)
      integer           isrng       ! strike range variation for solutions with fit<fitlim
      integer           j           ! loop index over dip
      integer           jjwt(mxstat) ! index for data weight 
      integer           k           ! loop index
      character*4       kilsta(mxstat)                  ! ignored station names
      integer           l           ! loop index over moment tensor
      character*131     line        ! output string of nearby solutions orientations
      logical           lstall      ! if true then list indices of good solutions
      integer           m           ! loop index over rake
      integer           maxobs      ! maximum number of observations rquired
      integer           minobs      ! minimum number of observations required 
      integer           ncht        ! number of unique patterns of reversed stations
      integer           ndelc       ! number of fault dip increments for coarse search
      integer           ndelf       ! number of fault dip increments for fine search
      integer           ndrng(mxdip) ! number of dip solution ranges binned into ddelf degree increments
      integer           nev         ! number of events to process
      integer           nfit(20)    ! number of solutions binned into .025 fit increments
      integer           nkil        ! number of ignored stations
      integer           nlamc       ! number of fault rake increments for coarse search
      integer           nlamf       ! number of fault rake increments for fine search
      integer           npax        ! number of cells covered by p axes
      integer           nphic       ! number of fault strike increments for coarse search
      integer           nphif       ! number of fault strike increments for fine search
      integer           nr          ! -1=eof, 0=skip event, nr>0 => number of stations 
      integer           nrwt        ! number used in fm
      integer           nrev        ! number of reversed stations
      integer           nrrng(mxrake) ! number of rake solution ranges binned into dlamf degree increments
      integer           nsrng(mxstrk) ! number of strike solution ranges binned into dphif degree increments
      integer           nstar         ! number of solutions having fit within 5% of fitmin
      integer           nstat         ! total # of stations reporting for entire data set
      integer           ntax          ! number of cells covered by t axes
      real              ppct          ! percent of cells with p axes
      real              phi0c       ! initial fault strike angle in degrees for coarse search
      real*8            plusert     ! error rate for + or - readings
      real              pobs(mxstat) ! observed first motion polarities; .5=compression, -.5=dilatation
      real              prad        ! radiation amplitude corresponding ain, az.  
                                    ! (dilatation) -1.<prad<+1.(compression)
      real              prcntx      ! % of stations that are machine picked
      real*8            presmx      ! maximum permitted p-residual
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
      integer           scnt(mxstat,2)   ! index 1=# of dscrpnt polarities for stat, index 2=# of observations
      real              scntwt(mxstat,2) ! index 1=weighted # dscrpnt polarities for stat, index 2=sum of weights
      character*1       sflag            ! flag indicating secondary solutions
      real              sigmaf           ! calculated standard deviation of fit based on data errors
      real              smnlow(mxcase)   ! lower limit of sec for 2nd stage search
      real              smnhi(mxcase)    ! upper limit of sec for 2nd stage search
      real              smxlow(mxcase)   ! lower limit of sec for 2nd stage search
      real              smxhi(mxcase)    ! upper limit of sec for 2nd stage search
      character*4       stat(mxstat)     ! names of all stations reporting
      real              stdr             ! station distibution ratio 
      character*4       stn(mxstat)  ! station names per event
      character*13      string       ! scratch variable
      real              sumwt        ! sum of observed first motion weights
      character*48      title        ! data set descriptor
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
      real*8            xainmn        ! minimum ang of inc
      real*8            xainmx        ! maximum ang of inc
      character*7       xcode(mxstat) ! code giving reason for exclusion
c                                       k = kill station; i = angel of inc.;
c                                       a = azimuth; d = distance

      real              xlam0c         ! initial fault rake angle in degrees for coarse search
      character*80      fname1         ! name of file with input list of axes
      character*80      fname2         ! name of file with list of fit values
      character*80      ftemp          ! temporary file name

c     integer           idip1           ! (input) initial fault dip angle in degrees
c     integer           idpdr1          ! (input) initial fault dip direction angle in degrees
c     integer           islip1          ! (input) initial fault slip angle in degrees
      real*8            dbdip2          ! (output) fault dip 
      real*8            dbstr2          ! (output) fault dip direction in degrees
      real*8            dbrake2         ! (output) fault slip angle in degrees
      real*8            dbdip1          ! (output) fault dip 
      real*8            dbstr1          ! (output) fault dip direction in degrees
      real*8            dbrake1         ! (output) fault slip angle in degrees
      real*8            paxazm          ! (output) azimuth of P-axis 
      real*8            paxplg          ! (output) plunge of P-axis 
      real*8            taxazm          ! (output) azimuth of T-axis 
      real*8            taxplg          ! (output) plunge of T-axis 

c     variables for dbfpfit
      integer           ndistwt     ! # of distance weights
      integer           nqualwt     ! # of quality weights
      real              wtqlim(mxqual)  ! limits of quality weighting
      character*256 dbin, orin, strcmpsit, struniq, strlstall, expr
      character*15 auth
      integer db(4), dbo(4), dbfp(4), dbtmp(4)
      integer*8 pf, nfp, nrec, lastmechid, orid, mechid
      integer  rc, iargc, iorin, jdb, keys 
      integer table_present, table_writable, nor
      real*8 seci
      
c
c begin code
c
c  initialize statistics arrays to zero
c
      data nstat, nfit /0, 20*0/
      data qcnt, qcntwt /mxqual*0, mxqual*0, mxqual*0.0, mxqual*0.0/
      data ndrng, nsrng, nrrng /mxdip*0, mxstrk*0, mxrake*0/

      print *, 'Welcome to dbfpfit!  This version of fpfit finds the'
      print *, 'best focal mechanism, and the distribution of p and t'
      print *, 'axes of solutions that are "almost" as good.'

c********start Natasha's modifications****************
      call pfread('dbfpfit', pf, result)
c     Mitch modified 
c     if  (result) then
      if  (result .ne. 0) then
        call die (0,'Can not read parameter file')
      endif
      print*,'Opened parameter file.'

c open trial solutions file (tunit = 11)

c     call pfget_string(pf,'trial_mechs',fname1)
c     ch_trial_mechs = 'trial_mechs'
      call pfget_string(pf, ch_trial_mechs,fname1)

      call openit('read', fname1, tunit, ierr)
      if(ierr .ne. 0) then
        call die (0, "Can't open file with trial mechanisms. 
     *  Check trial_mechs path in parameter file dbfpfit.pf")
      endif
      read (tunit, 1155) fmt
1155  format (a)

c open input phase data file name (iunit = 2)
c get name of the input database and output *.ray file
      m = iargc()
       
      if (m .eq. 4) then
        call getarg(3, dbin)
        call getarg(4, root)
      elseif (m .eq. 2) then
        call getarg(1, dbin)
        call getarg(2, root)
      else
        call die(0,"Usage: dbfpfit [-o origin] dbin file_out")
      endif
      
c    open input database
      rc = dbopen(dbin, "r+", db)
      if (rc .lt. 0) then
        call die(0,"dbfpfit: Unable to open database.")
      endif

      call dblookup (dbo, db, "", "origin", "", "")
      call dbquery(dbo, dbRECORD_COUNT, nor)
      if (nor .lt. 1) then
        call die (0, "dbfpfit: No origins to process in origin table")
      endif
      
c    removed open control file
      cmpsit = 0
      uniq = 0
      lstall = 0
c    check if composite run
c     call pfget_string( pf, "composite", strcmpsit)
c     ch_composite = 'composite'
      call pfget_string( pf, ch_composite , strcmpsit)
      if(strcmpsit.eq.'yes' .or. strcmpsit.eq.'Yes') then
      cmpsit=1
        print *, 'This is a composite run.'
      endif

c     call pfget_string( pf, "unique_revesals", struniq)
c     ch_unique_revesals = 'unique_revesals'
      call pfget_string( pf, ch_unique_revesals, struniq)
      if(struniq.eq.'yes' .or. struniq.eq.'Yes') then
      uniq = 1
      print *,'The program will identify station reversals.'
      endif

c     call pfget_string( pf, "trialsol_confreg", strlstall)
c     ch_trialsol_confreg = 'trialsol_confreg'
      call pfget_string( pf, ch_trialsol_confreg, strlstall)
      if(strlstall.eq.'yes' .or. strlstall.eq.'Yes') then
        lstall = 1
        print *, 'The program will generate list of trial solutions'
        print *, ' within theconfidence region.'
        fname2 = root(1:lentru(root))//'.idx'      
        call openit('write', fname2, kunit, ierr)
      endif

c     ch_sec_increment = 'secondary_increment'
      call pfget_double(pf, ch_sec_increment, seci, result)
c      print*,'seci =', seci
c     Mitch modified 
c     if (result) seci = 1.
      if (result .ne. 0) seci = 1.
      
c     open root.ray file for output
c     punit = 3
      ftemp = root(1:lentru(root))//'.ray'      
      call openit('write', ftemp, punit, ierr)
      if(ierr .eq. 1) stop

c     open root.extsum file for output
c     sunit = 4
c      ftemp = root(1:lentru(root))//'.extsum'      
c     call openit('write', ftemp, sunit, ierr)
c      if(ierr .eq. 1) stop
c
c     open root.out file for output
c     eunit = 8
      ftemp = root(1:lentru(root))//'.out'      
      call openit('write', ftemp, eunit, ierr)
      if(ierr .eq. 1) stop
c
      write(eunit, 1154) fname1
1154  format(' input list of grid axes = ', a)

c read in control parameters

      print *, ' '
      print *, ' Reading in control parameters from parameter file.'
      if (.not. fpinp ( auth, ddelc, ddelf, del0c, dfitc, distmx, 
     & dlamc, dlamf, dphic, dphif, erate, eunit, fmagmn, iamwt, ijeff, 
     & iprnt, isnrwt, kilsta, maxobs, minobs, mxdip, mxqual, mxrake, 
     & mxstat, mxstrk, ndelc, ndelf, ndistwt, nev, nkil, nlamc, nlamf, 
     & nphic, nphif, nqualwt, nrev, pf, phi0c, plusert, presmx, revsta, 
     & weight, wterat, wtkey, wtlim, wtqlim, xainmn, xainmx, xlam0c ))
     & stop

c     funit = 9

      print *, ' '
      title = dbin
      write (punit, 125) title, seci
      write (eunit, 125) title, seci
125   format(1x, a, ' sec. grid srch. inc. = ', f5.1)

c read in fault models

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
        print *, ' '


c find out how many events to process and if it's composite or not 

      if(iargc().eq.4) then
c one event to process with orid given in command line

        call getarg(2, orin)
        read(orin,*) iorin
c        print*,'orid = ', iorin
        nor = 1
        
      elseif(.not. cmpsit) then
c all events in the database to process, not composite

        iorin = 0
        call dblookup (dbo, db, "", "origin", "", "")
        call dbquery(dbo, dbRECORD_COUNT, nor)
        print*,'Number of events in origin table nor = ', nor
        
        if(nor.eq.1) then
        dbo(4) = 0
        jdb = dbgetv( dbo, 0, "orid", iorin, 0)
        print*,'One event in the database to process orid = ', iorin
        endif
        
      else
c all events to process as a composite solution 

        call dblookup (dbo, db, "", "origin", "", "")
        call dbquery(dbo, dbRECORD_COUNT, nor)
        print*,'Finding composite solution for ', nor,' events.'
        nor = 1
             
      endif
      
c read in events
      ievp = 0
      ievr = 0

      do 20 j = 0, nor-1
      
      if( nor. gt. 1. ) then
        dbo(4) = j
        jdb = dbgetv( dbo, 0, "orid", iorin, 0)
      endif        
      
      ievr = ievr + 1

      print*,''
      print*,'++++++++++++++++++++++++++++++++++++++++++++++++++'
      print *, 'Read event number ', ievr, ' with orid = ',iorin

      call readeqdb ( ain, az, cmpsit, date, db, dist, distmx, 
     & erate, eunit, event, fmagmn, ijeff, iorin, isnrwt, iunit,
     & jjwt, kilsta, maxobs, minobs, munit, mxqual, mxstat, ndistwt,
     & nkil, nor, nqualwt, nr, nrev, nrwt, plusert, pobs, prcntx,
     & presmx, prmk, revsta, sigmaf, stn, sumwt, sunit, wterat, wtkey,
     & wtlim, wtqlim, wtmax, wtobs, xainmn, xainmx, xcode )

      print *, 'read in event with ', nrwt, ' first motions used.'
c
      if (nrwt .eq. -1 .or. ievp .eq. nev) then
c
c end of data, close files
c
        ievr = ievr - 1
        close (cunit)
        close (iunit)
        close (punit)
c       close (sunit)
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
        stop
      end if
c
c insufficient readings so skip event
c
      if (nr .eq. 0) then
        if (.not. cmpsit) goto 20
        write (eunit, *) 'too few first motions, so stop'
        stop
      endif
c
c begin event loop - loop through data, computing
c p-wave direction unit vector  (up, south, east) for each ray 
c
      call jdate(irmo, irdy, iryr, ihr, imn, isec)
      write(dattim, 1201) iryr, irmo, irdy, ihr, imn
1201  format(i2.2, '/', i2.2, '/', i2.2, ' - ', i2.2, 
     1 ':', i2.2, ' ')
      write (punit, 22) ievr, dattim
22    format (' c*event number ', i5, '    fpfit solution of ', a)
      print *, 'begin event number ', ievr
c     print *, 'set up p-wave direction unit vector'
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
c
c find the best of these nsec+1 trial solutions (tm) 
c
        call search2 (bot2, botmn, botmx, chart, coef, fitmx, fitmn, 
     *  iamwt, ic, ntcase, mmn, mmx, mxcase, mxntc, mxstat, nr, pobs, 
     *  secinc, smnlow(ic), smnhi(ic), smxlow(ic), smxhi(ic), tmsv,
     *  uniq, wtobs)
c       print *, '    fitmn,    fitmx,   smnlow,    smnhi,   ',
c    *  'smxlow,     smxhi '
c       print '(6f10.2)',  fitmn, fitmx, smnlow(ic), smnhi(ic), 
c    *  smxlow(ic), smxhi(ic)
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
c         print *, 'current max = ', fitmx, ' mmx, ic = ', mmx, ic
c         print *, 'dip dir    dip    rake = ', dpdirb(ic), dippb(ic),
c    *    rakkb(ic)
          fitmxx = fitmx
          icmx = ic
          botmxx = botmx
        else if ( (fitmx .eq. fitmxx) .and. (botmx .gt. botmxx) ) then
c         use bot to break ties
c         print *, 'current max = ', fitmx, ' mmx, ic = ', mmx, ic
c         print *, 'dip dir    dip    rake = ', dpdirb(ic), dippb(ic),
c    *    rakkb(ic)
          fitmxx = fitmx
          icmx = ic
          botmxx = botmx
        endif
35    continue
        print '(a, i4, a)', ' finished first coarse search of ', 
     *  ncase, ' axes.'
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
c	Mitch change jmod with mod
c       islip1 = jmod(islip1, 360)
        islip1 = mod(islip1, 360)
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
c	Mitch change jmod with mod
c       islip1 = jmod(islip1, 360)
        islip1 = mod(islip1, 360)
        if (islip1 .gt. 180) islip1 = islip1 - 360
        if (islip1 .lt. -180) islip1 = islip1 + 360
      else
        botmax = botmnn
        fitmin = fitmnn
        idpdr1 = iround(dpdirs(icmn))
        idip1 = iround(dipps(icmn))
        islip1 = iround(rakks(icmn))
      endif
      print *, 
     * 'coarse grid search solution dip_dir, dip, rake, fit, botmax:'
      print *, idpdr1, idip1, islip1, fitmin, botmax
      if (lstall) then
c       print *, 'coarse grid search solution dip dir, dip, rake, fit:'
c       print *, idpdr1, idip1, islip1, fitmin, botmax
        idum = 0
        write (kunit, '(i6, 2x, g10.4, 3i10, g10.4, a)') 
     1  idum, fitmin, idpdr1, idip1, islip1, botmax,
     1  ' best grid solution'
      endif
c
c  do final fine search centered on best grid solution
c  recompute chart for new best solution
c
c     print *, 'do fine search for the best solution'
c     print *, 'nphif, dphif, ndelf, ddelf, nlamf, dlamf'
c     print *,  nphif, dphif, ndelf, ddelf, nlamf, dlamf
c
c     integer           idip1           ! (input) initial fault dip angle in degrees
c     real              dip2            ! (output) fault dip 
c     integer           idpdr1          ! (input) initial fault dip direction angle in degrees
c     real              dpdr2           ! (output) fault dip direction in degrees
c     integer           islip1          ! (input) initial fault slip angle in degrees
c     real              slip2           ! (output) fault slip angle in degrees
c     print *, 'input to fsearch2: idpdr1, idip1, islip1, nr'   
c     print *, idpdr1, idip1, islip1, nr
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
c     print *, 'express solution in terms of dip etc'
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
c
c set limit for marginal solutions
c
      fit90 = 1.282 * sigmaf
      fitlim = fitmin + fit90
c     if(fitlim .gt. .5) fitlim = .5
c     print *, 'fitmin, fitlim ', fitmin, fitlim
c      print *, 'finished fine search.'
      print *, 'fine search solution dip dir, dip, rake, fit:'
      print *, idpdr2, idip2, islip2, fitmin
c      print *, dpdr2, dip2, slip2
c
c loop through axes again and improve those that are just above fitlim
c
      if ( (seci .le. 9.) .and. (fitlim .lt. .5) )then
        do 43 ic = 1, ncase
ccc       if ( (fits(ic) .gt. fitlim) .and. (fits(ic) .lt. .5) ) then
          if (fits(ic) .lt. .5) then
c           print *, 'try to improve #', ic, ' fitmin = ', fits(ic)
c
c zero in on the lowest fit values with 1 degree increments
c first get the tm matrix for 1 degree increments of t axis
c
c           first define the set of nsec+1 tm's
c           print *, 'find maximum in ', smxlow(ic), ' to ', smxhi(ic),
c    *      ' by 1.'
            call tmgen (axaz(ic), dpdir, dipp, axdp(ic), dpr, mxcase, 
     *      nsec, rad, rakk, smnhi(ic), seci, smnlow(ic), tm)
c           print *, 'now use search3 in 1 degree search for minimum.'
c           print *, 'try ', nsec+1, ' mechanisms'
            call search3(botmn, botmx, chart, coef, fitmx, fitmn, 
     *      iamwt, nsec+1, mmn, mmx, mxcase, mxstat, nr, pobs, tm, 
     *      uniq, wtobs)
c           print *, 'mmn, fitmn ', mmn, fitmn
c
c save the fault dip direction, dip, and rake, and the chart and 
c fit value for this solution
c
            if (uniq) charts(ic) = chart(mmn)
            dpdirs(ic) = dpdir(mmn)
            dipps(ic) = dipp(mmn)
            rakks(ic) = rakk(mmn)
c           if ( fitmn .le. fitlim) then
c             print *, 'fitmn improved from ', fits(ic), ' to ', fitmn
c           endif
            fits(ic) = fitmn
          endif
c
c zero in on the highest fit values with 1 degree increments
c first get the tm matrix for 1 degree increments of t axis
c
          if(1. - fitb(ic) .lt. .5) then
ccc       if ( ((1. - fitb(ic)) .gt. fitlim) .and. 
ccc  *         ((1. - fitb(ic)) .lt. .5) ) then
c           print *, 'try to improve #', ic, ' fitmx = ', fitb(ic)
c           first define the set of nsec+1 tm's
c           print *, 'find maximum in ', smxlow(ic), 
c    *      ' to ', smxhi(ic), ' by 1.'
            call tmgen (axaz(ic), dpdir, dipp, axdp(ic), dpr, mxcase, 
     *      nsec, rad, rakk, smxhi(ic), seci, smxlow(ic), tm)
c           now get the highest value of fit for this p axis
            call search3(botmn, botmx, chart, coef, fitmx, fitmn, 
     *      iamwt, nsec+1, mmn, mmx, mxcase, mxstat, nr, pobs, tm, 
     *      uniq, wtobs)
c           print *, 'mmx, fitmx ', mmx, fitmx
c
c save the fault dip direction, dip, and rake, and the chart and 
c fit value for this solution
c
            if (uniq) chartb(ic) = chart(mmx)
            dpdirb(ic) = dpdir(mmx)
            dippb(ic) = dipp(mmx)
            rakkb(ic) = rakk(mmx)
c           if ( (1. - fitmx) .le. fitlim) then
c             print *, 'fitmx improved from ', fitb(ic), ' to ', fitmx
c           endif
            fitb(ic) = fitmx
          endif
c
43      continue
c
      endif
      print *, 'finished secondary grid search of all axes.'
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
c     print *, 'found ', nstar, ' good solutions'
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
c      print *, 'assign quality code to solution'
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
c      print *, 'write out results'
c
            sflag = ' '
            avwt = sumwt/float(nrwt)
            stdr = bstbot/sumwt
            write (evfit, 100) idpdr2, idip2, islip2, fitmin,
     &      nrwt, avwt, stdr, prcntx, isrng, idrng, irrng, ftqual, sflag
100         format (i3, i3, i4, f6.3, i4, 1x, 2f5.2, 1x, f4.2,
     &      1x, 3i3, 1x, a3, a1)
c           if (iprnt .eq. 1) write (funit, 110) event, '/', evfit
110         format ('1', a80, a1, a51)
c            write (sunit, 120) event, '/', evfit
120         format (a80, a1, a51)
            write (punit, 130) event, evfit
130         format (
     1' c* date       time     lat     lon       z      m    n  ', /,
     1' s*', a80, /,
     1' c*  dd dp  rk  fit    n  avwt stdr  %m  dst dd dr q', /,
     1' f* ', a51)

c get the other nodal plane and rake and the p and t axes
	    call tandpnew(float(idpdr2), float(idip2), float(islip2), 
     *        dd2, da2, sa2, az1, ain1, az3, ain3)

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
            iaz1 = az1 + sign(0.5, az1)
            iaz2 = az2 + sign(0.5, az2)
            iaz3 = az3 + sign(0.5, az3)
            idd2 = dd2 + sign(0.5, dd2)
            ida2 = da2 + sign(0.5, da2)
            isa2 = sa2 + sign(0.5, sa2)
            write (punit, 132) idd2, ida2, isa2, fitlim,
     1        iaz1, int(pl1 + 0.5), iaz2, int(pl2 + 0.5),
     1        iaz3, int(pl3 + 0.5)

132         format(
     1' c*  dd dp  rk fitlim pbt = az1 pl1  az2 pl2  az3 pl3', /,
     1' c*'i4, i3, i4, f6.3, 7x, 3(i4, i4, 1x))

c write out results into table fplane

      print*,''
      print*,'Writing fplane table'

      if ( idpdr2. ge. 90) then
        dbstr1 = idpdr2-90
      else
        dbstr1 = idpdr2+270
      endif
      if ( idd2. ge. 90) then
        dbstr2 = idd2-90
      else
        dbstr2 = idd2+270
      endif
      dbdip1 =  idip2
      dbdip2 =  ida2
      dbrake1 = islip2
      dbrake2 = isa2
      taxazm = iaz3
      paxazm = iaz1
      taxplg = int(pl3+.5)
      paxplg = int(pl1+.5)

      print*,'str1, dip1, rake1, str2, dip2, rake2, az3, pl3, az1, pl1'
      write(*,136) dbstr1, dbdip1, dbrake1, dbstr2, dbdip2, dbrake2,
     & taxazm, taxplg, paxazm, paxplg
136   format(2(f5.1,x,f4.1,x,f6.1,x),2(f5.1,x,f4.1,x))

c write output for non-composite
      if ( .not.cmpsit) then
      
c check it fplane table already exists
      call dblookup(dbfp, db, 0, "fplane", 0, 0)
c      print*,'dblookup fplane done'
      call dbquery(dbfp,dbTABLE_PRESENT,table_present)
c      print*,'table_psresent done',table_present
            
      if (table_present. eq. 0) then
c fplane table does not exist
        
        mechid = 1
c        print*,'next mechid = ', mechid
        jdb = dbaddv(db, "fplane","orid", iorin, "mechid", mechid,
     &                           "str1", dbstr1,
     &                           "dip1", dbdip1,
     &                           "rake1", dbrake1,
     &                           "str2", dbstr2, 
     &                           "dip2", dbdip2, 
     &                           "rake2", dbrake2, 
     &                           "taxazm", taxazm, 
     &                           "taxplg", taxplg,
     &                           "paxazm", paxazm, 
     &                           "paxplg", paxplg,
     &                           "algorithm", "dbfpfit",
     &                           "auth", auth, 0)
     
      else 
c fplane table exists

        write(expr, 134), iorin
134     format("orid == ", i10)
      call dbsubset ( dbtmp, dbfp, expr, 0 )
c      print*,'subset dbtmp for orid = ', iorin,' done'
      call dbquery (dbtmp, dbRECORD_COUNT, nrec)
      
        if ( nrec. eq. 0 ) then
c        print*,'row doesnt exist'
c row for this particular event does not exist in fplane table
c find last mechid
        write(expr , 133)
133     format("mechid")
        call strtbl (keys, expr, 0)
        call dbsort (dbtmp, dbfp, keys, 0, "")
        call dbquery (dbtmp, dbRECORD_COUNT, nfp)
c        print*,'nfp = ',nfp
        dbtmp(4) = nfp-1
        jdb = dbgetv( dbtmp, 0, "mechid", lastmechid, 0)
        
        mechid = j+lastmechid+1
c        print*,'mechid = ',mechid,' lastmechid =',lastmechid
        jdb = dbaddv(db, "fplane","orid", iorin, "mechid", mechid,
     &                           "str1", dbstr1,
     &                           "dip1", dbdip1,
     &                           "rake1", dbrake1,
     &                           "str2", dbstr2, 
     &                           "dip2", dbdip2, 
     &                           "rake2", dbrake2, 
     &                           "taxazm", taxazm, 
     &                           "taxplg", taxplg,
     &                           "paxazm", paxazm, 
     &                           "paxplg", paxplg,
     &                           "algorithm", "dbfpfit",
     &                           "auth", auth, 0)
     
        else 
c row for this particular event exists in fplane table
c        print*,'row exists'
       
        call dblookup(dbfp, db, 0, "fplane", 0, 0)
        call dbquery (dbfp, dbRECORD_COUNT, nfp)
        
        do i = 0,nfp-1
          dbfp(4) = i
          jdb = dbgetv( dbfp, 0, "orid", orid, 0)
          if (orid.eq.iorin) then
          jdb = dbputv(dbfp, "","str1", dbstr1,
     &                           "dip1", dbdip1,
     &                           "rake1", dbrake1,
     &                           "str2", dbstr2, 
     &                           "dip2", dbdip2, 
     &                           "rake2", dbrake2, 
     &                           "taxazm", taxazm, 
     &                           "taxplg", taxplg,
     &                           "paxazm", paxazm, 
     &                           "paxplg", paxplg,
     &                           "algorithm", "dbfpfit",
     &                           "auth", auth, 0)
            endif
          enddo
        
        endif
c for table and record exists        
      endif
c for table exists
      endif  
c for not composite    
c
c recompute moment tensor representation for best solution to check
c for polarity discrepancies
c
           print *, 'recompute moment tensor for best solution'
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
c      print *, 'print out best solutions'
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
c           print *, 'accumulate statistics on fit parameter dist.'
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

20    enddo

c end loop for nor events in the database
      
99    continue
      
      stop
      end
