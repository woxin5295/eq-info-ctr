      subroutine rdeq3 (ain, az, cmpsit, cmpsum, date, dist, distmx, 
     & erate, eunit, event, fmagmn, ijeff, iunit, jjwt,kilsta, maxobs, 
     & minobs, 
     & munit, mxqual, mxstat, nkil, nr, nrev, nrwt, plusert, pobs, 
     & prcntx, presmx,prmk, revsta, sigmaf, stn, sumwt,
     & wterat, wtkey, wtlim, wtmax, wtobs, xainmn, xainmx, xcode)
c
c reads hypoellipse archive file with corrected first motions in col   .
c
c note: for fm = + or -, the reading weight code is increased by 1.
c
c reads hypoellipse archive file.  returns summary card and corresponding 
c  phase first motions, qualitites, angles of incidence, station names, 
c  and azimuths.  calculates standard deviation (sigmaf) of fit from estimated 
c  standard deviations of the data.  the estimated data errors are 
c  control-file inputs; corresponding data weights are calculated in main 
c  and passed to this routine in the parameter "weight".
c
      character*4       dnstrng, lshft
      real              ain(mxstat)  ! (output) ray incidence angles 
      real              az(mxstat)   ! (output) ray azimuth angles (corresponding to ain)
      logical           cmpsit       ! (input) true for composite solutions of all data
      character*50      cmpsum       ! (input) logical unit # for eqs used in composite
      character*10      date(mxstat) ! (output) arrival time date yrmodyhrmn
      real              dist(mxstat) ! (output) epicentral distance
      real              distmx       ! (input) maximum permitted epicentral distance 
      real              erate(mxqual) ! (input) assumed weighted error rates for each data class
      integer           eunit        ! (input) logical unit # of output of error messages
      character*80      event        ! (output) summary card
      real              fmagmn       ! (input) minimum permitted magnitude
      integer           ijeff        ! (input) flag: 1(0)=do (not) use data weighted out by jeffrey's weighting
      integer           iunit        ! (input) logical unit # of hypoellipse archive file
      character*4       kilsta(mxstat) ! (input) ignored station names
      integer           maxobs       ! (input) maximum number of observations
      integer           minobs       ! (input) minimum number of observations required 
      integer           munit        ! (input) logical unit for writing events used in composite
      integer           mxqual       ! (input) maximum # of qualities permitted
      integer           mxstat       ! (input) maximum # of stations permitted
      integer           nkil         ! (input) number of ignored stations
      integer           nsofar       ! current number of first motions 
c                                      in composite solution 
      integer           nr           ! (output) -1=eof, 0=skip event, nr>0 => number of stations 
      integer           nrwt         ! (output) number used in fm
      integer           nrev         ! (input) number of reversed stations
      real              plusert      ! (input) error rate weight for + or -
      real              pobs(mxstat) ! (output) observed first motion polarities; .5=compression, -.5=dilatation
      real              prcntx       ! (output) % of stations that are machine picked
      real              presmx       ! (input) maximum permitted p-residual
      character*4       prmk(mxstat) ! (output) first motion description (eg. ipu0)
      character*4       revsta(mxstat) ! (input) reversed station names
      real              sigmaf       ! (output) calculated standard deviation of fit based on data errors
      character*4       stn(mxstat)  ! (output) station names
      real              sumwt        ! (output) sum of observed first motion weights
      real              wterat(mxqual) ! (input) error rates for alternate weighting
      character*4       wtkey         ! (input) key to alternate weights - dist, azim angi
      real              wtlim(mxqual) ! (input) limits of alternate variable
      real              wtmax         ! (output) maximum weight
      real              wtobs(mxstat) ! (output) observed first motions weights
      real              xainmn        ! (input) minimum ang of inc to exclude
      real              xainmx        ! (input) maximum ang of inc to exclude
      character*7       xcode(mxstat) ! (output) code giving reason for exclusion
c                                       k = kill station; i = angel of inc.;
c                                       a = azimuth; d = distance
c
      character*1       fm           ! first motion direction (u, d, +, -)
      real              fmag         ! event magnitude
      integer           inlast       ! number of first motions in last eq
      integer           ipwt         ! qualiity assigned to p arrival
      integer           j            ! dummy loop index
      character*2       jfrywt       ! flag: '**' denotes p arrival time unreliable by jeffreys' weighting
      integer           jwt          ! index for data weight
      integer           jjwt(mxstat) ! index for data weight 
      integer           k            ! counter of good phase readings
      integer           kwt          ! number of phases to be used in current eq
      character*106     line         ! line of hypoellipse archive file
      character*1       m            ! test for fortran carriage control
      integer           nclas(20)    ! number of observations in each data class
      character*4       test         ! 2nd-6th characters of line of hypo71 output
      real              varf         ! calculated variance in fit based on data errors.
c
c     print *, 'distmx, fmagmn, ijeff, iunit, minobs, maxobs'
c     print *, 'munit, mxqual, mxstat, nkil, nrev, cmpsum'
c     print *, distmx, fmagmn, ijeff, iunit, minobs, maxobs
c     print *, munit, mxqual, mxstat, nkil, nrev, cmpsum
      wtmax = 0.
      prcntx = 0.
      sumwt = 0.
11    k = 1

c
c reset values
c
        do 115 i = 1, 4
        nclas(i) = 0
115   continue
c
        nused = 0
        nsum = 0
c
c find next summary record with fmag .ge. fmagmn
c
12      read (iunit, 13, end = 1000) line
13      format (a)
c       print *, 'sum ? ', line
        if (dnstrng(line(1:2)) .eq. 'c*') goto 12
        if (dnstrng(line(17:17)) .ne. 'n') goto 12
135     read (line, 14) fmag
14      format (34x, f2.1)
        if(fmag .lt. fmagmn) goto 12
        nsum = nsum + 1
        nused = nused + 1
        event = line
        kwt = 0
c     
c read next input record (this should not be a summary record)
50      read (iunit, 13, end = 65) line
c       print *, 'phase ? ', line
c get station name
        if(dnstrng(line(1:2)) .eq. 'c*') goto 50
        stn(k) = dnstrng(line(1:4))
	stn(k) = lshft(stn(k))
	if((stn(k)(4:4) .eq. 'n') .or.
     *     (stn(k)(4:4) .eq. 'e')) goto 50
c skip comment records
        if(line(1:4) .eq. '    ') then
c
c         found instruction records (end of event for non-composite)
c
55        if(.not. cmpsit) goto 70
          inlast = (k - 1) - nsofar
c         print *, 'this event has ', inlast, 'first motions.'
c         print *, 'of these ', kwt, ' will be used.'
          if( kwt .lt. minobs .or. kwt .gt. maxobs) then
            k = k - inlast
            nused = nused - 1
          else if(dnstrng(cmpsum(1:4)) .ne. 'none') then
            write (munit, 13) event
          endif
          nsofar = k - 1
c         print *, 'found instruction record for event:'
c         print *, event
c         print *, 'nsofar = ', nsofar, ' nused = ', nused,
c    &    'nsum = ', nsum
c         go get next summary record
          goto 12
        endif
c
c process out of order summary records
        if (dnstrng(line(17:17)) .eq. 'n') then
          write (eunit, *) '***** rdeq3 error: found summary record prio
     &r to instruction record'
          write (eunit, *) ' this event will be skipped: ', line
          if(cmpsit) then
            goto 135
          else
            write (eunit, *) ' this event will be skipped: ', line
            goto 70
          endif 
        endif
        goto 80
c
c unexpected end of input file
c
65      write (eunit, *) 'found end of file prior prior to expected inst
     &ruction record.'
c       treat this case as if instruction record were found instead.
        goto 55
c
c end of non-composit event
c
70      continue
c       print *, 'read ', k-1, ' first motions from ', nused,
c    &  ' events.'
        if (k - 1 .ge. minobs) then
          nr = k - 1
        else
          nr = 0
        end if
        goto 1001
c
c ignore this station?
c
80      xcode(k) = '    '
        errat = 0.
        if (nkil .gt. 0) then
          do 81 i = 1, nkil
            if (stn(k) .eq. kilsta(i)) then
              xcode(k)(1:1) = 'k'
              errat = .5
              goto 82
            endif
81        continue
        end if
c
c  so far, so good: now check phase card for polarity, distance, 
c     and quality
c     print *, 'so far, so good'
c
c       read p-residual rather than jfrywt
c       print *, 'k = ', k
c       print *, line
82      read (line, 83, err=84) prmk(k),   date(k),  dist(k), az(k),
     *     ain(k),      fm,      pres
83      format (                   4x, a4,   1x, a10, 5x, f4.1,  f3.0,
     *   9x, f3.0, 21x, a1, 10x, f5.2)
        goto 85
84      print *, 'decode error with following line:'
        print *, line
        print *, 'format(4x, a4, 16x, f4.1,  f3.0, 9x, f3.0, 21x, a1,10
     *x, f5.2)'
        goto 50
85      prmk(k)(3:3) = fm
c       print *, 'prmk(k), dist(k), az(k) ', prmk(k), dist(k), az(k)
c       print *, 'ain(k), fm, pres ', ain(k), fm, pres

c       print *, 'check prmk (', prmk(k), ') for p or blank'
        if ( (dnstrng(prmk(k)(2:2)) .ne. 'p') .and. 
     &       (dnstrng(prmk(k)(2:2)) .ne. 'v') .and.
     &       (dnstrng(prmk(k)(2:2)) .ne. 'z') .and.
     &       (prmk(k)(2:2) .ne. ' ') .and. 
     *       (dnstrng(prmk(k)(2:2)) .ne. 'P') .and.
     *       (dnstrng(prmk(k)(2:2)) .ne. 'V') .and.
     &       (dnstrng(prmk(k)(2:2)) .ne. 'Z'))  goto 50
c
c       print *, 'check first motion symbol'
        if (dnstrng(fm) .eq. 'c') fm = 'u'
        if (dnstrng(fm) .ne. 'u' .and. 
     &      dnstrng(fm) .ne. 'd' .and. 
     &      fm .ne. '+' .and.
     &      fm .ne. '-') goto 50
c
c       print *, 'check that disk(k) .le. ', distmx
        if (dist(k) .gt. distmx) then
          xcode(k)(4:4) = 'd'
          errat = .5
        endif
c
c       print *, 'skip if angle of inc is within range ', xainmn, xainmx
        if((ain(k) - xainmn)*(ain(k) - xainmx) .le. 0.) then
          xcode(k)(2:2) = 'i'
          errat = .5
        endif
c
c       print *, 'skip if pres (',pres,') is .gt. presmx'
        if (abs(pres) .gt. presmx) then
          xcode(k)(5:5) = 'r'
          errat = .5
        endif
c
c       print *, 'take care of weight code'
        read (prmk(k), '(3x, i1)') ipwt
c       print *, 'ipwt = ', ipwt
        jwt = ipwt + 1
c       print *, 'skip if jwt (', jwt,') is .ge. 5'
        if (jwt .ge. 5) jwt = 5
        if ((jwt .eq. 5) .or. (erate(jwt) .ge. 0.5)) then
          xcode(k)(6:6) = 'w'
          errat = .5
        else
          errat = errat + erate(jwt) - 2.*errat*erate(jwt)
        endif
        jjwt(k) = jwt
c
c       print *, take care of + and - first motions
        if (fm .eq. '+') then
          errat = errat + plusert - 2.*errat*plusert
          if (plusert .ge. 0.5) then
            xcode(k)(7:7) = '+'
            errat = 0.5
          endif
        else if(fm .eq. '-') then
          errat = errat + plusert - 2.*errat*plusert
          if (plusert .ge. 0.5) then
            xcode(k)(7:7) = '+'
            errat = 0.5
          endif
        endif
c
c take care of alternate weighting 
c
        if (dnstrng(wtkey) .eq. 'dist') then
           if ( (dist(k) .ge. wtlim(1)) .and. 
     &          (dist(k) .lt. wtlim(2)) ) then
             jwt = 1
           else if ( (dist(k) .ge. wtlim(2)) .and. 
     &               (dist(k) .lt. wtlim(3)) ) then
             jwt = 2
           else if ( (dist(k) .ge. wtlim(3)) .and.
     &               (dist(k) .lt. wtlim(4)) ) then
             jwt = 3
           else if ( (dist(k) .ge. wtlim(4)) .and.
     &               (dist(k) .lt. wtlim(5)) ) then
             jwt = 4
           else
             xcode(k)(4:4) = 'd'
             errat = 0.5
             goto 89
           endif
        else if (dnstrng(wtkey) .eq. 'azim') then
           if ( (az(k) .ge. wtlim(1)) .and. 
     &          (az(k) .lt. wtlim(2)) ) then
             jwt = 1
           else if ( (az(k) .ge. wtlim(2)) .and. 
     &               (az(k) .lt. wtlim(3)) ) then
             jwt = 2
           else if ( (az(k) .ge. wtlim(3)) .and.
     &               (az(k) .lt. wtlim(4)) ) then
             jwt = 3
           else if ( (az(k) .ge. wtlim(4)) .and.
     &               (az(k) .lt. wtlim(5)) ) then
             jwt = 4
           else
             xcode(k)(3:3) = 'a'
             errat = 0.5
             goto 89
           endif
        else if (dnstrng(wtkey) .eq. 'angi') then
           if ( (ain(k) .ge. wtlim(1)) .and. 
     &          (ain(k) .lt. wtlim(2)) ) then
             jwt = 1
           else if ( (ain(k) .ge. wtlim(2)) .and. 
     &               (ain(k) .lt. wtlim(3)) ) then
             jwt = 2
           else if ( (ain(k) .ge. wtlim(3)) .and.
     &               (ain(k) .lt. wtlim(4)) ) then
             jwt = 3
           else if ( (ain(k) .ge. wtlim(4)) .and.
     &               (ain(k) .lt. wtlim(5)) ) then
             jwt = 4
           else
             xcode(k)(2:2) = 'i'
             errat = 0.5
             goto 89
           endif
        else
           goto 89
        endif
        errat = errat + wterat(jwt) - 2.*errat*wterat(jwt)
        if (wterat(jwt) .ge. 0.5) then
          if (dnstrng(wtkey) .eq. 'dist') then
            xcode(k)(4:4) = 'd'
            errat = 0.5
          else if (dnstrng(wtkey) .eq. 'azim') then
            xcode(k)(3:3) = 'a'
            errat = 0.5
          else if (dnstrng(wtkey) .eq. 'angi') then
            xcode(k)(2:2) = 'i'
            errat = 0.5
          endif
        endif
        jjwt(k) = jwt
89      if(errat .ge. 0.5) then
          wtobs(k) = 0.
        else
          wtobs(k) = 1./sqrt(errat - errat*errat) - 2.
          kwt = kwt + 1
        endif
c
c flip polariites if station is designated as reversed
c
        continue
        if(wtobs(k) .gt. wtmax) wtmax = wtobs(k)
c       print *, 'flip polarities'
        do 90 i = 1, nrev
          if (stn(k) .eq. revsta(i)) then
            if (dnstrng(fm) .eq. 'u') prmk(k)(3:3) = 'd'
            if (dnstrng(fm) .eq. 'd') prmk(k)(3:3) = 'u'
            if (fm .eq. '+') prmk(k)(3:3) = '-'
            if (fm .eq. '-') prmk(k)(3:3) = '+'
            fm = prmk(k)(3:3)
          end if
90      continue
c
        if (dnstrng(fm) .eq. 'u' .or. 
     &      fm .eq. '+') then
          pobs(k) = .5
        else
          pobs(k) = -.5
        end if
c
c increment k and check number against array dimensions
c
c       print *, 'increment k'
        k = k + 1
        if (k .gt. mxstat) then
          write (eunit, *) '***** readeq error: number of stations readi
     &ngs exceeds ', mxstat, 'for event:'
          write (eunit, *) '  ', line, '*****'
          nr = k - 1
          goto 1001
        end if
c
c read another phase
c
        goto 50
c
c end of file
c
1000  nr = -1
      if(cmpsit) then
        write (eunit, *) nsum, 'events passed magnitude limit.'
        write (eunit, *) 'used ', k-1, ' first motions from ', 
     &  nused, ' events with nobs from'
        write (eunit, *) minobs, ' to ', maxobs
        if (k - 1 .ge. minobs) then
          nr = k - 1
        endif
      endif
c
c finish up with the weights
c
1001  nrwt = 0
      if( nr .le. 0) return
      do 1002 k = 1, nr
        jwt = jjwt(k)
        nclas(jwt) = nclas(jwt) + 1
        if(wtobs(k) .ne. 0.) then
          sumwt = sumwt + wtobs(k)
          nrwt = nrwt + 1
        endif
1002  continue
      if(nrwt .eq. 0) then
c        print *, 'this will not work! all readings have zero weight.'
c        print *, 'name, xcode, prmk, dist, az, ain'
c        do 1010 k = 1, nr
c          print *, stn(k), xcode(k), prmk(k), dist(k), az(k), ain(k)
c1010    continue        
        nr = 0
        return
      endif
      sigmaf = sqrt( float(nrwt) )/sumwt
      return
      end
c
