      subroutine readeq (ain, az, dist, distmx, eunit, event, fmagmn,
     & ijeff, iunit, kilsta, minobs, mxqual, mxstat, nkil, nr, nrev, 
     & pobs, prcntx, prmk, revsta, sigmaf, stn, sumwt, weight, wtobs)
c
c reads hypo71 output listing. returns summary card and corresponding phase first motions, qualitites, angles of incidence,
c station names, and azimuths.  calculates standard deviation (sigmaf) of fit from estimated standard deviations of the data. 
c the estimated data errors are control-file inputs; corresponding data weights are calculated in main and passed to this 
c routine in the parameter "weight".
c
      character*4       dnstrng
      real              ain(mxstat)                     ! (output) ray incidence angles 
      real              az(mxstat)                      ! (output) ray azimuth angles (corresponding to ain)
      real              dist(mxstat)                    ! (output) epicentral distance
      real              distmx                          ! (input) maximum permitted epicentral distance 
      integer           eunit                           ! (input) logical unit # of output of error messages
      character*80      event                           ! (output) summary card
      real              fmagmn                          ! (input) minimum permitted magnitude
      integer           ijeff                           ! (input) flag: 1(0)=do (not) use data weighted out by jeffrey's weighting
      integer           iunit                           ! (input) logical unit # of hypo71 listing file (input file)
      character*4       kilsta(mxstat)                  ! (input) ignored station names
      integer           minobs                          ! (input) minimum number of observations required 
      integer           mxqual                          ! (input) maximum # of qualities permitted
      integer           mxstat                          ! (input) maximum # of stations permitted
      integer           nkil                            ! (input) number of ignored stations
      integer           nr                              ! (output) -1=eof, 0=skip event, nr>0 => number of stations 
      integer           nrev                            ! (input) number of reversed stations
      real              pobs(mxstat)                    ! (output) observed first motion polarities; .5=compression, -.5=dilatation
      real              prcntx                          ! (output) % of stations that are machine picked
      character*4       prmk(mxstat)                    ! (output) first motion description (eg. ipu0)
      character*4       revsta(mxstat)                  ! (input) reversed station names
      real              sigmaf                          ! (output) calculated standard deviation of fit based on data errors
      character*4       stn(mxstat)                     ! (output) station names
      real              sumwt                           ! (output) sum of observed first motion weights
      real              weight(mxqual)                  ! (input) weights associated with qualities
      real              wtobs(mxstat)                   ! (input) observed first motions weights
c
      character*1       fm                              ! first motion direction (u, d, +, -)
      real              fmag                            ! event magnitude
      integer           ipwt                            ! qualiity assigned to p arrival
      integer           j                               ! dummy loop index
      character*2       jfrywt                          ! flag: '**' denotes p arrival time unreliable by jeffreys' weighting
      integer           jwt                             ! index for data weight
      integer           k                               ! counter of good phase readings
      character*80      line                            ! line of hypo71 station output 
      character*1       m                               ! test for fortran carriage control
      integer           nclas(20)                       ! number of observations in each data class
      character*80      temp                            ! temporary line for rearranging event format
      character*4       test                            ! 2nd-6th characters of line of hypo71 output
      real              varf                            ! calculated variance in fit based on data errors.
      real              wt                              ! weight assigned to p arrival
c
c reset values
c
      do 10 i = 1, mxqual
        nclas(i) = 0
10    continue
c
c find line prior to summary card
c
      prcntx = 0.
      sumwt = 0.
20    read (iunit, 30, end = 1000) test
30    format (2x, a4)
      if (dnstrng(test) .ne. 'date') goto 20
c
c read summary card
c
        read (iunit, 40, end = 1000) event
40      format (1x, a80)
        read (event, '(45x, f5.2)') fmag
c
c check magntitude
c
        if (fmag .lt. fmagmn) then
          nr = 0
          return
        end if
        read (iunit, 30, end = 1000) test
        read (iunit, 30, end = 1000) test
c
c check whether phase data or focal mechanism follow 
c
        if (dnstrng(test) .ne. 'stn ') goto 20
        k = 1
50      stn(k) = '    '
        read (iunit, 60, end = 70) line
60      format (a)
        read (line, '(a1, a4)') m, stn(k)
c
c check for end of phase data
c
70      if (m .eq. '1' .or. stn(k) .eq. '    ') then
c
c end of event
c
          if (k - 1 .ge. minobs) then
            nr = k - 1
            prcntx = prcntx/float(nr)
            varf1 = 0.
            varf2 = 0.
            do 80 jwt = 1, mxqual
              if (weight(jwt) .ge. .0001) then
                varf1 = varf1 + nclas(jwt)
                varf2 = varf2 + nclas(jwt)*weight(jwt)
              end if
80          continue
            varf  = varf1/(varf2*varf2)
            sigmaf= sqrt(varf)
          else
            nr = 0
          end if
c
c reformat event into standard hypo71 summary format
            temp = event(1:53)//event(57:60)//event(54:56)//'.0'//
     &      event(63:80)
            event = temp
          return
        end if
c
c check for repeated phase card
c
        if (k .gt. 2) then
          do 100 j = 1, k - 1
            if (stn(k) .eq. stn(j)) then
              write (eunit, 90) stn(k), event(1:18)
90            format (' ', '***** readeq error: ', a4,
     & ' has multiple phase cards for event:', a18, ' *****')
              goto 50
            end if
100       continue
        end if
c
c ignore this station?
c
        if (nkil .gt. 0) then
          do 110 i = 1, nkil
            if (stn(k) .eq. kilsta(i)) goto 50
110       continue
        end if
c
c  so far, so good: now check phase card for polarity, distance, jeffreys' weighting, quality
c
        read (line, 120) dist(k), az(k), ain(k), prmk(k), jfrywt
120     format (6x, f5.1, 1x, f3.0, 1x, f3.0, 1x, a4, 35x, a2)
        read (prmk(k), '(2x, a1, i1)') fm, ipwt
        if (dnstrng(fm) .ne. 'u' .and. dnstrng(fm) .ne. 'd' .and. 
     *      fm .ne. '+' .and.
     &      fm .ne. '-') goto 50
        if (dist(k) .gt. distmx) goto 50
        if (ijeff .eq. 0 .and. jfrywt .eq. '**') goto 50
        if (ipwt .ge. mxqual/2) then
          wt = 0.
        else if (dnstrng(prmk(k)(1:1)) .eq. 'x' .or. 
     *           dnstrng(prmk(k)(1:1)) .eq. 'y') then
          jwt = ipwt + mxqual/2 + 1
          wt = weight(jwt)
          if (wt .ne. 0.) prcntx = prcntx + 1.
        else
          jwt = ipwt + 1
          wt = weight(jwt)
        end if
        if (wt .eq. 0.) goto 50
c
c flip polariites if station is designated as reversed
c
        do 130 i = 1, nrev
          if (stn(k) .eq. revsta(i)) then
            if (dnstrng(fm) .eq. 'u') prmk(k)(3:3) = 'd'
            if (dnstrng(fm) .eq. 'd') prmk(k)(3:3) = 'u'
            if (fm .eq. '+') prmk(k)(3:3) = '-'
            if (fm .eq. '-') prmk(k)(3:3) = '+'
            fm = prmk(k)(3:3)
          end if
130     continue
c
        nclas(jwt) = nclas(jwt) + 1
        wtobs(k) = wt
        sumwt = sumwt + wt
        if (dnstrng(fm) .eq. 'u' .or. dnstrng(fm) .eq. '+') then
          pobs(k) = .5
        else
          pobs(k) = -.5
        end if
c
c increment k and check number against array dimensions
c
        k = k + 1
        if (k .gt. mxstat) then
          write (eunit, *) '***** readeq error: number of stations readi
     &ngs exceeds ', mxstat, 'for event:', event(1:18), ' *****'
          if (nr .gt. minobs) then
            nr = k - 1
            prcntx = prcntx/float(nr)
          else
            nr = 0
          end if
          return
        end if
c
c read another phase
c
        goto 50
c
c end of file
c
1000  nr = -1
      return
      end
c
c
c
c
c

