      subroutine rdeq2 (ain, az, dist, distmx, eunit, event, fmagmn,
     & ijeff, iunit, kilsta, minobs, mxqual, mxstat, nkil, nr, nrev, 
     & pobs, prcntx, prmk, revsta, sigmaf, stn, sumwt, weight, wtobs)
c
c reads hypoinverse archive file. returns summary card and corresponding phase first motions, qualitites, angles of incidence,
c station names, and azimuths.  calculates standard deviation (sigmaf) of fit from estimated standard deviations of the data. 
c the estimated data errors are control-file inputs; corresponding data weights are calculated in main and passed to this 
c routine in the parameter "weight".
c
      character*1       dnstrng
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
      character*90      evline                          ! line for reading event summary
      character*1       fm                              ! first motion direction (u, d, +, -)
      real              fmag                            ! event magnitude
      integer           ipwt                            ! qualiity assigned to p arrival
      integer           j                               ! dummy loop index
      character*1       jfrywt                          ! flag: '*' denotes p arrival time unreliable by jeffreys' weighting
      integer           jwt                             ! index for data weight
      integer           k                               ! counter of good phase readings
      character*90      line                            ! line of hypoinverse station data 
      character*1       m                               ! test for fortran carriage control
      integer           nclas(20)                       ! number of observations in each data class
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
      evline = ' '
      prcntx = 0.
      sumwt = 0.
c
c read summary card (skip non-summary cards)
c
39      read (iunit, 40, end = 1000) evline
40      format (a90)
        ic = ichar (evline(1:1))
        if (ic .lt. 48 .or. ic .gt. 57) goto 39
c
c check magnitudes
c
        read (evline, '(t68, f2.1, t35, f2.1)') fmag, pmag
        if (pmag .eq. 0.) pmag = fmag
        if (fmag .ne. 0.) pmag = 0.5 * (pmag + fmag)
        if (pmag .lt. fmagmn) then
          nr = 0
          return
        end if
c
c reformat the summary record
c
      read (evline, 41, err=888) iymd, ihm, sec, lat, xlat, lon, xlon,
     1                  dep, nsp, igap, dmin, rms, erh, erz
41    format (i6, i4, f4.2, i2, 1x, f4.2, i3, 1x, f4.2, f5.2, 2x,
     1        i3, i3, f3.0, f4.2, 31x, 2f4.2)
      write (event, 42) iymd, ihm, sec, lat, xlat, lon, xlon, dep,
     1                  pmag, nsp, igap, dmin, rms, erh, erz
42    format (i6, 1x, i4, 1x, f5.2, i3, '-', f5.2, i4, '-', f5.2, 
     1        2f7.2, i3, i4, f5.1, f5.2, 2f5.1, 1x)
c
c get the phase data
c
        k = 1
50      stn(k) = '    '
        read (iunit, 60, err=777, end = 70) line
60      format (a)
        read (line, '(a4)') stn(k)
c
c check for end of phase data
c
70      if (stn(k) .eq. '    ') then
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
        read (line, 120) prmk(k), dist(k), ain(k), jfrywt, az(k)
120     format (4x, a4, t59, f4.1, f3.0, 5x, a1, 4x, f3.0)
        read (prmk(k), '(2x, a1, i1)') fm, ipwt
        if (dnstrng(fm) .ne. 'u' .and. dnstrng(fm) .ne. 'd' .and.
     *   fm .ne. '+' .and.
     *   fm .ne. '-') goto 50
        if (dist(k) .gt. distmx) goto 50
        if (ijeff .eq. 0 .and. jfrywt .eq. '*') goto 50
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
        if (dnstrng(fm) .eq. 'u' .or. fm .eq. '+') then
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
c
888   write (6, *) iymd, ihm, sec
      stop
c
777   write (6, *) iymd, ihm, sec, k
      stop
c
      end
c
c
c
c
c

