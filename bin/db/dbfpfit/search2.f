      subroutine search2 (bot2, botmn, botmx, chart, coef, fitmx, 
     *  fitmn, iamwt, ic, ntcase, mmn, mmx, mxcase, mxntc, mxstat, 
     *  nr, pobs, secinc, smnlow, smnhi, smxlow, smxhi, tmsv, uniq, 
     *  wtobs)
c coarse loop over focal mechanism space represented by one p axis to 
c find the largest and smallest value of fit and return range of t
c axes that will be used in 1 degree search.
c    
c
      real         bot2(mxcase)   ! the larger the value, the 
c                                        further data is from nodal planes
      real         botmn          ! bot for lowest fit
      real         botmx          ! bot for largest fit
      real         coef(6, mxstat) ! (input) coefficients by which tm multiplied to give p radiation pattern
      character*80 chart(mxcase)  ! (output) chart of station reversals
      logical      dntrans        ! .true. on transition to smaller fit value
      real         fit            ! fit of current trial solution
      real         fitmx          ! (output) weighted measure of agreement
c                                         between obs, pred polarities (maximum)
      real         fitmn          ! (output) weighted measure of agreement
c                                         between obs, pred polarities (minimum)
      logical      holemn        ! .true. if minimum values are not continuous
      logical      holemx         ! .true. if maximum values are not continuous
      integer      iamwt          ! (input) flag controling amplitude 
c                                        weighting (0=omit, 1=use)
      integer      ic             ! (input) index to current p axis
      integer      ntcase           ! (input) number of t axes to try out
      integer      m              ! loop index over cases
      integer      mxcase         ! (input) maximum # of focal mechanisms 
c                                        to try
      integer      mxntc          ! (input) max number of t axes per p 
      integer      mxstat         ! (input) maximum # of stations permitted
      integer      nr             ! (input) -1=eof, 0=nr<minobs, nr>0 => 
c                                        number of stations 
      real         pobs(mxstat)   ! (input) observed first motion polarities; .5=compression, -.5=dilatation
c
      real         prad           ! radiation amplitude corresponding ain, 
c                                        phi. ! (dilatation) -1.<prad<+1.
c                                        (compression)
      real         pth            ! predicted first motion polarity; same 
c                                        convention as for pobs
      real         secinc         ! (input) increment in t axis orientation
      real         smnlow         ! lower limit of sec for 2nd stage search
      real         smnhi          ! upper limit of sec for 2nd stage search
      real         smxlow         ! lower limit of sec for 2nd stage search
      real         smxhi          ! upper limit of sec for 2nd stage search
      real         tmsv(6, mxntc, mxcase) ! (input) moment tensor for 
c                                           first coarse search
      real         top            ! sum of weighted difference of 
c                                        predicted, obs. polarities; 0<= top <=1
      logical      uniq           ! (input) if true, then identify unique
c                                        reversal patterns.
      logical      uptrans        ! .true. on transition to larger fit value
      real         wtobs(mxstat)  ! (input) observed first motions weights
      real         wtth           ! first motions weight based on amplitude
c
c begin code
c
      fitmn = 2.0
      fitmx = -1.0
c
c calculate radiation pattern for each model (eqtn 4.91, aki & richards, pg. 118)
c assume the fixed axis is a p axis.  each case represent a different t axis.
c
c     print *, ' m       fit     fitmn m1 m2 m3 m4 holemn trans'
      do 50 m = 1, ntcase
c           print '(a, 6f10.2)', 'tmsv = ', (tmsv(k, m, ic), k = 1, 6)
            top = 0.
            bot2(m) = 0.
            top1 = 0.
            bot1 = 0.
            chart(m) = ' '
            do 20 i = 1, nr
              prad = 0
              do 10 k = 1, 6
                prad = prad + tmsv(k, m, ic)*coef(k, i)
10            continue
c
c select amplitude weighting and calculate fit function for this model
c
              pth = sign(0.5, prad)
              wtth = sqrt(abs(prad))
              if (iamwt .eq. 1) 
     *          top = top + abs(pobs(i) - pth)*wtobs(i)*wtth
              bot2(m) = bot2(m) + wtobs(i)*wtth
              top1 = top1 + abs(pobs(i) - pth)*wtobs(i)
              bot1 = bot1 + wtobs(i)
c             chart out which stations are reversed
              if ((uniq) .and. (i .lt. 81)) then
                if(pobs(i) .eq. pth) then
                  chart(m)(i:i) = '0'
                else
                  chart(m)(i:i) = '1'
                endif
              endif
20          continue
c
c use amplitude weighting
            if (iamwt .eq. 1) then
              fit = top/bot2(m)
c
c do not use amplitude weighting
            else if (iamwt .eq. 0) then
              fit = top1/bot1
            end if
c
            if (m .eq. 1) then
              minmn1 = 0
              minmx1 = 0
              maxmn1 = 0
              maxmx1 = 0
              fitmn = fit
              minmn = m
              maxmn = m
              nmin = 1
              uptrans = .false.
              holemn = .false.
              fitmx = fit
              minmx = m
              maxmx = m
              nmax = 1
              dntrans = .false.
              holemx = .false.
              fitm1 = fit
              goto 50
            endif
            if (fit .gt. fitm1) uptrans = .true.
            if (fit .lt. fitm1) dntrans = .true.
            if (fit .lt. fitmn) then
              uptrans = .false.
              holemn = .false.
              fitmn = fit
              minmn = m
              maxmn = m
              nmin = 1
            else if (fit .eq. fitmn) then
              nmin = nmin + 1
              if (holemn) then
c               define end of second low (assume one hole max)
                maxmn1 = m
              else if (uptrans) then
c               this minimum is not continuous
c               first low after a hole
                holemn = .true.
                minmn1 = m
                maxmn1 = m
              else
                maxmn = m
              endif
            endif
            if (fit .gt. fitmx) then
              dntrans = .false.
              holemx = .false.
              fitmx = fit
              minmx = m
              maxmx = m
              nmax = 1
            else if (fit .eq. fitmx) then
              nmax = nmax + 1
              if (holemx) then
c               define end of second low (assume one hole max)
                maxmx1 = m
              else if (dntrans) then
c               this minimum is not continuous
c               first low after a hole
                holemx = .true.
                minmx1 = m
                maxmx1 = m
              else
                maxmx = m
              endif
            endif
            fitm1 = fit
c     print '(i3, 2f10.4, 4i3, 3i3 )',  
c    *   m, fit, fitmn, minmn, maxmn, minmn1, maxmn1, holemn, uptrans,
c    *   nmin
c     print '(i3, 2f10.4, 4i3, 3i3 )',  
c    *   m, fit, fitmx, minmx, maxmx, minmx1, maxmx1, holemx, dntrans,
c    *   nmax
50    continue
c
c find the best value for smnlow and smnhi
c
      if (holemn) then
c       print *, 'there is a hole in minimum data'
        if ( (minmn .eq. 1) .and. (maxmn1 .eq. ntcase) ) then
          j = minmn1 - ntcase 
          ammn = (maxmn + minmn1)/2. + ntcase/2.
          mmn = ammn
          if(mmn .gt. ntcase) mmn = mmn - ntcase
          smnlow = (ammn - 19.)*secinc - 10.
          smnhi = smnlow + 20.
        else
c         print *, 'found unexpected hole in minimum data'
          smnlow = (minmn - 19.)*secinc - 5.
          smnhi = (maxmn1 - 19.)*secinc + 5.
          mmn = (minmn + maxmn1)/2.
        endif   
      else if (nmin .eq. ntcase) then
c       in this case they were all equal
        smnlow = -180.
        smnhi = -1.
        mmn = 1
      else
        ammn = (maxmn + minmn)/2. 
        mmn = ammn
        smnlow = (ammn - 19.)*secinc - 10.
        smnhi = smnlow + 20.
      endif
      botmn = bot2(mmn)
c
c find the best value for ammx
c
      if (holemx) then
c       print *, 'there is a hole in maximum data'
        if ( (minmx .eq. 1) .and. (maxmx1 .eq. ntcase) ) then
c         print *, 'this is a normal hole, minmx1, maxmx, ntcase,  ',
c    *    minmx1, maxmx, ntcase    
          ammx = (maxmx + minmx1)/2. + ntcase/2.
          mmx = ammx
          if(mmx .gt. ntcase) mmx = mmx - ntcase
          smxlow = (ammx - 19.)*secinc - 10.
          smxhi = smxlow + 20.
        else
c         print *, 'found unexpected hole in maximum data'
          smxlow = (minmx - 19.)*secinc - 5.
          smxhi = (maxmx1 - 19.)*secinc + 5.
          mmx = (minmx + maxmx1)/2.
        endif   
      else if (nmin .eq. ntcase) then
c       print *, 'in this case all maximum data are equal'
        smxlow = -180.
        smxhi = -1.
        mmx = 1
      else
c       print *, 'normal case for maximum data'
        ammx = (maxmx + minmx)/2. 
        mmx = ammx
        smxlow = (ammx - 19.)*secinc - 10.
        smxhi = smxlow + 20.
      endif
      botmx = bot2(mmx)
      return
      end
c
c
c
c
c

