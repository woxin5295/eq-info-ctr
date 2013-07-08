      subroutine search3(botmn, botmx, chart, coef, fitmx, fitmn, 
     * iamwt, npax, mmn, mmx, mxcase, mxstat, nr, pobs, tm, uniq, wtobs)
c
c loop over focal mechanism space represented by one p axis, find
c the largest and smallest value of fit and return best fit indices.
c   in case of tie fit, choose fit with largest "bot".  
c    
c
      real         botmn          ! (output) largest bot for solutions with 
c                                        fit=fitmn (ie. ties)
      real         botmx          ! (output) largest bot for solutions with 
c                                        fit=fitmx (ie. ties)
      real         bot            ! the larger the value, the 
c                                        further data is from nodal planes
      real         coef(6, mxstat) ! (input) coefficients by which tm multiplied to give p radiation pattern
      character*80 chart(mxcase)  ! (output) chart of station reversals
      real         fit            ! fit of current trial solution
      real         fitmx          ! (output) weighted measure of agreement
c                                         between obs, pred polarities (maximum)
      real         fitmn          ! (output) weighted measure of agreement
c                                         between obs, pred polarities (minimum)
      integer      iamwt          ! (input) flag controling amplitude 
c                                        weighting (0=omit, 1=use)
      integer      npax           ! (input) number of t axes to try out
      integer      m              ! loop index over cases
      integer      mmn            ! (output) value of m for fitmn
      integer      mmx            ! (output) value of m for fitmx
      integer      mxcase         ! (input) maximum # of focal mechanisms 
c                                        to try
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
      real         tm(6, mxcase)  ! (input) moment tensor in upper triangular 
c                                        symetric storage mode
      real         top            ! sum of weighted difference of 
c                                        predicted, obs. polarities; 0<= top <=1
      logical      uniq           ! (input) if true, then identify unique
c                                        reversal patterns.
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
c     print *, 'm,      bot,     fit,     fitmx,      mmx,     fitmn,',
c    *'      mmn'
      do 50 m = 1, npax
            top = 0.
            bot = 0.
            top1 = 0.
            bot1 = 0.
            chart(m) = ' '
            do 20 i = 1, nr
              prad = 0
              do 10 k = 1, 6
                prad = prad + tm(k, m)*coef(k, i)
10            continue
c
c select amplitude weighting and calculate fit function for this model
c
              pth = sign(0.5, prad)
              wtth = sqrt(abs(prad))
              if (iamwt .eq. 1) 
     *          top = top + abs(pobs(i) - pth)*wtobs(i)*wtth
              bot = bot + wtobs(i)*wtth
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
              fit = top/bot
c
c do not use amplitude weighting
            else if (iamwt .eq. 0) then
              fit = top1/bot1
            end if
c
            if (fit .lt. fitmn) then
              fitmn = fit
              botmn = bot
              mmn = m
            else if ( (fit .eq. fitmn) .and. (bot .gt. botmn) )
     *      then
c             for tie solutions, find solution with most stations away 
c             from nodes
              mmn = m
              botmn = bot
            endif
            if (fit .gt. fitmx) then
              fitmx = fit
              botmx = bot
              mmx = m
            else if ( (fit .eq. fitmx) .and. (bot .gt. botmx) ) 
     *      then
c             for tie solutions, find solution with most stations away 
c             from nodes
              mmx = m
              botmx = bot              
            endif
c     print '(i3, 3f10.4, i3, f10.4, i3)',  
c    *   m, bot, fit, fitmx, mmx, fitmn, mmn      
50    continue
c
      return
      end
c
c
c
c
c

