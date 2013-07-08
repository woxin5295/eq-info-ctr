      subroutine ctop( bot, coef, mxstat, nr, pobs, tm, top, 
     1                top1, wtobs)
c
c compute top, top1, and bot
c calculate radiation pattern for model (eqtn 4.91, aki & richards, pg. 118)
c
      real    bot             ! (output) sum of product of observed and predicted weights
      real    coef(6, mxstat) ! (input) coefficients by which tm multiplied to give p radiation pattern
      integer mxstat          ! (input) maximum # of stations permitted
      integer nr              ! (input) -1=eof, 0=nr<minobs, nr>0 => number of stations 
      real    pobs(mxstat)    ! (input) observed first motion polarities; .5=compression, -.5=dilatation
      real    prad            ! radiation amplitude corresponding ain, phi. 
                              ! (dilatation) -1.<prad<+1.(compression)
      real    pth             ! predicted first motion polarity; same convention as for pobs
      real    top             ! (output) sum of weighted difference of 
c                               predicted, obs. polarities; 0<= top <=1
c                               using radiation weighting
      real    tm(6)           ! moment tensor in upper triangular symetric storage mode
      real    top1            ! (output) same as top, but no radiation weighting
      real    wtobs(mxstat)   ! (input) observed first motions weights
      real    wtth            ! predicted first motions weights
c
            top = 0
            bot = 0
            top1 = 0.
            do 20 i = 1, nr
              prad = 0
              do 18 k = 1, 6
                prad = prad + tm(k)*coef(k, i)
18            continue
c
c select amplitude weighting and calculate fit function for this model
c
              pth = sign(0.5, prad)
              wtth = sqrt(abs(prad))
              top = top + abs(pobs(i) - pth)*wtobs(i)*wtth
              bot = bot + wtobs(i)*wtth
              top1 = top1 + abs(pobs(i) - pth)*wtobs(i)
20          continue
      return
      end

