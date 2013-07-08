      subroutine fsearch2 (bstbot, coef, ddel, dlam,
     & dphi, fitmin, iamwt, idip1, dip2, idpdr1, dpdr2, islip1, slip2,
     & mxstat, ndel, nlam, nphi, nr,
     & pobs, rad, wtobs, punit)
c
c loop over the fine search mechanism space, compute fit parameter for each 
c solution, and return best fit indices.
c in case of tie fit, choose fit with largest "bot".  
c
      real    bot             ! sum of product of observed and predicted weights
      real    coef(6, mxstat) ! (input) coefficients by which tm multiplied to give p radiation pattern
      real    ddel            ! (input) fault dip increment in degrees 
      real    dlam            ! (input) fault slip increment in degrees 
      real    dphi            ! (input) fault dip direction increment in degrees 
      real    fit             ! weighted measure of agreement between obs, pred polarities
      real    fitmin          ! (output) fit of best solution corresponding to fit(j1, n1, m1)
      integer iamwt           ! (input) flag controling amplitude weighting (0=omit, 1=use)
      integer idip1           ! (input) initial fault dip angle in degrees
      real    dip2            ! (output) fault dip 
      integer idpdr1          ! (input) initial fault dip direction angle in degrees
      real    dpdr2           ! (output) fault dip direction in degrees
      integer islip1          ! (input) initial fault slip angle in degrees
      real    slip2           ! (output) fault slip angle in degrees
      integer j1              ! dip index of best solution
      integer m1              ! slip index of best solution
      integer mxstat          ! (input) maximum # of stations permitted
      integer n1              ! dip direction index of best solution
      integer ndel            ! (input) number of fault dip increments 
      integer nlam            ! (input) number of fault slip increments 
      integer nnover          ! number of extensions to search range
      logical nover           ! true if solution is on the edge
      integer nphi            ! (input) number of fault dip direction 
c                                         increments 
      integer nr              ! (input) -1=eof, 0=nr<minobs, nr>0 => number of stations 
      real    pobs(mxstat)    ! (input) observed first motion polarities; .5=compression, -.5=dilatation
      integer punit           ! (input) unit # for ray file
      real    rad             ! (input) conversion from degrees to radians
      real    wtobs(mxstat)   ! (input) observed first motions weights
c
      real    bstbot          ! largest bot for solutions with 
c                                         fit=fitmin (ie. ties)
      real    dip             ! fault dip angle in radians
      integer j               ! loop index over dip
      integer m               ! loop index over slip
      integer n               ! loop index of dip direction
      real    slip            ! fault slip angle in radians
      real    dpdir           ! fault dip direction angle in radians
      real    tm(6)           ! moment tensor in upper triangular symetric storage mode
      real    top             ! sum of weighted difference of 
c                               predicted, obs. polarities; 0<= top <=1
c                               using radiation weighting
      real    top1            ! same as top, but no radiation weighting
c
c begin code
      pi = 180.*rad
      nover = .false.
      nnover = 0
c
c compute first value for dpdir, dip, and slip
c     print *, 'fsearch2. Starting with dipdr, dip, slip: ', 
c    * idpdr1, idip1, islip1
c
c dip direction 
      phi0 = idpdr1 - float(nphi/2)*dphi
c dip
      del0 = idip1 - float(ndel/2)*ddel
c slip
      xlam0 = islip1 - float(nlam/2)*dlam
c
c calculate bot1
c
      bot1 = 0.
      do 13 i = 1, nr
        bot1 = bot1 + wtobs(i)
13    continue
c
15    bstbot = 0.
      fitmin = 2.0

c     print *, 'dip dir  ',  phi0, ' to ',  phi0 + nphi*dphi
c     print *, 'dip      ',  del0, ' to ',  del0 + ndel*ddel
c     print *, 'slip     ', xlam0, ' to ', xlam0 + nlam*dlam

      do 50 m = 1, nlam
c       slip
        slip = (xlam0 + (m - 1)*dlam)*rad
        do 40 n = 1, nphi
c         dip direction
          dpdir = (phi0 + (n - 1) * dphi)*rad
          do 30 j = 1, ndel
c           dip
            dip = (del0 + (j - 1) * ddel)*rad

c
c calculate moment tensor representation of shear fault (postive up, south, east)
c
            call shrflt1 (dpdir, dip, slip, tm)
c
c calculate bot, top, top1
            call ctop( bot, coef, mxstat, nr, pobs, tm, top, 
     1                top1, wtobs)
c
            if (iamwt .eq. 0) then
c
c do not use amplitude weighting
              fit = top1/bot1
            else 
c
c use amplitude weighting
              fit = top/bot
            end if
c
c     write(34, '(3i4, 6f10.3)') 
c    1 n,j,m,dpdir/rad,dip/rad,slip/rad,top1,bot,fit
            if (fit .lt. fitmin) then
              fitmin = fit
              bstbot = bot
              j1 = j
              n1 = n
              m1 = m
            else if( (fit .eq. fitmin) .and. (bot .gt. bstbot) ) then
c             for tie solutions, find solution with most stations away 
c             from nodes
              bstbot = bot
              j1 = j
              n1 = n
              m1 = m
            endif
30        continue
40      continue
50    continue
c
c
c now we have the j1, n1, and m1 index to the best solution
c slip
      slip2 = (xlam0 + (m1 - 1)*dlam)
c dip direction
      dpdr2 = (phi0 + (n1 - 1) * dphi) 
c dip
      dip2 = (del0 + (j1 - 1) * ddel)

c     print *, 'best dip dir', dpdr2
c     print *, 'best dip    ', dip2
c     print *, 'best slip   ', slip2

      if (slip2 .gt. 180.) slip2 = slip2 - 360.
      if (dip2 .gt. 90.) then
        dip2 = 180. - dip2
        slip2 = -slip2
        dpdr2 = dpdr2 + 180.
      endif
      if (dip2 .lt. 0) then
        dpdr2 = dpdr2 + 180.
        dip2 = -dip2
        slip2 = slip2 + 180.
        if (slip2 .gt. 180.) slip2 = slip2 - 360.
      endif
      if (dpdr2 .gt. 360.) then
        dpdr2 = dpdr2 - 360.
      endif
      if (dpdr2 .lt. 0.) then
	dpdr2 = dpdr2 + 360.
      endif
c     print *, 'after conversion:'
c     print *, 'best dip dir', dpdr2
c     print *, 'best dip    ', dip2
c     print *, 'best slip   ', slip2

c
c watch for best solution at limit of fine search range
c    
      if (nlam .lt. 2 .and. nphi .lt. 2 .and. ndel .lt. 2) goto 95
c        
      if (m1 .eq. 1) then 
        print *, '** warning ** solution is at the lower limit ',
     *           'of fine search rake range.'
        nover = .true.
      else if (m1 .eq. nlam) then
        print *, '** warning ** solution is at the upper limit ',
     *           'of fine search rake range.'
        nover = .true.
      endif
c
      if (n1 .eq. 1) then
        print *, '** warning ** solution is at the lower limit ',
     *           'of fine search dip direction.'
        nover = .true.
      else if (n1 .eq. nphi) then
        print *, '** warning ** solution is at the upper limit ',
     *           'of fine search dip direction.'
        nover = .true.
      endif 
c
      if (j1 .eq. 1) then
        print *, '** warning ** solution is at the lower limit ',
     *           'of fine search dip.'
        nover = .true.
      else if (j1 .eq. ndel) then
        print *, '** warning ** solution is at the upper limit ',
     *           'of fine search dip.'
        nover = .true.
      endif    
      if ( nover ) then
	nnover = nnover + 1
        if (nnover .gt. 5) then
          print *, 'extensions of fine search range not sufficient!'
          print *, '** solution still at limit of search range **'
          write(punit, 94)
94        format(' c* solution is at limit of search range.')
        else 
          print *, '#', nnover, ' extension of fine search range.'
c         new starting dip direction 
          phi0 = dpdr2 - float(nphi/2)*dphi
c         new starting dip
          del0 = dip2 - float(ndel/2)*ddel
c         new starting slip
          xlam0 = slip2 - float(nlam/2)*dlam
          nover = .false.
          print *, 'new starting dip dir, dip, strike = ',
     *      phi0, del0, xlam0
          goto 15
        endif
      endif
95    continue
      return
      end
c
