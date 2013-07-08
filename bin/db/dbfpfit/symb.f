      subroutine symb( sym, hite, x, y, dash, space, az)
c the first call to symb sets some initial values, including setting the
c number of user symbols to 0, but does not plot a symbol.
c make the first call to symb before calling usrsym, the sub. that 
c defines user symbols.
c call with hite<0 to reset number of user symbols to 0.
c

      real    ang             ! angle of symbol derived from sym value
      real    az              ! clockwise rotation of symbol (degrees)
      real    dash            ! dash length
      real    defact          ! fractional reduction in size prior to each fill sym
      logical frstim          ! true only the first time in this sub.
      data    frstim/.true./
      real    hite            ! height of symbol
      integer ibs             ! yz portion of symbol - npts*10 + ityp
      integer ifil            ! fill code - (0=no fill, 9=all filled)
      integer isk             ! index to root symbol array
      integer ikey(99)        ! ikey(yz) is the root symbol number that
c                               corresponds to symbol number yz
      data ikey/99*0/
      integer isym            ! xyz portion of symbol (integer part)
      integer ityp            ! type of symbol (0=reg poly, 1=star, 2=* pattern,
c                               3 or greater=user symbol)
      integer iup             ! calcomp plot control number
      integer nfil            ! number of additional symbols needed for fill
      integer npts            ! number of points on symbol
      integer nu              ! # of user symbols defined so far (20 max)
      real    rad             ! radians per degree
      data rad/.017453293/
      real    space           ! space length
      real    sym             ! plotting symbol xyz.abc
c                               x (ifil) is the fill code (0=no fill, 9=filled)
c                               y (npts) is the number of points in symbol 
c                               z (ityp) is the symbol style (0=reg poly, 
c                                                    1=star, 2=* pattern
c                               abc is the azimuth for rotation (090=90 deg)
      logical trace
      real    x               ! x coordinate of center of symbol
      real    xr(30, 20)      ! xr(i,j) is the j'th x value for root symbol # i
      real    xu(20, 200)     !  x(i,j) is the j'th x value for user symbol # i
      real    y               ! y coordinate of center of symbol
      real    yr(30, 20)      ! yr(i,j) is the j'th y value for root symbol # i
      real    yu(20, 200)     !  y(i,j) is the j'th y value for user symbol # i
c
      common/symb1/ xu, yu, nu, xr, yr, ikey
c
c	write(95,*) '---> called symb with:'
c	write(95,*) 'sym, hite, x, y, dash, space, az ='
c	write(95,*) sym, hite, x, y, dash, space, az
c
      trace = .false.                                   
      if(sym .lt. 0.) trace = .true.
      absym = abs(sym)
c      write(91,*) 'hite: ',hite
c
c read root symbols into arrays
      if (frstim) then
        nu = 0
        call rdrsym(xr, yr, ikey) 
        frstim = .false.
c	write(95,*) '---> returned from symb 1'
        return
      endif
c reset user symbols if hite is negative
      if (hite .lt. 0.) then
        nu = 0
c	write(95,*) '---> returned from symb 2'
        return
      endif
c just drop pen if symbol hite = 0.
      if (hite .eq. 0.) then
        call pltt(x, y, 3)
        call pltt(x, y, 2)
c	write(95,*) '---> returned from symb 3'
        return
      endif
c decode symbol type
      isym = absym
      ang = (absym - isym)*1000
      ifil = isym/100
      ibs = isym - ifil*100
      ityp = ibs - (ibs/10)*10
      npts = (ibs - ityp)/10
      nfil = 0
c      write(91,*) 'sym ifil npts ityp ang  ibs'
c      write(91,*) absym, ifil, npts, ityp, ang, ibs
c if symbol is to be filled, decide how many interior symbols to plot (nfil)
c and the fractional decrease in size between symbols
      if(ifil .ne. 0) 
     *     call nfilok(defact, hite, ibs, ifil, ityp, nfil, npts)
      if((ikey(ibs) .eq. 0) .or. (ityp .lt. 3)) then
c plot normal symbol
        if(ikey(ibs) .eq. 0) then
c          write(91,*)'warning - symbol number ',ibs,' is not defined,'
c          write(91,*) 'so a + will be plotted.'
          call pltarr(14, hite, x, y, dash, space, az, ang, xr,
     *         yr, nfil, defact, ifil, ityp, 30, trace)
c	write(95,*) '---> returned from symb 4'
          return
        endif
        call pltarr(ikey(ibs), hite, x, y, dash, space, az, ang, xr,
     *         yr, nfil, defact, ifil, ityp, 30, trace)
c	write(95,*) '---> returned from symb 5'
        return
      else
c plot user defined symbol
c        write(91,*) 'use user defined symbol ', ibs 
        call pltarr(ikey(ibs), hite, x, y, dash, space, az, ang, xu,
     *         yu, nfil, defact, ifil, ityp, 20, trace)
c	write(95,*) '---> returned from symb 6'
        return
      endif
      end
