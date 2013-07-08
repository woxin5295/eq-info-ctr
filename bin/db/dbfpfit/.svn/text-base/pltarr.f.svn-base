      subroutine pltarr(isk, hite, x, y, dash, space, az, ang, xx,
     *             yy, nfil, defact, ifil, ityp, ndx, trace)
c plot pre-defined symbol number nu
      real    ang             ! angle of symbol derived from sym value
      real    az              ! clockwise rotation of symbol (degrees)
      real    caz             ! cos(orientation angle)
      real    dash            ! dash length
      real    defact          ! fractional reduction in size prior to each fill sym
      real    factor          ! fractional size of fill symbol
      real    hite            ! height of symbol
      integer ifil            ! fill code - (0=no fill, 9=all filled)
      integer ityp            ! type of symbol (0=reg poly, 1=star, 2=* pattern)
      integer iup             ! calcomp plot control number
      integer isk             ! index to symbol array
      integer nfil            ! number of additional symbols needed for fill
      real    rad             ! radians per degree
      real    saz             ! sin(orientation angle)
      real    smlest          ! size of iner square for completely filled symbol
      real    space           ! space length
      logical trace           ! if true, write all x,y values to file 21
      real    xplt            ! x value to plot
      real    xt(200)         ! temporary x values for current symbol
      real    xx(ndx, *)      ! xx(i,j) is the j'th x value for symbol # i
      real    yplt            ! y value to plot
      real    yt(200)         ! temporary y values for current symbol
      real    yy(ndx, *)      ! yy(i,j) is the j'th y value for symbol # i
      data rad/.017453293/
c
c	write(95,*) '---> called pltarr with:'
c	write(95,*) 'isk, hite, x, y, dash, space, az, ang, 
c     &nfil, defact, ifil, ityp, ndx, trace ='
c      	write(95,*) isk, hite, x, y, dash, space, az, ang,
c     &nfil, defact, ifil, ityp, ndx, trace
c
      iup = 3
      if(az+ang .eq. 0.) then
        do 30 n = 1, 200
c          write(91,*) xx(isk,n), yy(isk,n)
          if(xx(isk, n) .eq. 8888. ) then
            if(trace) write(21, *) xx(isk, n), yy(isk, n)
            xt(n) = 8888.
            iup = 3
            go to 30
          endif
          if(xx(isk, n) .eq. 9999. ) go to 42
          xt(n) = xx(isk, n)*hite
          xplt = x + xt(n)
          yt(n) = yy(isk, n)*hite
          yplt = y + yt(n)
          call dashit(xplt, yplt, iup, dash, space, 0.)
          iup = 2
          if(trace) write(21, *) xt(n), yt(n)
30      continue
      else
        caz = cos(rad*(az+ang))
        saz = sin(rad*(az+ang))
        do 40 n = 1, 200
          if(xx(isk, n) .eq. 8888. ) then
            if(trace) write(21, *) xx(isk, n), yy(isk, n)
            xt(n) = 8888.
            iup = 3
            go to 40
          endif
          if(xx(isk, n) .eq. 9999. ) go to 42
          xt(n) = xx(isk, n)*hite*caz + yy(isk, n)*hite*saz
          xplt = x + xt(n)
          yt(n) = yy(isk, n)*hite*caz - xx(isk, n)*hite*saz
          yplt = y + yt(n)
          call dashit(xplt, yplt, iup, dash, space, 0.)
          iup = 2
          if(trace) write(21, *) xt(n), yt(n)
40      continue
      endif
42    if(trace) write(21, *) xx(isk, n), yy(isk, n)
      nsv = n - 1
      if(nfil .ne. 0) then
        factor = 1.
        do 60 i = 1, nfil
          iup = 3
          factor = factor - defact
          do 50 n = 1, nsv
            if(xt(n) .eq. 8888. ) then
              if(trace) write(21, *) xt(n), yt(n)
              iup = 3
              go to 50
            endif
            xplt = x + xt(n)*factor
            yplt = y + yt(n)*factor
            call dashit(xplt, yplt, iup, dash, space, 0.)
            iup = 2
            if(trace) write(21, *) xt(n)*factor, yt(n)*factor
50        continue
60      continue
        call transp
c        write(91,*) 'final value of factor = ', factor
c       fill in the center
        if(ifil .eq. 9) then
          smlest = defact*hite*.125
          if(ityp .eq. 1) smlest = smlest*.5
c          write(91,*) 'hite, defact'
c          write(91,*) 'ityp, smlest', ityp, smlest
          call mbox(x-smlest,y-smlest,x+smlest,y+smlest)
        endif
      endif
c	write(95,*) '---> returned from pltarr'
      return
      end
