      subroutine arbsymb(x, y, symht, azr, isym)
      real              xs(125)       ! symbol x values
      real              ys(125)       ! symbol y values
      npts = 0
      call gensymb(x, y, symht, azr, isym, npts, xs, ys)
c       call transp
c       print *, 'x, y, symht, azr, isym, npts'
c       print *, x, y, symht, azr, isym, npts
      iup = 3
      do 30 i = 1, npts
c       call transp
c       print *, 'i, xs(i), ys(i), iup'
c       print *, i, xs(i), ys(i), iup
        if(xs(i) .eq. 9999. .and. ys(i) .eq. 9999.) then
          iup = 3
          go to 30
        endif
        call plot(xs(i), ys(i), iup)
c       call transp
c       print *, xs(i), ys(i), iup
        iup = 2
30    continue
      return
      end
