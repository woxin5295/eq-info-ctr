      subroutine symbol (type, x, y, size, thick, iclip, ifill)
      character*(*)      type
c
c     common /pscl/ xmin,xmax,ymin,ymax,xscale,yscale,xrange,yrange
c
      real*4 xplt(10), yplt(10)
      character*8 xtype, ytype
c
      xr = xmap(x)
      yr = ymap(y)
      xscale = 1.0
      yscale = 1.0
      call gettype (xtype, ytype)
      call getdim (xdim, ydim, xlow, ylow)
      call getscl (xxmin, xxmax, yymin, yymax)
      call ntype ('LIN', 'LIN')
      call setdim (xdim, ydim, xlow, ylow)
      call setscl (0.0, xdim, 0.0, ydim)
      xx = rxmap(xr)
      yy = rymap(yr)
      if (ifill .eq. 1) call nfillon
      if (type .eq. 'box') then
        x1 = xx - size*0.5 / xscale
        y1 = yy - size*0.5 / yscale
        x2 = xx + size*0.5 / xscale
        y2 = yy + size*0.5 / yscale
        call box (x1, x2, y1, y2, thick, 0, iclip)
      else if (type .eq. 'circle') then
        call circle(xx,yy,size*0.5/xscale,90,ifill,iclip,0.1,0)
      else if (type .eq. 'triangle') then
	xplt(1) = xx
	yplt(1) = yy + size/sqrt(3.0)/yscale
	xplt(2) = xx + size*0.5/xscale
	yplt(2) = yy - size*0.5/sqrt(3.0)/yscale
	xplt(3) = xx - size*0.5/xscale
	yplt(3) = yplt(2)
	xplt(4) = xplt(1)
	yplt(4) = yplt(1)
	xplt(5) = xplt(2)
	yplt(5) = yplt(2)
	call nplot (5, xplt, yplt, 0, iclip, thick, 0, ' ')
      else if (type .eq. 'hexagon') then
	xplt(1) = xx - size*0.25/xscale
	yplt(1) = yy + size*0.5*sqrt(0.75)/yscale
	xplt(2) = xx + size*0.25/xscale
	yplt(2) = yplt(1)
	xplt(3) = xx + size*0.5/xscale
	yplt(3) = yy
	xplt(4) = xplt(2)
	yplt(4) = yy - size*0.5*sqrt(0.75)/yscale
	xplt(5) = xplt(1)
	yplt(5) = yplt(4)
	xplt(6) = xx - size*0.5/xscale
	yplt(6) = yplt(3)
	xplt(7) = xplt(1)
	yplt(7) = yplt(1)
	xplt(8) = xplt(2)
	yplt(8) = yplt(2)
	call nplot (8, xplt, yplt, 0, iclip, thick, 0, ' ')
      else if (type .eq. 'cross') then
        x1 = xx - size*0.5 / xscale
        y1 = yy - size*0.5 / yscale
        x2 = xx + size*0.5 / xscale
        y2 = yy + size*0.5 / yscale
        call line (x1, yy, x2, yy, thick, 0, iclip)
        call line (xx, y1, xx, y2, thick, 0, iclip)
      end if
      if (ifill .eq. 1) call nfilloff
      call ntype (xtype, ytype)
      call setdim (xdim, ydim, xlow, ylow)
      call setscl (xxmin, xxmax, yymin, yymax)
c
      return
      end
