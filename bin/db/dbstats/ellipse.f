      subroutine ellipse(xc,yc,r1,r2,az,nbarc,iclose,iclip,thick,ithick)
c
c    routine ellipse will draw either a open or closed ellipse (open meaning
c    an ellipse with a white center, closed an ellipse with a black center)
c
c    inputs  - xc     = x-coordinate of the center of the circle in user units
c	       yc     = y-coordinate of the center of the circle in user units
c              r1     = semi-major axis of the ellipse in inches
c              r2     = semi-minor axis of the ellipse in inches
c              az     = azimuth (in degrees) of the semi-major axis from the 
c			vertical (north)
c              nbarc   = number of straight line segments to approximate the
c			ellipse with.
c                       (note: if eq -4 draws an unfilled diamond
c                       or a filled star,  if eq +4 draws a filled or ufilled 
c                       square.  Each fit inside an ellipse with radius r1,r2)
c              iclose = open-closed flag
c			= 0 - ellipse open
c			.ne. 0 - ellipse closed
c              iclip  = clip flag see nplot
c              thick  = line thickness see nplot
c			note: this is passed to nplot along with iclip.
c 			      This allows big open ellipses with thick
c			      circumferences. Probably should not be used
c			      for closed ellipses.
c              ithick = thickness flag see nplot
c
      dimension xbuf(100),ybuf(100)
c
      common /pscl/ xmin,xmax,ymin,ymax,xscale,yscale,xrange,yrange
c
      common /spc/ xll,yll,xur,yur,ASPECT,rc
c
      data  pi  / 3.141596 /
c
c  rc = Standard raster conversion in rasters per inch
c
c    choose nice orientations for small order polygons
c
      azz=az*pi/180.
      narc=nbarc
      bomeg = 0.
      if(narc.eq.3)bomeg=pi/2.
      if(narc.eq.4)bomeg=pi/4.
      if(narc.eq.-4)narc=4
c
c    draw open ellipse
c
      if (narc .gt. 99) narc = 99
      npl = narc + 1
      domega = 2.*pi/narc
      do 10  i = 1,npl
      omega = bomeg + domega*(i-1)
      xb = r2*cos(omega)
      yb = r1*sin(omega)
      xbuf(i) = xc + (xb*cos(azz) + yb*sin(azz))*rc/xscale
   10 ybuf(i) = yc + (yb*cos(azz) - xb*sin(azz))*rc/yscale
      call nplot(npl,xbuf,ybuf,0,iclip,thick,ithick,' ')
c
c    close ellipse if necessary
c
      if (iclose .eq. 0)  go to 900
      dr1 = r1/sqrt(2.)
      dr2 = r2/sqrt(2.)
      xbuf(1) = xc - (dr1*sin(azz))*rc/xscale
      xbuf(2) = xc + (dr1*sin(azz))*rc/xscale
      ybuf(1) = yc - (dr1*cos(azz))*rc/yscale
      ybuf(2) = yc + (dr1*cos(azz))*rc/yscale
      rth = 2.*dr2
      call nplot(2,xbuf,ybuf,0,iclip,rth,ithick,' ')
      dr = r1/1.6
      dr = r1 - dr
      rr = r1 - dr/2.
      rth = dr
      do 20  i = 1,npl
      omega = bomeg + domega*(i-1)
      xb = r2*cos(omega)
      yb = r1*sin(omega)
      xbuf(i) = xc + (xb*cos(azz) + yb*sin(azz))*rc/xscale
  20  ybuf(i) = yc + (yb*cos(azz) - xb*sin(azz))*rc/yscale
      call nplot(npl,xbuf,ybuf,0,iclip,rth,1,' ')
c
  900 return
      end
