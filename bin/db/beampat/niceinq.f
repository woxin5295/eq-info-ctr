      subroutine niceinq(xdim,ydim,xlow,ylow,xmin,xmax,ymin,ymax,
     *                   height,ratio,slant)
c
c    routine niceinq returns the physical dimensions of the current plot
c    and min and max ranges of the current plot.
c
c    Outputs - xdim   = horizontal dimension in inches
c 	       ydim   = vertical dimension in inches
c	       xlow   = horizontal location of lower left hand corner of plot
c			in inches
c	       ylow   = vertical location of lower left hand corner of plot
c			in inches
c              xmin   = value of plot data corresponding to the left hand edge
c			of the plot
c	       xmax   = value of plot data corresponding to the right hand edge
c			of the plot
c	       ymin   = value of plot data corresponding to the bottom edge
c			of the plot
c	       ymax   = value of plot data corresponding to the top edge
c			of the plot
c              height = Height of character
c              ratio  = Ratio of character cell width to height
c              slant  = Slant of character in degrees
c
      common /pdim/ xxdim,yydim,xxlow,yylow,rxdim,rydim,rxlow,rylow,
     1              xbl,ybl,xbh,ybh,xbm,ybm,itran,tangle,
     2              ca,sa,cellht,cellwd
      common /pscl/ xxmin,xxmax,yymin,yymax,xscale,yscale,xrange,yrange
      common /partxt/ sinang,cosang,tansln,hite,rat
c
      data xc / 1638.4 /
      data rad / 0.0174532925 /
c
      xmin = xxmin
      ymin = yymin
      xmax = xxmax
      ymax = yymax
c
      xdim = xxdim
      ydim = yydim
      xlow = xxlow
      ylow = yylow
c
      height = hite/xc
      a = atan(tansln)
      slant = a/rad
      ratio = rat
      return
      end
