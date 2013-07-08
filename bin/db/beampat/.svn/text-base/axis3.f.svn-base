      subroutine axis3(xdim,ydim,xmarg,ymarg,xlow,ylow,xmax,xmin,
     1  ymax,ymin,dxsmal,dxnumb,dysmal,dynumb,
     2  fmtx,fmty,labelx,labely,title,title2,title3,iclear)
c
c    routine axis will draw a box with tic marks, label the x and y axes
c    and label the plot with a title.
c
c    inputs  - xdim   = x dimension of the box in inches
c              ydim   = y dimension of the box in inches
c              xmarg  = margin below the x-axis in inches - determines
c			the vertical spacing of the x-axis label
c              ymarg  = margin to the left of the y-axis in inches - determines
c			the horizontal spacing of the y-axis label
c              xlow   = x-location of the lower left hand corner of the box
c			in inches from the lower left hand corner of the
c			plot
c              ylow   = y-location etc.
c              xmax   = x value of the right edge of the box in user units
c              xmin   = x value of the left edge of the box in user units
c              ymax   = y value of the top of the box in user units
c              ymin   = y value of the bottom of the box in user units
c              dxsmal = increment between small tic marks along the
c			x-axis without number labelling
c              dxnumb = increment between large tic marks along the
c                       x-axis with number labelling
c              dysmal,dynumb = etc. for y-axis
c              fmtx   = character string format specification including
c			paranthesis which determines the x-axis numerical
c			labeling format
c              fmty   = etc. for the y-axis
c              labelx = character string label for the x-axis
c              labely = etc. for y-axis
c              title  = character string title which is placed on top
c			of the plot
c              iclear = clear flag for tektronix (if .ne. 0 then clear)
c
      character*(*) labelx,labely,title,title2,title3,fmtx,fmty
c
      common  /ocflag/  imflag,iocflg,iltp
      integer*4  ltp
c
      common /npcolr/ fhue, flight, fsat, bhue, blight, bsat
c
      common /axstuf/ ifont
c
c     data  ifont / 115 /
c
c
c    clear if appropriate
c
      if (iclear .ne. 0 .and. iclear .ne. 1) call clear
      imfl = imflag
      if (imflag .eq. 1) call nopen
      ltp = iltp
      CALL NLSTYL(0)
      call ntype('LIN','LIN')
c
c    box first
c
      x = xdim   + .03
      y = ydim  + .03
      xl = xlow  - .015
      yl = ylow  - .015
      call setdim(x,y,xl,yl)
      call setscl(0.,1.,0.,1.)
      if (iclear .eq. 1) call clrrgn(0.,1.,0.,1.)
      call box(0.,1.,0.,1.,.03,0,1)
c
c    title next
c
      call cfont(ifont)
      call setdim(xdim,ydim,xlow,ylow)
      call setscl(0.,1.,0.,y)
      call chrsiz(.2,1.,0.)
      y1 = y + .7
      y2 = y + .45
      y3 = y + .2
      if(title3.eq.' '.and.title2.eq.' ') then
        y1 = y3
      elseif(title3.eq.' '.and.title2.ne.' ') then
        y1 = y2
        y2 = y3
      elseif(title3.ne.' '.and.title2.eq.' ') then
        y1 = y2
      endif
      call text(.5,y1,0.,3,title,1)
      call chrsiz(.14,1.,0.)
      call text(.5,y2,0.,3,title2,1)
      call text(.5,y3,0.,3,title3,1)
c
c    x-axis
c
      call setdim(xdim,ydim,xlow,ylow)
      call setscl(xmin,xmax,0.,ydim)
C
C    SMALL TICS
C
      DDXBIG = DXNUMB
      if (dxsmal .le. 0.)  go to 101
C
C    FIRST DETERMINE "GOOD" INCREMENT
C
      DDXSML = DXSMAL
  109 NTICS = ABS(XMAX-XMIN)/DDXSML
      IF (NTICS .LT. 60)  GO TO 110
      DDXSML = DDXSML*2.
      CALL DNICE(DDXSML,DDXSML,DDXBIG)
      GO TO 109
  110 if (xmax .gt. xmin) then
        xmx = xmax
        xmn = xmin
        iref = 1
      else
        xmx = xmin
        xmn = xmax
        iref = -1
      end if
      if (xmn .ge. 0.) then
        xx = (xmn/DDXSML)*1.000001 + 1.
        nmin = xx
      else
        xx = (-xmn/DDXSML)*0.999999
        nmin = xx
        nmin = -nmin
      end if
      if (xmx .ge. 0.) then
        xx = (xmx/DDXSML)*0.999999
        nmax = xx
      else
        xx = (-xmx/DDXSML)*1.000001 + 1.
        nmax = xx
        nmax = -nmax
      end if
      ntics = nmax - nmin + 1
      if (ntics .le. 0)  go to 101
      stic = nmin*DDXSML - xmn
      call tics(xmn,0.,xmx,0.,stic,ntics,DDXSML,.10,0.,iref)
      iref = -iref
      call tics(xmn,ydim,xmx,ydim,stic,ntics,DDXSML,
     1  .10,0.,iref)
  101 if (dxnumb .le. 0.)  go to 100
      if (xmax .gt. xmin) then
        xmx = xmax
        xmn = xmin
        iref = 1
      else
        xmx = xmin
        xmn = xmax
        iref = -1
      end if
      if (xmn .ge. 0.) then
        xx = (xmn/DDXBIG)*1.000001 + 1.
        nmin = xx
      else
        xx = (-xmn/DDXBIG)*0.999999
        nmin = xx
        nmin = -nmin
      end if
      if (xmx .ge. 0.) then
        xx = (xmx/DDXBIG)*0.999999
        nmax = xx
      else
        xx = (-xmx/DDXBIG)*1.000001 + 1.
        nmax = xx
        nmax = -nmax
      end if
      ntics = nmax - nmin + 1
      if (ntics .le. 0)  go to 90
      stic = nmin*DDXBIG - xmn
      call tics(xmn,0.,xmx,0.,stic,ntics,DDXBIG,.17,0.,iref)
      iref = -iref
      call tics(xmn,ydim,xmx,ydim,stic,ntics,DDXBIG,
     1  .17,0.,iref)
      if (abs(stic-DDXBIG)/DDXBIG .lt. .00001) nmin = nmin - 1
      if (abs(xmx-nmax*DDXBIG-DDXBIG)/DDXBIG .lt. .00001)
     1  nmax = nmax + 1
      do 50  i = nmin,nmax
      x = DDXBIG*i
      call number(x,-.1,0.,5,x,fmtx)
   50 continue
      go to 100
   90 call number(xmin,-.1,0.,5,xmin,fmtx)
      call number(xmax,-.1,0.,5,xmax,fmtx)
      ix = xmax
  100 xx = -xmarg
      yy = (xmin+xmax)/2.
      call text(yy,xx,0.,3,labelx,1)
c
c    y-axis
c
      call setscl(0.,xdim,ymin,ymax)
C
C    SMALL TICS
C
      DDYBIG = DYNUMB
      if (dysmal .le. 0.)  go to 201
C
C    FIRST DETERMINE "GOOD" INCREMENT
C
      DDYSML = DYSMAL
  209 NTICS = ABS(YMAX-YMIN)/DDYSML
      IF (NTICS .LT. 60)  GO TO 210
      DDYSML = DDYSML*2.
      CALL DNICE(DDYSML,DDYSML,DDYBIG)
      GO TO 209
  210 if (ymax .gt. ymin) then
        xmx = ymax
        xmn = ymin
        iref = -1
      else
        xmx = ymin
        xmn = ymax
        iref = 1
      end if
      if (xmn .ge. 0.) then
        xx = (xmn/DDYSML)*1.000001 + 1.
        nmin = xx
      else
        xx = (-xmn/DDYSML)*0.999999
        nmin = xx
        nmin = -nmin
      end if
      if (xmx .ge. 0.) then
        xx = (xmx/DDYSML)*0.999999
        nmax = xx
      else
        xx = (-xmx/DDYSML)*1.000001 + 1.
        nmax = xx
        nmax = -nmax
      end if
      ntics = nmax - nmin + 1
      if (ntics .le. 0)  go to 201
      stic = nmin*DDYSML - xmn
      call tics(0.,xmn,0.,xmx,stic,ntics,DDYSML,.10,0.,iref)
      iref = -iref
      call tics(xdim,xmn,xdim,xmx,stic,ntics,DDYSML,
     1  .10,0.,iref)
  201 if (dynumb .le. 0.)  go to 200
      if (ymax .gt. ymin) then
        xmx = ymax
        xmn = ymin
        iref = -1
      else
        xmx = ymin
        xmn = ymax
        iref = 1
      end if
      if (xmn .ge. 0.) then
        xx = (xmn/DDYBIG)*1.000001 + 1.
        nmin = xx
      else
        xx = (-xmn/DDYBIG)*0.999999
        nmin = xx
        nmin = -nmin
      end if
      if (xmx .ge. 0.) then
        xx = (xmx/DDYBIG)*0.999999
        nmax = xx
      else
        xx = (-xmx/DDYBIG)*1.000001 + 1.
        nmax = xx
        nmax = -nmax
      end if
      ntics = nmax - nmin + 1
      if (ntics .le. 0)  go to 290
      stic = nmin*DDYBIG - xmn
      call tics(0.,xmn,0.,xmx,stic,ntics,DDYBIG,.17,0.,iref)
      iref = -iref
      call tics(xdim,xmn,xdim,xmx,stic,ntics,DDYBIG,
     1  .17,0.,iref)
      if (abs(stic-DDYBIG)/DDYBIG .lt. .00001) nmin = nmin - 1
      if (abs(xmx-nmax*DDYBIG-DDYBIG)/DDYBIG .lt. .00001)
     1  nmax = nmax + 1
      do 250  i = nmin,nmax
      x = DDYBIG*i
      call number(-.1,x,0.,7,x,fmty)
  250 continue
      go to 200
  290 call number(ymin,-.1,0.,5,ymin,fmty)
      call number(ymax,-.1,0.,5,ymax,fmty)
  200 xx = -ymarg
      yy = (ymin+ymax)/2.
      call text(xx,yy,90.,5,labely,1)
c
      call setscl(xmin,xmax,ymin,ymax)
c
c     if (imfl .eq. 1) call nclose
c
      return
      end
