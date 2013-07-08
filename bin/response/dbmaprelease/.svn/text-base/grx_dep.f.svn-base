# 1 "grx_dep_mac.F"
      subroutine axisdep(xdim,ydim,xmarg,ymarg,xlow,ylow,xmax,xmin,
     1  ymax,ymin,xm,rad,dxsmal,dxnumb,dysmal,dynumb,
     2  fmtx,fmty,labelx,labely,title,iclear)
c
c    routine axisdep will draw a box with tic marks, label the x and y axes
c    and label the plot with a title.  Uses a cylindrical projection to give
c    (for example) more accurate depth plots that are very deep.
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
c              xm     = x value about which to do cylindrical projection
c              rad    = radius for cylindrical projection
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
      character*(*) labelx,labely,title,fmtx,fmty
c
      common  /ocflag/  imflag,iocflg,iltp
      integer*4  ltp
c
      common /npcolr/ fhue, flight, fsat, bhue, blight, bsat
c
      common /axstuf/ ifont
c
c    clear if appropriate
c
      if (iclear .ne. 0) call clear
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
      call setscl(xmin,xmax,ymin,ymax)
      call boxdep(xmin,xmax,ymin,ymax,xm,rad,.03,0,1)
c
c    title next
c
      call cfont(ifont)
      call setdim(xdim,ydim,xlow,ylow)
      call setscl(0.,1.,0.,y)
      call chrsiz(.15,1.,0.)
      y2 = y + .2
      call text(.5,y2,0.,3,title,1)
      call chrsiz(.10,1.,0.)
c
c    x-axis
c
      call setdim(xdim,ydim,xlow,ylow)
      call setscl(xmin,xmax,ymin,ymax)
      xxx=xmin - .02*(xmax-xmin)
      yyy=ymin - .02*(ymax-ymin)
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
      call ticsdep(xmn,ymin,xmx,ymin,xm,rad,stic,ntics,DDXSML,.07,0.,
     *iref)
      iref = -iref
      call ticsdep(xmn,ymax,xmx,ymax,xm,rad,stic,ntics,DDXSML,
     1  .07,0.,iref)
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
      call ticsdep(xmn,ymin,xmx,ymin,xm,rad,stic,ntics,DDXBIG,.10,0.,
     *iref)
      iref = -iref
      call ticsdep(xmn,ymax,xmx,ymax,xm,rad,stic,ntics,DDXBIG,
     1  .10,0.,iref)
      if (abs(stic-DDXBIG)/DDXBIG .lt. .00001) nmin = nmin - 1
      if (abs(xmx-nmax*DDXBIG-DDXBIG)/DDXBIG .lt. .00001)
     1  nmax = nmax + 1
      do 50  i = nmin,nmax
      x = DDXBIG*i
      call depcon(x,yyy,xm,rad,xd,yd)
      call number(xd,yd,0.,5,x,fmtx)
   50 continue
      go to 100
   90 call depcon(xmin,yyy,xm,rad,xd,yd)
      call number(xd,yd,0.,5,xmin,fmtx)
      call depcon(xmax,yyy,xm,rad,xd,yd)
      call number(xd,yd,0.,5,xmax,fmtx)
      ix = xmax
  100 xx = -xmarg
      yy = (xmin+xmax)/2.
      call setscl(xmin,xmax,0.,ydim)
      call text(yy,xx,0.,3,labelx,1)
c
c    y-axis
c
      call setscl(xmin,xmax,ymin,ymax)
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
      call ticsdep(xmin,xmn,xmin,xmx,xm,rad,stic,ntics,DDYSML,.07,0.,
     *iref)
      iref = -iref
      call ticsdep(xmax,xmn,xmax,xmx,xm,rad,stic,ntics,DDYSML,
     1  .07,0.,iref)
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
      call ticsdep(xmin,xmn,xmin,xmx,xm,rad,stic,ntics,DDYBIG,.10,0.,
     *iref)
      iref = -iref
      call ticsdep(xmax,xmn,xmax,xmx,xm,rad,stic,ntics,DDYBIG,
     1  .10,0.,iref)
      if (abs(stic-DDYBIG)/DDYBIG .lt. .00001) nmin = nmin - 1
      if (abs(xmx-nmax*DDYBIG-DDYBIG)/DDYBIG .lt. .00001)
     1  nmax = nmax + 1
      do 250  i = nmin,nmax
      x = DDYBIG*i
      call depcon(xxx,x,xm,rad,xd,yd)
      call number(xd,yd,0.,7,x,fmty)
  250 continue
      go to 200
  290 call depcon(xxx,ymin,xm,rad,xd,yd)
      call number(xd,yd,0.,5,ymin,fmty)
      call depcon(xxx,ymax,xm,rad,xd,yd)
      call number(xd,yd,0.,5,ymax,fmty)
  200 xx = -ymarg
      yy = (ymin+ymax)/2.
      call setscl(0.,xdim,ymin,ymax)
      call text(xx,yy,90.,5,labely,1)
c
      call setscl(xmin,xmax,ymin,ymax)
c
c     if (imfl .eq. 1) call nclose
c
      return
      end
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
      subroutine axis3lglg(xdim,ydim,xmarg,ymarg,xlow,ylow,xmax,
     1  xmin,ymax,ymin,dxsmal,dxnumb,dysmal,dynumb,
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
c     data  ifont / 112 /
c
c
      x10 = 10.
c
c    clear if appropriate
c
      if (iclear .ne. 0 .and. iclear .ne. 1) call clear
      imflag = 0
      ltp = iltp
      CALL NOPEN
      CALL NLSTYL(0)
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
      xrem=xmin - aint(xmin)
      xrem1 = 0.
C
C    SMALL TICS
C
      DDXBIG = 1.
      if (dxsmal .le. 0.)  go to 101
C
C    FIRST DETERMINE "GOOD" INCREMENT
C
      DDXSML = 1.
  109 NTICS = aint(ABS(XMAX-XMIN))
      if(xrem.ne.0.) then
        ntics=ntics+1
        xrem1=-1.
      endif
  110 xmx = xmax
      xmn = xmin
      iref = 1
      if (xmax .lt. xmin) xmx = xmin
      if (xmax .lt. xmin) xmn = xmax
      if (xmax .lt. xmin) iref = -1
      if (ntics .le. 0)  go to 101
      irefs=iref
      do 1 i=1,9
        stic = alog10(float(i)) + xrem + xrem1
        iref=irefs
        call tics(xmn,0.,xmx,0.,stic,ntics,DDXSML,.10,0.,iref)
        iref = -iref
        call tics(xmn,ydim,xmx,ydim,stic,ntics,DDXSML,
     1    .10,0.,iref)
  1   continue
  101 if (dxnumb .le. 0.)  go to 100
      xmx = xmax
      xmn = xmin
      iref = 1
      if (xmax .lt. xmin) xmx = xmin
      if (xmax .lt. xmin) xmn = xmax
      if (xmax .lt. xmin) iref = -1
      ntics = aint(abs(xmax-xmin) + 1.)
      if (ntics .le. 0)  go to 90
      stic = xrem
      call tics(xmn,0.,xmx,0.,stic,ntics,DDXBIG,.17,0.,iref)
      iref = -iref
      call tics(xmn,ydim,xmx,ydim,stic,ntics,DDXBIG,
     1  .17,0.,iref)
      do 50  i = 1,ntics
        x = DDXBIG*(i-1) + xmin +xrem
        call number(x,-.13,0.,5,x10,'(i2)')
        call chrsiz(.09,1.,0.)
        xe = x + .03*(xmax-xmin)
        call number(xe,-.13,0.,1,x,fmtx)
        call chrsiz(.14,1.,0.)
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
      DDYBIG = 1.
      if (dysmal .le. 0.)  go to 201
C
C    FIRST DETERMINE "GOOD" INCREMENT
C
      DDYSML = 1.
  209 NTICS = aint(ABS(YMAX-YMIN))
  210 xmx = ymax
      xmn = ymin
      iref = -1
      if (ymax .lt. ymin) xmx = ymin
      if (ymax .lt. ymin) xmn = ymax
      if (ymax .lt. ymin) iref = 1
      if (ntics .le. 0)  go to 201
      irefs=iref
      do 2 i=1,9
        stic = alog10(float(i))
        iref=irefs
        call tics(0.,xmn,0.,xmx,stic,ntics,DDYSML,.10,0.,iref)
        iref = -iref
        call tics(xdim,xmn,xdim,xmx,stic,ntics,DDYSML,
     1    .10,0.,iref)
  2   continue
  201 if (dynumb .le. 0.)  go to 200
      xmx = ymax
      xmn = ymin
      iref = -1
      if (ymax .lt. ymin) xmx = ymin
      if (ymax .lt. ymin) xmn = ymax
      if (ymax .lt. ymin) iref = 1
      ntics = aint(abs(ymax-ymin) + 1.)
      if (ntics .le. 0)  go to 290
      stic = 0.
      call tics(0.,xmn,0.,xmx,stic,ntics,DDYBIG,.17,0.,iref)
      iref = -iref
      call tics(xdim,xmn,xdim,xmx,stic,ntics,DDYBIG,
     1  .17,0.,iref)
      do 250  i = 1,ntics
        x = DDYBIG*(i-1) + ymin
        call number(-.24,x,0.,7,x10,'(i2)')
        call chrsiz(.09,1.,0.)
        xe = x + .01
        call number(-.24,xe,0.,0,x,fmty)
        call chrsiz(.14,1.,0.)
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
      CALL NCLOSE
      imflag = 1
c
      return
      end
      subroutine axis3lgln(xdim,ydim,xmarg,ymarg,xlow,ylow,xmax,
     1  xmin,ymax,ymin,dxsmal,dxnumb,dysmal,dynumb,
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
c     data  ifont / 112 /
c
c
      x10 = 10.
c
c    clear if appropriate
c
      if (iclear .ne. 0 .and. iclear .ne. 1) call clear
      imflag = 0
      ltp = iltp
      CALL NOPEN
      CALL NLSTYL(0)
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
      DDXBIG = 1.
      if (dxsmal .le. 0.)  go to 101
C
C    FIRST DETERMINE "GOOD" INCREMENT
C
      DDXSML = 1.
  109 NTICS = aint(ABS(XMAX-XMIN))
  110 xmx = xmax
      xmn = xmin
      iref = 1
      if (xmax .lt. xmin) xmx = xmin
      if (xmax .lt. xmin) xmn = xmax
      if (xmax .lt. xmin) iref = -1
      if (ntics .le. 0)  go to 101
      irefs=iref
      do 1 i=1,9
        stic = alog10(float(i))
        iref=irefs
        call tics(xmn,0.,xmx,0.,stic,ntics,DDXSML,.10,0.,iref)
        iref = -iref
        call tics(xmn,ydim,xmx,ydim,stic,ntics,DDXSML,
     1    .10,0.,iref)
  1   continue
  101 if (dxnumb .le. 0.)  go to 100
      xmx = xmax
      xmn = xmin
      iref = 1
      if (xmax .lt. xmin) xmx = xmin
      if (xmax .lt. xmin) xmn = xmax
      if (xmax .lt. xmin) iref = -1
      ntics = aint(abs(xmax-xmin) + 1.)
      if (ntics .le. 0)  go to 90
      stic = 0.
      call tics(xmn,0.,xmx,0.,stic,ntics,DDXBIG,.17,0.,iref)
      iref = -iref
      call tics(xmn,ydim,xmx,ydim,stic,ntics,DDXBIG,
     1  .17,0.,iref)
      do 50  i = 1,ntics
        x = DDXBIG*(i-1) + xmin
        call number(x,-.13,0.,5,x10,'(i2)')
        call chrsiz(.09,1.,0.)
        xe = x + .1
        call number(xe,-.13,0.,1,x,fmtx)
        call chrsiz(.14,1.,0.)
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
      DDYBIG = abs(DYNUMB)
      if (dysmal) 207,201,208
  207 ddysml=abs(dysmal)
      ntics=abs(ymax-ymin)/ddysml
      go to 210
C
C    FIRST DETERMINE "GOOD" INCREMENT
C
  208 DDYSML = DYSMAL
  209 NTICS = ABS(YMAX-YMIN)/DDYSML
      IF (NTICS .LT. 60)  GO TO 210
      DDYSML = DDYSML*2.
      CALL DNICE(DDYSML,DDYSML,DDYBIG)
      GO TO 209
  210 xmx = ymax
      xmn = ymin
      iref = -1
      if (ymax .lt. ymin) xmx = ymin
      if (ymax .lt. ymin) xmn = ymax
      if (ymax .lt. ymin) iref = 1
      xx = (xmn/DDYSML) + 1.
      if (xx .ge. 0.) xx = xx + .0001
      if (xx .lt. 0.) xx = xx - .0001
      nmin = xx
      xx = (xmx/DDYSML)
      if (xx .ge. 0.) xx = xx - .0001
      if (xx .lt. 0.) xx = xx + .0001
      nmax = xx
      ntics = nmax - nmin + 1
      if (ntics .le. 0)  go to 201
      stic = nmin*DDYSML - xmn
      call tics(0.,xmn,0.,xmx,stic,ntics,DDYSML,.10,0.,iref)
      iref = -iref
      call tics(xdim,xmn,xdim,xmx,stic,ntics,DDYSML,
     1  .10,0.,iref)
  201 if (dynumb .eq. 0.)  go to 200
      xmx = ymax
      xmn = ymin
      iref = -1
      if (ymax .lt. ymin) xmx = ymin
      if (ymax .lt. ymin) xmn = ymax
      if (ymax .lt. ymin) iref = 1
      xx = (xmn/DDYBIG) + 1.
      if (xx .ge. 0.) xx = xx + .0001
      if (xx .lt. 0.) xx = xx - .0001
      nmin = xx
      xx = (xmx/DDYBIG)
      if (xx .ge. 0.) xx = xx - .0001
      if (xx .lt. 0.) xx = xx + .0001
      nmax = xx
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
      CALL NCLOSE
      imflag = 1
c
      return
      end
      subroutine axis3lnlg(xdim,ydim,xmarg,ymarg,xlow,ylow,xmax,xmin,
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
c     data  ifont / 112 /
c
c
      x10 = 10.
c
c    clear if appropriate
c
      if (iclear .ne. 0 .and. iclear .ne. 1) call clear
      imflag = 0
      ltp = iltp
      CALL NOPEN
      CALL NLSTYL(0)
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
      DDXBIG = abs(DXNUMB)
      if (dxsmal) 107,101,108
  107 ddxsml=abs(dxsmal)
      ntics=abs(xmax-xmin)/ddxmsl
      go to 110
C
C    FIRST DETERMINE "GOOD" INCREMENT
C
  108 DDXSML = DXSMAL
  109 NTICS = ABS(XMAX-XMIN)/DDXSML
      IF (NTICS .LT. 60)  GO TO 110
      DDXSML = DDXSML*2.
      CALL DNICE(DDXSML,DDXSML,DDXBIG)
      GO TO 109
  110 xmx = xmax
      xmn = xmin
      iref = 1
      if (xmax .lt. xmin) xmx = xmin
      if (xmax .lt. xmin) xmn = xmax
      if (xmax .lt. xmin) iref = -1
      xx = (xmn/DDXSML) + 1.
      if (xx .ge. 0.) xx = xx + .0001
      if (xx .lt. 0.) xx = xx - .0001
      nmin = xx
      xx = (xmx/DDXSML)
      if (xx .ge. 0.) xx = xx - .0001
      if (xx .lt. 0.) xx = xx + .0001
      nmax = xx
      ntics = nmax - nmin + 1
      if (ntics .le. 0)  go to 101
      stic = nmin*DDXSML - xmn
      call tics(xmn,0.,xmx,0.,stic,ntics,DDXSML,.10,0.,iref)
      iref = -iref
      call tics(xmn,ydim,xmx,ydim,stic,ntics,DDXSML,
     1  .10,0.,iref)
  101 if (dxnumb .eq. 0.)  go to 100
      xmx = xmax
      xmn = xmin
      iref = 1
      if (xmax .lt. xmin) xmx = xmin
      if (xmax .lt. xmin) xmn = xmax
      if (xmax .lt. xmin) iref = -1
      xx = (xmn/DDXBIG) + 1.
      if (xx .ge. 0.) xx = xx + .0001
      if (xx .lt. 0.) xx = xx - .0001
      nmin = xx
      xx = (xmx/DDXBIG)
      if (xx .ge. 0.) xx = xx - .0001
      if (xx .lt. 0.) xx = xx + .0001
      nmax = xx
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
      DDYBIG = 1.
      if (dysmal .le. 0.)  go to 201
C
C    FIRST DETERMINE "GOOD" INCREMENT
C
      DDYSML = 1.
  209 NTICS = aint(ABS(YMAX-YMIN))
  210 xmx = ymax
      xmn = ymin
      iref = -1
      if (ymax .lt. ymin) xmx = ymin
      if (ymax .lt. ymin) xmn = ymax
      if (ymax .lt. ymin) iref = 1
      if (ntics .le. 0)  go to 201
      irefs=iref
      do 1 i=1,9
        stic = alog10(float(i))
        iref=irefs
        call tics(0.,xmn,0.,xmx,stic,ntics,DDYSML,.10,0.,iref)
        iref = -iref
        call tics(xdim,xmn,xdim,xmx,stic,ntics,DDYSML,
     1    .10,0.,iref)
  1   continue
  201 if (dynumb .le. 0.)  go to 200
      xmx = ymax
      xmn = ymin
      iref = -1
      if (ymax .lt. ymin) xmx = ymin
      if (ymax .lt. ymin) xmn = ymax
      if (ymax .lt. ymin) iref = 1
      ntics = aint(abs(ymax-ymin) + 1.)
      if (ntics .le. 0)  go to 290
      stic = 0.
      call tics(0.,xmn,0.,xmx,stic,ntics,DDYBIG,.17,0.,iref)
      iref = -iref
      call tics(xdim,xmn,xdim,xmx,stic,ntics,DDYBIG,
     1  .17,0.,iref)
      do 250  i = 1,ntics
        x = DDYBIG*(i-1) + ymin
        call number(-.24,x,0.,7,x10,'(i2)')
        call chrsiz(.09,1.,0.)
        xe = x + .01
        call number(-.24,xe,0.,0,x,fmty)
        call chrsiz(.14,1.,0.)
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
      CALL NCLOSE
      imflag = 1
c
      return
      end
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
      SUBROUTINE DEPCON (XIN,Z,XM,RAD,XP,ZP)
C.======================================================================
C.    PURPOSE                                                           
C     one_line_description_of_subroutine                            xx<<
C     Write the general comments here. It is not enough with the        
C     'one_line_description' only ||                                    
C.----------------------------------------------------------------------
C.    KEYWORDS                                                          
C.----------------------------------------------------------------------
C.    INPUT                                                             
C                                                                       
C                                                                       
C.    OUTPUT                                                            
C..   IRC         -   Return code                                       
C                     = 0  no error                                     
C                     = ?                                               
C.----------------------------------------------------------------------
C.    PROGRAMMER    user risktool
C.    CREATION_DATE 10 Jan 1990
C.    MADE_AT  NTNF/NORSAR                                              
C     Pb. 51                                                            
C     N-2007 Kjeller                                                    
C                                                                       
C.    MODIFICATION                                                      
C.    CORRECTION                                                        
C.======================================================================

      x = XIN - XM

      if (x .lt. 0.0) then
         x = -x
         xsn = -1.0
      else
         xsn = 1.0
      endif

      r = RAD - Z

c     theta = atan(x/RAD)
c  change to circular geometry
c
      theta = (x/RAD)

c     XP = xsn*r*sin(theta)
c  changed to yield xp in the same range as xin
c
      XP = xsn*r*sin(theta) + xm

c     ZP = -(RAD-(r*cos(theta)))
c  changed to yield positive depth out for positive depth in
c
      ZP = (RAD-(r*cos(theta)))

      return

      end
      subroutine depprf(x,y,z,x1,y1,x2,y2,del,r,d,dpp)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  Subroutine to project point (x,y,z) orthogonally onto a plane
c
c  Input:
c    x  -  x coordinate of observed point
c    y  -  y coordinate of observed point
c    z  -  z coordinate of observed point
c    x1 -  x coord of 1st point for line where plane intersects surface
c    y1 -  y coord of 1st point for line where plane intersects surface
c    x1 -  x coord of 2nd point for line where plane intersects surface
c    y1 -  y coord of 2nd point for line where plane intersects surface
c    del-  Dip of plane clockwise from point 1 measured from horizontal (deg)
c  
c  Output:
c    r  -  Orthogonal distance from epicenter to line on surface
c    d  -  Orthogonal distance from point (x,y,z) to plane
c    dpp-  Distance from point 1 along line to the orthogonal intersection
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      real x,y,z
      real x1,y1,x2,y2
      real del,dip
      real r,d,dpp
c
      data pi / 3.141592654 /
c
c  change dip from degrees to radians
c
      dip=del*pi/180.
c
c  Calculate slope of line intersection with surface
c
      if(x2.eq.x1) then
        r = abs(x-x1)
        dpp = sign(1.,(y2-y1))*(y-y1)
        d = r*sin(dip) + sign(1.,(x1-x))*z*cos(dip)
        return
      endif
      if(y2.eq.y1) then
        r = abs(y-y1)
        dpp = sign(1.,(x2-x1))*(x-x1)
        d = r*sin(dip) + sign(1.,(y1-y))*z*cos(dip)
        return
      endif
      b=(y2-y1)/(x2-x1)
c
c  Calculate intercept of line intersection with surface
c
      a=y1-(b*x1)
c
c  Calculate y distance of point from line
c
      yp=y-a-b*x
c
c  Calculate x distance of point from line
c
      xp=(y-a)/b - x
c
c  Calculate orthogonal distance of point from line
c  and distance along line to orthogonal intersection
c
      if(xp.eq.0..or.yp.eq.0.) then
        r = 0.
        dpp=sign(1.,(x-x1)*(x2-x1))*sqrt((x-x1)**2 + (a+b*x - y1)**2)
      else
        r=(xp*yp)/(sqrt((xp*xp)+(yp*yp)))
        dpp=sign(1.,(x-x1)*(x2-x1))*sqrt((x-x1)**2 + (a+b*x - y1)**2)
     +     + sign(1.,(y-a-b*x)*b*(x2-x1))*r*yp/xp
      endif
c
c  If dip = 90, d = r
c
      if(del.eq.90.) then
        d = r
      else
        d= r*sin(dip) + sign(1.,(x2-x1)*(y-a-b*x))*z*cos(dip)
      endif
      d=abs(d)
      return
      end
      subroutine depplt (xlat0, xlon0, x1, y1, x2, y2, del, dist, dep,
     +                   scal,vex,akmpin, nevs, ievs, nauth, auth,iprf,
     +                   icl,ylow,title)
c
      real*4 xlat0, xlon0
      real*4 r, d, dpp, amag(10000)
      real*4  rr(10000), dd(10000), zz(10000), ddpp(10000)
      integer iprf
      integer nevs
      integer ievs(nevs)
      integer nauth
      character*(*) auth(nauth)
      character*(*) title
      character*1   prf
      character*2   prf2
      character*11  profile
c
# 1 "/opt/antelope/5.2-64/include/EV_f.i" 1 

	integer	EV_NULL
        parameter (EV_NULL = (0))
	integer	EV_NAME
        parameter (EV_NAME = (1))
	integer	EV_NHYPOS
        parameter (EV_NHYPOS = (2))
	integer	EV_PREFHYPO
        parameter (EV_PREFHYPO = (3))
	integer	EV_HYPO_TIME
        parameter (EV_HYPO_TIME = (4))
	integer	EV_HYPO_LAT
        parameter (EV_HYPO_LAT = (5))
	integer	EV_HYPO_LON
        parameter (EV_HYPO_LON = (6))
	integer	EV_HYPO_DEPTH
        parameter (EV_HYPO_DEPTH = (7))
	integer	EV_HYPO_MB
        parameter (EV_HYPO_MB = (8))
	integer	EV_HYPO_MS
        parameter (EV_HYPO_MS = (9))
	integer	EV_HYPO_ML
        parameter (EV_HYPO_ML = (10))
	integer	EV_HYPO_AUTH
        parameter (EV_HYPO_AUTH = (11))
	integer	EV_HYPO_NSTAS
        parameter (EV_HYPO_NSTAS = (12))
	integer	EV_HYPO_ASSOCFLAG
        parameter (EV_HYPO_ASSOCFLAG = (13))
	integer	EV_HYPO_STA_STA
        parameter (EV_HYPO_STA_STA = (14))
	integer	EV_HYPO_STA_LAT
        parameter (EV_HYPO_STA_LAT = (15))
	integer	EV_HYPO_STA_LON
        parameter (EV_HYPO_STA_LON = (16))
	integer	EV_HYPO_STA_ELEV
        parameter (EV_HYPO_STA_ELEV = (17))
	integer	EV_HYPO_STA_NASSOCS
        parameter (EV_HYPO_STA_NASSOCS = (50))
	integer	EV_HYPO_STA_ASSOC_CHAN
        parameter (EV_HYPO_STA_ASSOC_CHAN = (18))
	integer	EV_HYPO_STA_ASSOC_PHASE
        parameter (EV_HYPO_STA_ASSOC_PHASE = (19))
	integer	EV_HYPO_STA_ASSOC_IPHASE
        parameter (EV_HYPO_STA_ASSOC_IPHASE = (20))
	integer	EV_HYPO_STA_ASSOC_TIME
        parameter (EV_HYPO_STA_ASSOC_TIME = (21))
	integer	EV_HYPO_STA_ASSOC_AZIMUTH
        parameter (EV_HYPO_STA_ASSOC_AZIMUTH = (22))
	integer	EV_HYPO_STA_ASSOC_INC
        parameter (EV_HYPO_STA_ASSOC_INC = (23))
	integer	EV_HYPO_STA_ASSOC_RECT
        parameter (EV_HYPO_STA_ASSOC_RECT = (24))
	integer	EV_HYPO_STA_ASSOC_DELTIME
        parameter (EV_HYPO_STA_ASSOC_DELTIME = (25))
	integer	EV_HYPO_STA_ASSOC_ASSOCFLAG
        parameter (EV_HYPO_STA_ASSOC_ASSOCFLAG = (26))
	integer	EV_HYPO_STA_ASSOC_DISTANCE
        parameter (EV_HYPO_STA_ASSOC_DISTANCE = (27))
	integer	EV_HYPO_STA_ASSOC_TIMERES
        parameter (EV_HYPO_STA_ASSOC_TIMERES = (28))
	integer	EV_HYPO_STA_ASSOC_SHAZ
        parameter (EV_HYPO_STA_ASSOC_SHAZ = (29))
	integer	EV_HYPO_STA_ASSOC_HSAZ
        parameter (EV_HYPO_STA_ASSOC_HSAZ = (30))
	integer	EV_HYPO_STA_ASSOC_AZRES
        parameter (EV_HYPO_STA_ASSOC_AZRES = (31))
	integer	EV_HYPO_STA_ASSOC_TIMEWGT
        parameter (EV_HYPO_STA_ASSOC_TIMEWGT = (32))
	integer	EV_HYPO_STA_ASSOC_AZWGT
        parameter (EV_HYPO_STA_ASSOC_AZWGT = (33))
	integer	EV_DBL
        parameter (EV_DBL = (34))
	integer	EV_TUPLE
        parameter (EV_TUPLE = (35))
	integer	EV_EVID
        parameter (EV_EVID = (36))
	integer	EV_HYPO_TUPLE
        parameter (EV_HYPO_TUPLE = (37))
	integer	EV_HYPO_ORID
        parameter (EV_HYPO_ORID = (38))
	integer	EV_HYPO_STA_ASSOC_ARID
        parameter (EV_HYPO_STA_ASSOC_ARID = (39))
	integer	EV_HYPO_STA_ASSOC_ASTUPLE
        parameter (EV_HYPO_STA_ASSOC_ASTUPLE = (40))
	integer	EV_HYPO_STA_ASSOC_ARTUPLE
        parameter (EV_HYPO_STA_ASSOC_ARTUPLE = (41))

	integer	EVSV_NULL
        parameter (EVSV_NULL = (0))
	integer	EVSV_HYPO_TIME
        parameter (EVSV_HYPO_TIME = (4))
	integer	EVSV_HYPO_LAT
        parameter (EVSV_HYPO_LAT = (5))
	integer	EVSV_HYPO_LON
        parameter (EVSV_HYPO_LON = (6))
	integer	EVSV_HYPO_DEPTH
        parameter (EVSV_HYPO_DEPTH = (7))
	integer	EVSV_HYPO_MB
        parameter (EVSV_HYPO_MB = (8))
	integer	EVSV_HYPO_MS
        parameter (EVSV_HYPO_MS = (9))
	integer	EVSV_HYPO_ML
        parameter (EVSV_HYPO_ML = (10))
	integer	EVSV_HYPO_AUTH
        parameter (EVSV_HYPO_AUTH = (11))
	integer	EVSV_HYPO_NSTAS
        parameter (EVSV_HYPO_NSTAS = (12))
	integer	EVSV_HYPO_ASSOCFLAG
        parameter (EVSV_HYPO_ASSOCFLAG = (13))
	integer	EVSV_HYPO_PREF
        parameter (EVSV_HYPO_PREF = (42))
	integer	EVSV_STA_STA
        parameter (EVSV_STA_STA = (14))
	integer	EVSV_STA_LAT
        parameter (EVSV_STA_LAT = (15))
	integer	EVSV_STA_LON
        parameter (EVSV_STA_LON = (16))
	integer	EVSV_STA_ELEV
        parameter (EVSV_STA_ELEV = (17))
        integer EVSV_STA_NSCVS
        parameter (EVSV_STA_NSCVS = (60))
        integer EVSV_STA_SCVS
        parameter (EVSV_STA_SCVS = (61))
	integer	EVSV_STA_NASSOCS
        parameter (EVSV_STA_NASSOCS = (50))
	integer	EVSV_STA_ASSOC_CHAN
        parameter (EVSV_STA_ASSOC_CHAN = (18))
	integer	EVSV_STA_ASSOC_PHASE
        parameter (EVSV_STA_ASSOC_PHASE = (19))
	integer	EVSV_STA_ASSOC_IPHASE
        parameter (EVSV_STA_ASSOC_IPHASE = (20))
	integer	EVSV_STA_ASSOC_TIME
        parameter (EVSV_STA_ASSOC_TIME = (21))
	integer	EVSV_STA_ASSOC_AZIMUTH
        parameter (EVSV_STA_ASSOC_AZIMUTH = (22))
	integer	EVSV_STA_ASSOC_INC
        parameter (EVSV_STA_ASSOC_INC = (23))
	integer	EVSV_STA_ASSOC_RECT
        parameter (EVSV_STA_ASSOC_RECT = (24))
	integer	EVSV_STA_ASSOC_DELTIME
        parameter (EVSV_STA_ASSOC_DELTIME = (25))
	integer	EVSV_STA_ASSOC_ASSOCFLAG
        parameter (EVSV_STA_ASSOC_ASSOCFLAG = (26))
	integer	EVSV_STA_ASSOC_DISTANCE
        parameter (EVSV_STA_ASSOC_DISTANCE = (27))
	integer	EVSV_STA_ASSOC_TIMERES
        parameter (EVSV_STA_ASSOC_TIMERES = (28))
	integer	EVSV_STA_ASSOC_SHAZ
        parameter (EVSV_STA_ASSOC_SHAZ = (29))
	integer	EVSV_STA_ASSOC_HSAZ
        parameter (EVSV_STA_ASSOC_HSAZ = (30))
	integer	EVSV_STA_ASSOC_AZRES
        parameter (EVSV_STA_ASSOC_AZRES = (31))
	integer	EVSV_STA_ASSOC_TIMEWGT
        parameter (EVSV_STA_ASSOC_TIMEWGT = (32))
	integer	EVSV_STA_ASSOC_AZWGT
        parameter (EVSV_STA_ASSOC_AZWGT = (33))
	integer	EVSV_DBL
        parameter (EVSV_DBL = (34))
	integer	EVSV_HYPO_TUPLE
        parameter (EVSV_HYPO_TUPLE = (37))
	integer	EVSV_HYPO_ORID
        parameter (EVSV_HYPO_ORID = (38))
	integer	EVSV_STA_ASSOC_ARID
        parameter (EVSV_STA_ASSOC_ARID = (39))
	integer	EVSV_STA_ASSOC_ASTUPLE
        parameter (EVSV_STA_ASSOC_ASTUPLE = (40))
	integer	EVSV_STA_ASSOC_ARTUPLE
        parameter (EVSV_STA_ASSOC_ARTUPLE = (41))


c $Id: grx_dep.f,v 1.11 2010-10-26 17:02:56 mitch Exp $ 
# 1564 "grx_dep_mac.F" 2 
      real*8 elat, elon, time, depth, sec
      character*80 author, alg, labelx, symbl
c
      symsiz = 0.05
      k = 0
c
      do 100  i = 1, nevs
	call evget (ievs(i), EV_NHYPOS, nhypos,
     +                       EV_PREFHYPO, ipref,
     +                       EV_EVID, ievid,
     +			 0)
	do 200  j = 1, nhypos
	  call evget (ievs(i), EV_HYPO_LAT, j, elat,
     +                       EV_HYPO_LON, j, elon,
     +                       EV_HYPO_DEPTH, j, depth,
     +                       EV_HYPO_TIME, j, time,
     +                       EV_HYPO_MB, j, xmb,
     +                       EV_HYPO_MS, j, xms,
     +                       EV_HYPO_ML, j, xml,
     +                       EV_HYPO_AUTH, j, author,
     +                       EV_HYPO_ORID, j, iorid,
     +                       0)
          xlat = elat
          xlon = elon
          z = depth/111.195
	  if (nauth .eq. 0) then
	  else
	    do 510  k = 1, nauth
	      if (author .eq. auth(k)) go to 511
  510       continue
	    go to 200
  511       continue
	  end if
          call latlon2xydel (xlat0, xlon0, xlat, xlon,
     +                         xdel, ydel)
          call depprf(xdel, ydel, z, x1, y1, x2, y2, del, r, d, dpp)
	  if (dist .ge. d*111.195) then
            k = k+1
            rr(k) = r*111.195
            dd(k) = d*111.195
            ddpp(k) = dpp*111.195
            zz(k) = z*111.195
            amag(k) = -999.0
            if(xml.ne.-999.0) then
              amag(k) = xml
            elseif(xmb.ne.-999.0) then
              amag(k) = xmb
            elseif(xms.ne.-999.0) then
              amag(k) = xms
            endif
	  end if
  200   continue
  100 continue
c     call e2h(time, iyear, iday, ihour, imin, sec)
c     write (6, '(a)') ' '
c     write (6, '(a,a)')    'AUTH:   ',author(1:ilen(author))
c     write (6, '(a,i8)')   'ORID:   ',iorid
c     write (6, '(a,i4,i3,1x,i2.2,a,i2.2,a,f5.2)')   
c    +                      'TIME:   ',iyear,iday,ihour,':',
c    +                   imin,':',sec
c     write (6, '(a,f9.4)') 'LAT:    ',elat
c     write (6, '(a,f9.4)') 'LON:    ',elon
c     write (6, '(a,f9.4)') 'DEPTH:  ',depth
c     write (6, '(a,f9.4)') 'MB:     ',xmb
c     write (6, '(a,f9.4)') 'MS:     ',xms
c     write (6, '(a,f9.4)') 'ML:     ',xml
c     xlat2 = elat
c     xlon2 = elon
c     call latlon2xydel (xlat0, xlon0, xlat2, xlon2,
c    +                         x, y)
      npprf = k
          call setchr(iprf,prf)
          zpmax=dep
          xpmax=sqrt((x2-x1)**2 + (y2-y1)**2) * 111.195
          xm = xpmax/2.
c         zpdim=5.
c         xpdim=6.
          zpdim=scal*vex*zpmax/akmpin
          xpdim=scal*xpmax/akmpin
          labelx="Distance Along Profile A - A' (km)"
          labelx(24:24) = prf
          labelx(28:28) = prf
          print*,xpdim,zpdim,xpmax,zpmax
          print*,labelx
          rad=6370.8
          xm=xpmax/2.
c         call setfor(0.,0.,0.)
          call axisdep(xpdim,zpdim,.6,.6,.8,ylow,xpmax,0.,0.,zpmax,xm,
     *    rad,1.,1.,1.,1.,'(i4)','(i4)',labelx,'Depth (km)',title,icl)
          call text(0.,0.,0.0,3,prf,1)
          prf2(1:1) = prf(1:1)
          prf2(2:2) = "'"
          call text(xpmax,0.,0.0,3,prf2,1)
c         call setfor(hev,lev,sev)
          do 659 i=1,npprf
            call depcon(ddpp(i),zz(i),xm,rad,xd,zd)
c      IF(AMAG(i).EQ.-999.0) THEN
c         CALL text(xd,zd,0.,4,'+',0)
c      ELSE
c           CALL circle(xd,zd,SYMSZ(i),50,0,0,.015,0)
c      ENDIF
            if (amag(i) .gt. -999.0) then
              ss = symsiz*(2.0 + 2.0*(amag(i)-2.0)/4.0)
              symbl = 'hexagon'
            else
              ss = symsiz
              symbl = 'box'
            end if
            call symbol (symbl, xd, zd, ss, 0.0, 0, 0)
659       continue
C
C   CLOSE PLOT
C
c     call finitt
      return
      end
      subroutine boxdep(xleft,xright,ybot,ytop,xm,rad,thick,ithick,
     *iclip)
c
c    routine box draws a box using a cylindrical projection
c
c    inputs  - xleft  = horizontal location of left edge of box in user units
c	       xright = horizontal location of right edge of box in user units
c	       ybot   = vertical location of bottom edge of box in user units
c	       ytop   = vertical location of top edge of box in user units
c              xm     = point about which to do cylindrical projection
c              rad    = radius for cylindrical projection
c	       thick  = thickness of lines making up box in inches
c              ithick = thickness flag
c	       iclip  = clip flag see routine plot1
c
      dimension x(61),y(61)
c
      dx=(xright-xleft)/29.
      do 1 i=1,30
        call depcon(xleft+(i-1)*dx,ytop,xm,rad,x(i),y(i))
        call depcon(xright-(i-1)*dx,ybot,xm,rad,x(30+i),y(30+i))
 1    continue
      x(61)=x(1)
      y(61)=y(1)
      call nplot(61,x,y,0,iclip,thick,ITHICK,' ')
c
      return
      end
      subroutine ticsdep(x1,y1,x2,y2,xm,rad,stic,ntic,dtic,tlen,
     *thick,idir)
c
c    routine ticsdep draws tic marks using a cylindrical projection
c
c    inputs  - x1     = x coordinate of starting point of axis in user units
c              y1     = y coordinate of starting point af axis in user units
c              x2     = x coordinate of ending point af axis in user units
c              y2     = y coordinate of ending point af axis in user units
c              xm     = x value about which compute cylindrical projection
c              rad    = radius for cylindrical projection
c              stic   = position along axis from (x1,y1) for first tic mark
c			in user units
c              ntic   = number of tic marks
c              dtic   = increment along axis for tic marks in user units
c              tlen   = length of tic marks IN INCHES
c              thick  = thickness of tic marks IN INCHES
c              idir   = flag which defines orientation of tic marks
c			(directions given for (x2,y2) directly to the
c			right of (x1,y1))
c			> 0 - tic marks point up
c			= 0 - tic marks point on both sides
c			< 0 - tic marks point down
c
      common /pdim/ xdim,ydim,xlow,ylow,rxdim,rydim,rxlow,rylow,
     1              xbl,ybl,xbh,ybh,xbm,ybm,itran,tangle,
     2              ca,sa,cellht,cellwd,ixtype,iytype
c
      common /pscl/ xmin,xmax,ymin,ymax,xscale,yscale,xrange,yrange
c
      common  /ocflag/  imflag,iocflg,iltp
      common /spc/ xll,yll,xur,yur,ASPECT,xc
      integer*4 ltp
c
      dimension x(3),y(3)
      dimension xd(3),yd(3)
c
c    xc = standard raster conversion factor in rasters per inch
c
      imfl = imflag
      ltp = iltp
      imflag = 0
      if (imfl .eq. 1)  call Nopen
      if (imfl .eq. 1)  call Nlstyl(ltp)
      xr1 = xmap(x1)
      yr1 = ymap(y1)
      xr2 = xmap(x2)
      yr2 = ymap(y2)
      dx = x2 - x1
      dy = y2 - y1
      axis = sqrt(dx**2 + dy**2)
      udx = dx/axis
      udy = dy/axis
      rdx = xr2 - xr1
      rdy = yr2 - yr1
      rrdx = rdx/axis
      rrdy = rdy/axis
      raxis = sqrt(rdx**2 + rdy**2)
      rxdtic = xc*tlen*(-rdy)/raxis
      rydtic = xc*tlen*rdx/raxis
      if (idir .lt. 0) rxdtic = -rxdtic
      if (idir .lt. 0) rydtic = -rydtic
c
      do 100 i = 1,ntic
      rxtic = xmap(x1 + (stic+dtic*(i-1))*udx)
      rytic = ymap(y1 + (stic+dtic*(i-1))*udy)
      x(1) = rxtic + rxdtic
      y(1) = rytic + rydtic
      x(2) = rxtic
      y(2) = rytic
      x(3) = rxtic - rxdtic
      y(3) = rytic - rydtic
      jplot = 2
      if (idir .eq. 0) jplot = 3
      do 110  j = 1,jplot
      x(j) = (x(j) - rxlow)/xscale + xmin
      y(j) = (y(j) - rylow)/yscale + ymin
      if (ixtype .eq. 1) x(j) = 10.**x(j)
      if (iytype .eq. 1) y(j) = 10.**y(j)
      call depcon(x(j),y(j),xm,rad,xd(j),yd(j))
  110 continue
  100 call nplot(jplot,xd,yd,0,1,thick,0,' ')
      if (imfl .eq. 1)  call Nclose
      imflag = imfl
c
      return
      end
      subroutine setchr(ichr,chr)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c Subroutine to set chr to the capitol ichr'th letter in Alphabet
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      character*1 chr
      character*26 chr26
      integer     ichr
c
      chr26 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      chr = chr26(ichr:ichr)
      return
      end
