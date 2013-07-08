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
      call chrsiz(.2,1.,0.)
      y2 = y + .2
      call text(.5,y2,0.,3,title,1)
      call chrsiz(.14,1.,0.)
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
      call ticsdep(xmn,ymin,xmx,ymin,xm,rad,stic,ntics,DDXSML,.10,0.,
     *iref)
      iref = -iref
      call ticsdep(xmn,ymax,xmx,ymax,xm,rad,stic,ntics,DDXSML,
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
      call ticsdep(xmn,ymin,xmx,ymin,xm,rad,stic,ntics,DDXBIG,.17,0.,
     *iref)
      iref = -iref
      call ticsdep(xmn,ymax,xmx,ymax,xm,rad,stic,ntics,DDXBIG,
     1  .17,0.,iref)
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
      call ticsdep(xmin,xmn,xmin,xmx,xm,rad,stic,ntics,DDYSML,.10,0.,
     *iref)
      iref = -iref
      call ticsdep(xmax,xmn,xmax,xmx,xm,rad,stic,ntics,DDYSML,
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
      call ticsdep(xmin,xmn,xmin,xmx,xm,rad,stic,ntics,DDYBIG,.17,0.,
     *iref)
      iref = -iref
      call ticsdep(xmax,xmn,xmax,xmx,xm,rad,stic,ntics,DDYBIG,
     1  .17,0.,iref)
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
      subroutine symbol(xc,yc,r1,r2,az,nsymb,iclose,iclip,thick,ithick)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c    routine symbol will draw either a open or closed symbol (open meaning
c    a symbol with a white center, closed a symbol with a black center)
c
c    inputs  - xc     = x-coordinate of the center of the symbol in user units
c	       yc     = y-coordinate of the center of the symbol in user units
c              r1     = semi-major axis of the symbol in inches
c              r2     = semi-minor axis of the symbol in inches
c              az     = azimuth (in degrees) of the semi-major axis from the 
c			vertical (north)
c              nsymb  = symbol number 
c			1  -  circle
c			2  -  Ellipse
c			3  -  triangle
c			4  -  box
c			5  -  six point star
c			6  -  sextagon
c			7  -  eight point star
c			8  -  octagon
c			9  -  downward arrow
c			10 -  Plus sign
c			11 -  circle repeated
c			12 -  ellipse repeated
c			13 -  encircled triangle
c			14 -  encircled box
c			15 -  encircled six point star
c			16 -  encircled sextagon
c			17 -  encircled eight point star
c			18 -  encircled octagon
c			19 -  encircled arrow (not yet implemented)
c			20 -  encircled plus sign
c              iclose = open-closed flag
c			= 0 - symbol open
c			.ne. 0 - symbol closed
c              iclip  = clip flag see nplot
c              thick  = line thickness see nplot
c			note: this is passed to nplot along with iclip.
c 			      This allows big open symbols with thick
c			      circumferences. Probably should not be used
c			      for closed symbols.
c              ithick = thickness flag see nplot
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      common /pscl/ xmin,xmax,ymin,ymax,xscale,yscale,xrange,yrange
c
      common /spc/ xll,yll,xur,yur,ASPECT,rc
c
      data rad / 0.0174532925 /
c
      go to (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),nsymb
      write(6,'(1x,a)')'Error : Symbol :: Proper symbol not chosen!'
c
 1    call ellipse(xc,yc,r1,r2,az,99,iclose,iclip,thick,ithick)
      go to 99
c
 2    call ellipse(xc,yc,r1,r2,az,99,iclose,iclip,thick,ithick)
      go to 99
c
 3    call ellipse(xc,yc,r1,r2,az,3,0,iclip,thick,ithick)
      if(iclose.ne.0) then
        dr=r1/2.
        nit=int((r1-dr)/.01)
        if(nit.gt.50)nit=50
        r11=r1
        r22=r2
        do 100 i=1,nit
          r11=r11-.01
          r22=r22-.01
          call ellipse(xc,yc,r11,r22,az,3,0,iclip,thick,ithick)
100     continue
        call ellipse(xc,yc,r11,r22,az,3,iclose,iclip,thick,ithick)
      endif
      go to 99
c
 4    call ellipse(xc,yc,r1,r2,az,4,iclose,iclip,thick,ithick)
      go to 99
c
 5    call ellipse(xc,yc,r1,r2,az,3,0,iclip,thick,ithick)
      call ellipse(xc,yc,r1,r2,az+180.,3,0,iclip,thick,ithick)
      if(iclose.ne.0) then
        dr=r1/2.
        nit=int((r1-dr)/.01)
        if(nit.gt.50)nit=50
        r11=r1
        r22=r2
        do 200 i=1,nit
          r11=r11-.01
          r22=r22-.01
          call ellipse(xc,yc,r11,r22,az,3,0,iclip,thick,ithick)
          call ellipse(xc,yc,r11,r22,az+180.,3,0,iclip,thick,ithick)
200     continue
        call ellipse(xc,yc,r11,r22,az,3,iclose,iclip,thick,ithick)
        call ellipse(xc,yc,r11,r22,az+180.,3,iclose,iclip,thick,ithick)
      endif
      go to 99
c
 6    call ellipse(xc,yc,r1,r2,az,6,iclose,iclip,thick,ithick)
      go to 99
c
 7    call ellipse(xc,yc,r1,r2,az,4,iclose,iclip,thick,ithick)
      call ellipse(xc,yc,r1,r2,az+45.,4,iclose,iclip,thick,ithick)
      go to 99
c
 8    call ellipse(xc,yc,r1,r2,az,8,iclose,iclip,thick,ithick)
      go to 99
 9    call niceinq(x1,x2,x3,x4,x5,x6,x7,x8,hi,ra,sl)
      call chrsiz(2.*r1+.03,r2/r1,0.)
      call cfont(21)
      call text(xc,yc,az,3,'z',iclip)
      call cfont(9)
      call chrsiz(hi,ra,sl)
      go to 99
c
 10   dyc=r1*rc/yscale*cos(az*rad)
      dxc=r1*rc/xscale*sin(az*rad)
      call line(xc+dxc,yc+dyc,xc-dxc,yc-dyc,thick,ithick,iclip)
      dxc=r2*rc/xscale*cos(az*rad)
      dyc=r2*rc/yscale*sin(az*rad)
      call line(xc+dxc,yc-dyc,xc-dxc,yc+dyc,thick,ithick,iclip)
c     call niceinq(x1,x2,x3,x4,x5,x6,x7,x8,hi,ra,sl)
c     call chrsiz(r1+.07,r2/r1,0.)
c     call cfont(21)
c     call text(xc,yc,az,4,'e',iclip)
c     call cfont(9)
c     call chrsiz(hi,ra,sl)
      go to 99
c
 11   call ellipse(xc,yc,r1,r2,az,99,iclose,iclip,thick,ithick)
      go to 99
c
 12   call ellipse(xc,yc,r1,r2,az,99,iclose,iclip,thick,ithick)
      go to 99
c
 13   call ellipse(xc,yc,r1,r2,az,3,0,iclip,thick,ithick)
      if(iclose.ne.0) then
        dr=r1/2.
        nit=int((r1-dr)/.01)
        if(nit.gt.50)nit=50
        r11=r1
        r22=r2
        do 300 i=1,nit
          r11=r11-.01
          r22=r22-.01
          call ellipse(xc,yc,r11,r22,az,3,0,iclip,thick,ithick)
300     continue
        call ellipse(xc,yc,r11,r22,az,3,iclose,iclip,thick,ithick)
      endif
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 14   call ellipse(xc,yc,r1,r2,az,4,iclose,iclip,thick,ithick)
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 15   call ellipse(xc,yc,r1,r2,az,3,0,iclip,thick,ithick)
      call ellipse(xc,yc,r1,r2,az+180.,3,0,iclip,thick,ithick)
      if(iclose.ne.0) then
        dr=r1/2.
        nit=int((r1-dr)/.01)
        if(nit.gt.50)nit=50
        r11=r1
        r22=r2
        do 400 i=1,nit
          r11=r11-.01
          r22=r22-.01
          call ellipse(xc,yc,r11,r22,az,3,0,iclip,thick,ithick)
          call ellipse(xc,yc,r11,r22,az+180.,3,0,iclip,thick,ithick)
400     continue
        call ellipse(xc,yc,r11,r22,az,3,iclose,iclip,thick,ithick)
        call ellipse(xc,yc,r11,r22,az+180.,3,iclose,iclip,thick,ithick)
      endif
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 16   call ellipse(xc,yc,r1,r2,az,6,iclose,iclip,thick,ithick)
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 17   call ellipse(xc,yc,r1,r2,az,4,iclose,iclip,thick,ithick)
      call ellipse(xc,yc,r1,r2,az+45.,4,iclose,iclip,thick,ithick)
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 18   call ellipse(xc,yc,r1,r2,az,8,iclose,iclip,thick,ithick)
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 19   go to 99
c
c20   call niceinq(x1,x2,x3,x4,x5,x6,x7,x8,hi,ra,sl)
c     call chrsiz(r1+.07,r2/r1,0.)
c     call cfont(21)
c     call text(xc,yc,az,4,'e',iclip)
c     call cfont(9)
c     call chrsiz(hi,ra,sl)
 20   dyc=r1*rc/yscale*cos(az*rad)
      dxc=r1*rc/xscale*sin(az*rad)
      call line(xc+dxc,yc+dyc,xc-dxc,yc-dyc,thick,ithick,iclip)
      dxc=r2*rc/xscale*cos(az*rad)
      dyc=r2*rc/yscale*sin(az*rad)
      call line(xc+dxc,yc-dyc,xc-dxc,yc+dyc,thick,ithick,iclip)
      rx=r1+thick
      ry=r2+thick
      call ellipse(xc,yc,rx,ry,az,99,0,iclip,thick,ithick)
      go to 99
c
 99   return
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
