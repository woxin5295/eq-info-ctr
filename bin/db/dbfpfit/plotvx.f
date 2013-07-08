      subroutine plotvx(x,y,in)
c     converted from plotbl for vax plot
      common /mbdx/ nofile, tracit, xmn, xmx, ymn, ymx, clipon
      logical tracit, clipon
c
c	write(95,*) '---> called plotvx with:'
c	write(95,*) 'x, y, in ='
c	write(95,*) x, y, in
c                                               
      i = in
      if (i .ge. 100) go to 100
   10 if(i .eq. -15) go to 50
      if(i .eq. 3) i = 13
      if(i .eq. 2) i = 12
      xnew = x
      ynew = y
      call pltclp (xold, yold, xnew, ynew, xmn, xmx, 
     *                   ymn, ymx, clipon, iclip)
      if(iclip .eq. 0) then
        call plot(xnew, ynew, in)
        if(tracit) write(14, 20) xnew, ynew, in
      else if(iclip .lt. 3) then
        if(i .eq. 12) then
          call plot(xold, yold, in)
          if(tracit) write(14, 20) xold, yold, in
          call plot(xnew, ynew, in)
          if(tracit) write(14, 20) xnew, ynew, in
        else
          call plot(xnew, ynew, in)
          if(tracit) write(14, 20) xnew, ynew, in
        endif 
      endif
      xold = x
      yold = y
20    format(2f12.6, 3x, i5)
c      	write(95,*) '---> returned from plotvx 1'
      return
c------ new pen size ----
   50 npen = x + .2
      call newpen(npen)
c      	write(95,*) '---> returned from plotvx 2'
      return
c------ check for special commands ----
  100 if(i .eq. 200) go to 500
      if ((i/100) .ne. 1) then
c      	write(95,*) '---> returned from plotvx 3'
      return
      end if
      i = 100 - i
      go to 10
c------ initialize plotting, origin offset, pen number
  500 write(nofile,510)                          
  510 format(1x,'----- initialize vax plotter:')
	call plots(0.,0.,0)
	call newpen(1)
c------ blank screen
c     call plot(0., 0., -999)
c     call plot(        -3)
c
c      	write(95,*) '---> returned from plotvx 4'
      return
      end
