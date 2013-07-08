      subroutine pltt(x,y,i)
c      each plot package must
c      draw lines
c      keep track of origin changes
c      keep track of maximum limits of plot
c      keep track of scale factors
c
c      i = +- 1      same as previous call
c        = +2        move with pen down
c        = +3        move with pen up
c        = -2 or -3  shift origin to new location of pen
c                    (102 and 103 get converted to -2 and -3)
c      i = 115 to change versatec pen size (x is size of pen)
c      i = 200 to make initial call to versatec plotter (plots)
c
c	write(95,*) '---> called pltt with:'
c	write(95,*) 'x, y, i ='
c	write(95,*) x, y, i
c
      data iold/3/
      ipass = i
      if(iabs(i) .eq. 1) ipass = iold
      iold = ipass
      call plotvx(x,y,ipass)
c	write(95,*) '---> returned from pltt'
      return
      end
