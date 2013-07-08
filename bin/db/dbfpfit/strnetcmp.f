      subroutine strnetcmp (cx, cy, rad, rmax)
c
c plot perimeter of a stereo net
c
      real              cx                              ! x position of circle center
      real              cy                              ! y position of circle center
      real              rad                             ! pi/180
      real              rmax                            ! radius of circle

      integer           i                               ! loop index over degrees
      integer           n                               ! tests 10 degree tick position
      integer           nn                              ! tests 90 degree tick position
      real              p                               ! tick length
      real              phi                             ! azimuth in radians
      real              x                               ! x postion of circle
      real              xp                              ! x position of end of tick
      real              y                               ! y postion of circle
      real              yp                              ! y position of end of tick
c
c draw circle @ 1 degree increments
c
      do 10 i = 1, 361, 2
        phi = (i - 1)*rad
        x = rmax*cos(phi) + cx
        y = rmax*sin(phi) + cy
        n = (i - 1) - ((i - 1)/10)*10
        nn = (i - 1) - ((i - 1)/90)*90
        if(i .eq. 1) then
	  write(1, *) x, y
          go to 10
        endif
	write(1, *) x, y
        if (n .ne. 0 .and. nn .ne. 0 .and. rmax .lt. 1.5) 
     *   go to 10
        p = 0.01*rmax          
        if  (n .eq. 0) p = 0.02*rmax
        if (nn .eq. 0) p = 0.05*rmax
        xp = (rmax + p)*cos(phi) + cx
        yp = (rmax + p)*sin(phi) + cy
  
	write(1, *) xp, yp
	write(1, *) x, y
  
10    continue
      return
      end
