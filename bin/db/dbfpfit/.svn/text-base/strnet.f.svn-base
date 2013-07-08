      subroutine strnet (cx, cy, rad, rmax)
c
c plot perimeter of a stereo net
c
      real              cx                              ! x position of circle center
      real              cy                              ! y position of circle center
      real              rad                             ! pi/180
      real              rmax                            ! radius of circle

      real              csiz                            ! scratch variable (rmax/100)
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
      call newpen (2)
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
          call plot (x, y, 3)
          go to 10
        endif
        call plot (x, y, 2)
        if (n .ne. 0 .and. nn .ne. 0 .and. rmax .lt. 1.5) 
     *   go to 10
        p = 0.01*rmax          
        if  (n .eq. 0) p = 0.02*rmax
        if (nn .eq. 0) p = 0.05*rmax
        xp = (rmax + p)*cos(phi) + cx
        yp = (rmax + p)*sin(phi) + cy
  
        call plot (xp, yp, 2)
        call plot (x, y, 2)
  
10    continue
c
c plot + at center
c
      csiz = .01*rmax
      call plot (cx - csiz, cy, 3)
      call plot (cx + csiz, cy, 2)
      call plot (cx, cy - csiz, 3)
      call plot (cx, cy + csiz, 2)
      call newpen (1)
c
      return
      end
c
c
c
c
c

