      subroutine strnt1 (cx, cy, rad, rmax)
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
c
c draw circle @ 5 degree increments
c
      do 10 i = 1, 73
        phi = float(i - 1)*rad*5.0
        x = rmax*cos(phi) + cx
        y = rmax*sin(phi) + cy
c        n = (i - 1) - ((i - 1)/2)*2
c        nn = (i - 1) - ((i - 1)/18)*18
c        if ((n .eq. 0) .and. (i .gt. 1)) then
c          p = 0.02*rmax
c        else ((nn .eq. 0) .and. (i .gt. 1)) then
c          p = 0.04*rmax
c        else
c          p = 0.01*rmax
c        end if
c        xp = (rmax + p)*cos(phi) + cx
c        yp = (rmax + p)*sin(phi) + cy
        if (i .gt. 1) then
          call plot (x, y, 2)
c       Mitch changed jmod to mod 
          if (mod(i - 1, 18) .eq. 0) then
            xp = (1.06*rmax)*cos(phi) + cx
            yp = (1.06*rmax)*sin(phi) + cy
            call plot (xp, yp, 2)
            call plot (x, y, 2)
          end if
        else
c go to the first point on sphere
          call plot (x, y, 3)
        end if
10    continue
c
c plot + at center
c
      csiz = .01*rmax
      call plot (cx - csiz, cy, 3)
      call plot (cx + csiz, cy, 2)
      call plot (cx, cy - csiz, 3)
      call plot (cx, cy + csiz, 2)
c
      return
      end
