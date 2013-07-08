c      call plots
c      call pltlin(       90., 0., 2., 5.,3.14, 3.14/180., 2., 'p') 
c      call pltlin(       90., 0., 4., 5.,3.14, 3.14/180., 2., 'p') 
c      call pltlin(       90., 0., 6., 5.,3.14, 3.14/180., 2., 'p') 
c      call pltlin(       90., 90., 2., 5.,3.14, 3.14/180., 2., 't') 
c      call pltlin(       90., 90., 4., 5.,3.14, 3.14/180., 2., 't') 
c      call pltlin(       90., 90., 6., 5.,3.14, 3.14/180., 2., 't') 
c      end
      subroutine pltlin (ain, az, cx, cy, dipmx, pi, rad, rmax, sym,
     * prot)
c
c    plot line with strike of p or t axis
c    add triangles to ends of t axis
c
      character*1       dnstrng
      real              ain           ! angle of incidence of symbol
      real              az            ! azimuth of symbol
      real              cx            ! x position of circle center
      real              cy            ! y position of circle center
      real              pi            ! pi
      real              rad           ! pi/180
      real              rmax          ! radius of circle
      character*1       sym           ! plot symbol 
      real              ainr          ! ain in radians
      real              azr           ! az in radians
      real              con           ! rmax * sqrt(2.0)
      real              r             ! distance from cx, cy to plot position
      real              x             ! x position of symbol
      real              xs(125)       ! symbol x values
      real              y             ! y position of symbol
      real              y2(125)       ! symbol y values
      real              prot          ! paper rotation: 0.00 --> +y up
c                                     ! paper rotation:-pi/2 --> -x up
c
c return if not nearly horizontal
c
      if (abs(ain-90.) .gt. dipmx) return
c
      azr = az*rad
      ainr = ain*rad
c
c upgoing rays
c
      if (ain .gt. 90.) then
        ainr = pi - ainr
        azr = pi + azr
      end if
c
      con = rmax*sqrt(2.0)
      r = con*sin(ainr*0.5)
      x = r*sin(azr + prot) + cx
      y = r*cos(azr + prot) + cy
c
c stress axis symbol
c
      if (dnstrng(sym) .eq. 'p') then
        call arbsymb(x, y, rmax/6., azr+pi, 3)
        call plot (x, y, 3)
        call plot (x, y, 2)
        x = r*sin(azr + pi + prot) + cx
        y = r*cos(azr + pi + prot) + cy
        call plot (x, y, 2)
        call arbsymb(x, y, rmax/6., azr, 3)
      else
        call plot (x, y, 3)
        call plot (x, y, 2)
        x = r*sin(azr + pi + prot) + cx
        y = r*cos(azr + pi + prot) + cy
        call plot (x, y, 2)
      endif
      return
      end
