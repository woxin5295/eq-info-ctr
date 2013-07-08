      subroutine pltsym (ain, az, cx, cy, ussiz, extname, pi, rad,
     &      rmax, ussym, prot)
c
c    plot either first motion symbol with station name next to symbol, 
c    or stress axes symbol (p & t) 
c    first motion symbols are as follows:
c
      real              ain      ! angle of incidence of symbol
      real              az       ! azimuth of symbol
      real              cx       ! x position of circle center
      real              cy       ! y position of circle center
      character*12      extname  ! string to be plotted to right of symbol
      real              pi       ! pi
      real              rad      ! pi/180
      real              rmax     ! radius of circle
      character*1       sym      ! plot symbol 
      character*6       svfont   ! original font
      real              wt       ! weight assigned to pick quality in program fpfit
      real              ainr     ! ain in radians
      real              azr      ! az in radians
      real              con      ! rmax * sqrt(2.0)
      logical           first    ! flag first time into routine
      real              r        ! distance from cx, cy to plot position
      real              ussiz    ! height of symbol to plot
      real              ussym    ! symbol type to plot
      real              usszp    ! use for spacing name from symbol (1.08*ussiz)
      real              x        ! x position of symbol
      real              y        ! y position of symbol
      real              prot     ! paper rotation: 0.00 --> +y up
c                                ! paper rotation:-pi/2 --> -x up

c
c
      azr = az*rad
      ainr = ain*rad
c
c upgoing rays
c
      if (ain .gt. 90.) then
        ainr = pi - ainr
        azr = pi + azr
      endif
      con = rmax*sqrt(2.0)
      r = con*sin(ainr*0.5)
      x = r*sin(azr + prot) + cx
      y = r*cos(azr + prot) + cy
c
c stress axis symbol for best solution
c
      if (ussym .eq. 1.) then
c       call getfon (svfont)
c       call setfon ('romtrp')
c       call centxt (x - sin(prot)*ussiz/2., y - cos(prot)*ussiz/2., 
c    *  ussiz, 'P', -prot/rad, 1)
        call symbol (x - sin(prot)*ussiz/2.3 - cos(prot)*ussiz/2.4, 
     *   y + sin(prot)*ussiz/2.4 - cos(prot)*ussiz/2.3, 
     *   ussiz, 'P', -prot/rad, 1)
c	print *, 'in pltsym, prot and rad are ', prot, rad
c       call setfon (svfont)
      else if (ussym .eq. 2.) then
c       call getfon (svfont)
c       call setfon ('romtrp')
c       call centxt (x - sin(prot)*ussiz/2., y - cos(prot)*ussiz/2., 
c    *  ussiz, 't', -prot/rad, 1)
        call symbol (x - sin(prot)*ussiz/2.3 - cos(prot)*ussiz/2.4, 
     *   y + sin(prot)*ussiz/2.4 - cos(prot)*ussiz/2.3, 
     *   ussiz, 'T', -prot/rad, 1)
c	print *, 'in pltsym, prot and rad are ', prot, rad
c       call setfon (svfont)
c
c stress axis symbol for alternate solution
      else if (ussym .eq. 1.1) then
        call symb ( 32.090, ussiz, x, y, 0., 0., 0.)
      else if (ussym .eq. 2.1) then
        call symb ( 32.270, ussiz, x, y, 0., 0., 0.)
c
c first motion symbol
c
      else
        call symb ( ussym, ussiz, x, y, 0., 0., 0.)
c
c plot station extname
c
        if (extname(1:4) .ne. ' ') then
          call left(extname)
          usszp = ussiz*1.08
c         call text (x + cos(prot)*usszp/2. - sin(prot)*usszp/4.,
c    &               y - cos(prot)*usszp/4. - sin(prot)*usszp/2.,
c    &    ussiz/2., extname, -prot/rad, lentru(extname))
          call symbol (x + cos(prot)*usszp/2. - sin(prot)*usszp/4.,
     &               y - cos(prot)*usszp/4. - sin(prot)*usszp/2.,
     &    ussiz/2., extname, -prot/rad, lentru(extname))
        endif
      endif
c
      return
      end
