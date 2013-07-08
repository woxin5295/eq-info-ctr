      subroutine tpplot (cx, cy, da1, dd1, dipmx, hite, pi, psym, 
     & rad, rmax, sa1, tsym, vd)
c
c plot p and t axes
c
      real              cx       ! x position of circle center
      real              cy       ! y position of circle center
      real              da1      ! dip angle of principle plane
      real              dd1      ! dip direction of principle plane
      real              hite     ! height of p,t symbol
      real              pi       ! pi
      real              rad      ! pi/180
      real              rmax     ! radius of circle
      real              sa1      ! rake of principle plane
c
      real              ain1     ! angle of incidence of p/t axis
      real              ain2     ! angle of incidence of t/p axis
      real              ang      ! angle of plot symbol
      real              az1      ! azimuth of p/t axis
      real              az2      ! azimuth of t/p axis
      character*8       blank    ! blank
      real              da2      ! dip angle of auxilliary plane
      real              dd2      ! dip direction of auxilliary plane
      real              psym     ! symbol for p axis
c                                  0   - no symbol
c                                 -1.  - call pltlin
c                                  1.  - plot 'P'
c                                  2.  - plot 'T'
c                                 other number - plot this symbol, ie 32.090
      real              sa2      ! strike of auxilliary plane
      character*1       syml1    ! p/t plot symbol 
      character*1       syml2    ! t/p plot symbol
      real              sym1     ! p/t plot symbol 
      real              sym2     ! t/p plot symbol
      real              tsym     ! symbol for t axis (see psym)
      real              vd       ! view direction
c
      parameter (ang = 0.0, blank = '    ')
c
c find auxilliary plane
c
      call auxpln (dd1, da1, sa1, dd2, da2, sa2)
d     print *, 'dd1, da1, sa1 ', dd1, da1, sa1
d      write(15, 905) dd1, da1, sa1
905   format(' dd1, da1, sa1 ', 3f10.1)
c
c find p and t axes
c
      call tandp (ain1, ain2, az1, az2, da1, da2, dd1, dd2, pi, rad)
      if (sa1 .lt. 0.) then
        sym1 = psym
        sym2 = tsym
        syml1 = 'P'
        syml2 = 'T'
      else
        sym1 = tsym
        sym2 = psym
        syml1 = 'T'
        syml2 = 'P'
      end if
c
c plot symbols
c
d     call transp
d     write(6, 20) tsym, psym, tsym, psym
d20     format('tsym   psym  /',a1,'/',a1,'/', 2o20)
d     write(6, 30) sym1, sym2, sym1, sym2
d30     format('sym1   sym2  /',a1,'/',a1,'/', 2o20)
d     print *, 'sym1   az1  ain1   vd   pi   rad'
d     print *, sym1, az1, ain1, vd, pi,  rad
d     print *, 'sym2 = ', sym2
      if(vd .ne. 999.) then 
        call flippl( -1, ain1, az1, vd, pi, rad)
d      print *, sym1, az1, ain1
      endif
      if(sym1 .ne. 0.) then
        if(sym1 .eq. -1.) then
d         print *, 'call pltlin with symbol = ', syml1         
          call pltlin (ain1, az1, cx, cy, dipmx, pi, rad, rmax, syml1)
        else
d         print *, 'call pltsym.  sym1, ain1, az1 ', sym1, ain1, az1
d          write(15, 910) sym1, ain1, az1
910       format('sym1, ain1, az1 ', f10.3, 2f10.1)
c         plot p and t as letters - pass to pltsym as 1 and 2
c         plot extra p's and t's on smaller focal plot as different symbols,
c           - pass 3 and 4 to pltsym
          call pltsym (ain1, az1, cx, cy, hite, blank, pi, rad, 
     &    rmax, sym1, 0.)
        endif
      endif
d     call transp
d     print *, sym2, az2, ain2
      if(vd .ne. 999.) then 
        call flippl( -1, ain2, az2, vd, pi, rad)
d      print *, sym2, az2, ain2
      endif
      if(sym2 .eq. 0.) return
      if(sym2 .eq. -1.) then
d       print *, 'call pltlin with symbol = ', syml2
        call pltlin (ain2, az2, cx, cy, dipmx, pi, rad, rmax, syml2)
      else
d       print *, 'call pltsym.  sym2, ain2, az2 ', sym2, ain2, az2
d        write(15, 912) sym2, ain2, az2
912     format('sym2, ain2, az2 ', f10.3, 2f10.1)
        call pltsym (ain2, az2, cx, cy, hite, blank, pi, rad, 
     &  rmax, sym2, 0)
      endif
c
      return
      end
