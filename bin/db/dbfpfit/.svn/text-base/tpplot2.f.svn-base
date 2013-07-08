      subroutine tpplot2 (cx, cy, da1, dd1, dipmx, pi, pc, 
     & rad, rmax, sa1, tc, vd, ain1, az1, syml1, ain2, az2, syml2,
     & prot, pctcht)
c
c plot p and t axes
c
      character*1       dnstrng
      real              cx       ! x position of circle center
      real              cy       ! y position of circle center
      real              da1      ! dip angle of principle plane
      real              dd1      ! dip direction of principle plane
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
      character*1       pc       ! character for p axis in alt. sphere
      real              pctcht   ! char height of alt. sphere char.
      real              psym     ! symbol for p axis
c                                  0   - no symbol
c                                 -1.  - call pltlin
c                                  1.  - plot 'p'
c                                  2.  - plot 't'
c                                 other number - plot this symbol, ie 32.090
      real              sa2      ! strike of auxilliary plane
      character*1       syml1    ! p/t plot symbol 
      character*1       syml2    ! t/p plot symbol
      real              sym1     ! p/t plot symbol 
      real              sym2     ! t/p plot symbol
      character*1       tc       ! character for t axis in alt. sphere
      real              tsym     ! symbol for t axis (see psym)
      real              vd       ! view direction
      real              prot     ! paper rotation: 0.00 --> +y up
c                                ! paper rotation:-pi/2 --> -x up
c
      parameter (ang = 0.0, blank = '    ')
c
c find auxilliary plane
c
      call auxpln (dd1, da1, sa1, dd2, da2, sa2)
c     print *, 'dd1, da1, sa1 ', dd1, da1, sa1
c
c find p and t axes
c
      call tandp (ain1, ain2, az1, az2, da1, da2, dd1, dd2, pi, rad)
c	print *, 'da1, da2, dd1, dd2 ', da1, da2, dd1, dd2
c	print *, 'ain1 az1 ain2 az2 = ', ain1, az1, ain2, az2
      psym = 0
      tsym = 0
      if (dnstrng(pc) .eq. 'p') psym = 1.
      if (dnstrng(tc) .eq. 't') tsym = 2.
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
      if(vd .ne. 999.) then 
        call flippl( -1, ain1, az1, vd, pi, rad)
      endif
      if(sym1 .ne. 0.) then
        if(sym1 .eq. -1.) then
          call pltlin (ain1, az1, cx, cy, dipmx, pi, rad, rmax, syml1,
     *    prot)
        else
c         plot p and t as letters - pass to pltsym as 1 and 2
c         plot extra p's and t's on smaller focal plot as different symbols,
c           - pass 3 and 4 to pltsym
          call pltsym (ain1, az1, cx, cy, pctcht, blank, pi, rad, 
     &    rmax, sym1, prot)
        endif
      endif
      if(vd .ne. 999.) then 
        call flippl( -1, ain2, az2, vd, pi, rad)
      endif
      if(sym2 .eq. 0.) return
      if(sym2 .eq. -1.) then
        call pltlin (ain2, az2, cx, cy, dipmx, pi, rad, rmax, syml2,
     *  prot)
      else
        call pltsym (ain2, az2, cx, cy, pctcht, blank, pi, rad, 
     &  rmax, sym2, prot)
      endif
c
      return
      end
