      subroutine flippl ( irefp, da, dd, vd, pi, rad)
c
c revises dip angle and direction for horizontal view in vd direction
c
      real              da         ! dip angel (degrees)
      real              dap        ! revised dip angel (degrees)
      real              dd         ! dip direction (degrees)  (degrees)
      real              ddp        ! revised dip direction (degrees)
      integer           irefp      ! reference pole (+1 for north, -1 for south)
      real              pi         ! pi (radians)
      real              rad        ! pi/180 (radians/degree)
      real              vd         ! direction of view (degrees)
c
!d     print *, ' da  dd rad irefp ', da, dd, rad, irefp
      if(irefp .lt. 0) then
        dd = dd + 180.
      endif
      vlat = 0.
      call delaz( 0., (180.+vd)*rad, dekm, dedeg, azm,
     *    (90.-da)*rad, dd*rad)
      dap = dedeg
      ddp = azm
      if(irefp .lt. 0) then
        ddp = ddp + 180.
      endif
      if (ddp .lt. 0.0) ddp = ddp + 360.0
      if (ddp .gt. 360.) ddp = ddp - 360.0
      if (dap .gt. 90.) then
        dap = 180. - dap
        ddp = ddp + 180        
        if (ddp .gt. 360.) ddp = ddp - 360.0d0
      endif
      da = dap
      dd = ddp
!d     print *, 'new da  dd ', da, dd
      return
      end
c
c
c
c
c

