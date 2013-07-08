      subroutine tandp(ain1, ain2, az1, az2, da1, da2, dd1, dd2, pi,rad)
c
c     given two planes compute az and angle of incidence of p & t axes
c
      real              ain1      ! angle of incidence of p/t axis
      real              ain2      ! angle of incidence of t/p axis
      real              az1       ! azimuth of p/t axis
      real              az2       ! azimuth of t/p axis
      real              da1       ! dip angle of priniciple plane
      real              da2       ! dip angle of auxilliary plane
      real              dd1       ! dip direction of principle plane
      real              dd2       ! dip direction of auxilliary plane
      real              pi        ! pi
      real              rad       ! pi/180
      real              alat1     ! dip angle in radians of principle 
c                                   plane measured from vertical
      real              alat2     ! dip angle in radians of auxilliary 
c                                   plane measured from vertical
      real              alon1     ! dd1 in radians
      real              alon2     ! dd2 in radians
      real              azimth    ! azimuth in radians of pole ??
      real              az0       ! azimuth from pole of auxilliary 
c                                   plane to pole of principle ??
      real              bazm      ! not used
      real              delta     ! not used
      real              plunge    ! plunge in radians of pole ??
      real              shift     ! azimuthal shift from pole of plane
c                                   to p to t axis (= 45 degrees)?? 
      real              xpos      ! not used
      real              ypos      ! not used
c
      parameter (shift = 45.)
c
      alat1 = (90. - da1)*rad
      alon1 = -dd1*rad
      alat2 = (90. - da2)*rad
      alon2 = -dd2*rad
c     compute distance and azimth (deg) of 90.-da1,dd1 (rad) wrt 
c       90.-da2,dd2 (rad)
      call delaz ( alat2, alon2, delkm, deldeg, az0, alat1, alon1)
c      print *,    alat2, alon2, delkm, deldeg, az0, alat1, alon1
c      print *, 'new az0 ', az0
c     shift from alat2,alon2 (rad) by shift deg in azimth az0 deg
c       and wind up at plunge,azimth (rad)
      call back1 (shift, az0, plunge, azimth, alat2, alon2)
      azimth = -azimth
c      print *, 'new plunge, azimuth ', plunge/rad, azimth/rad      
      if (abs(azimth) .gt. pi) azimth = azimth - sign(2.0*pi, azimth)
      az1 = azimth/rad
      ain1 = plunge/rad + 90.      
      az0 = az0 + 180.
      call back1 (shift, az0, plunge, azimth, alat2, alon2)
      azimth = -azimth
      if (abs(azimth) .gt. pi) azimth = azimth - sign(2.0*pi, azimth)
      az2 = azimth/rad
      ain2 = plunge/rad + 90.     
c
      return
      end
c
c
c
c
c


