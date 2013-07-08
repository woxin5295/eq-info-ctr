      subroutine tandpnew(dd1, da1, sa1, 
     *  dd2, da2, sa2, azp, ainp, azt, aint)
c
c     given a focal mechanism, find the other nodal plane and
c     the azimuth and angle of incidence of p & t axes
c
      real              az1       ! azimuth of p/t axis
      real              az2       ! azimuth of t/p axis
      real              ain1      ! angle of incidence of p/t axis
      real              ain2      ! angle of incidence of t/p axis
      real              dd1       ! dip direction of principle plane
      real              dd2       ! dip direction of auxilliary plane
      real              da1       ! dip angle of priniciple plane
      real              da2       ! dip angle of auxilliary plane
      real		sa1	  ! rake of first nodal plane
      real		sa2	  ! rake of second nodal plane
      real              alat1     ! "latitude" of pole to first nodal plane (radians)
      real              alon1     ! "longitude" of pole of first nodal plane (radians)
      real              alat2     ! "latitude" of pole to 2nd nodal plane (radians)
      real              alon2     ! "longitude" of pole of 2nd nodal plane (radians)
      real              az0       ! azimuth from pole of 2nd nodal plane
c                                   to pole of 1st nodal plane
      real              pi        ! 3.14...
      real              rad       ! radians per degree
      real		deg	  ! degrees per radian
c
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
c
c     print *, 'tandpnew arguments'
c     print *, 'dd1, da1, sa1, azp, ainp, azt, aint'
c     print *, dd1, da1, sa1, azp, ainp, azt, aint

c find specifications of second nodal plane
      call auxpln (dd1, da1, sa1, dd2, da2, sa2)

      alat1 = (90. - da1)*rad
      alon1 = dd1*rad
      alat2 = (90. - da2)*rad
      alon2 = dd2*rad

c     compute distance and azimth (deg) from pole of 2nd to pole of 1st plane
c     print *, 'alat1, alon1, alat2, alon2'
c     print *,  alat1*deg, alon1*deg, alat2*deg, alon2*deg 
      call delaz ( alat2, alon2, delkm, deldeg, az0, alat1, alon1)
c     print *,   'delkm, deldeg, az0'
c     print *,    delkm, deldeg, az0
c     print *, 'new az0 ', az0

c    move 45.0 degrees from alat2,alon2 (rad) in the azimth az0 degrees
c       and wind up at p or t lat and lon (radians)
      call back1 (45.0, az0, portlat, portlon, alat2, alon2)
      portlat = portlat*deg
      portlon = portlon*deg
      ain1 = 90. + portlat
      az1 = portlon
      if(portlat .gt. 0.0) then
        az1 = portlon + 180.
	if(az1 .ge. 360.) az1 = az1 - 360.
	ain1 = 90. - portlat
      endif
c     print *, 'first pole azimuth and aingle of incidence ',
c    *  az1, ain1

c now move 45 degrees in the opposite direction
      az0 = az0 + 180.

      call back1 (45.0, az0, portlat, portlon, alat2, alon2)
      portlat = portlat/rad
      portlon = portlon/rad
      ain2 = 90. + portlat
      az2 = portlon
      if(portlat .gt. 0.0) then
        az2 = portlon + 180.
        if(az2 .ge. 360.) az1 = az1 - 360.
	ain2 = 90. - portlat
      endif
c
c depending on rake, define P and T axes
      if(sa1 .lt. 0.) then
	azp = az1
	ainp = ain1
	azt = az2
	aint = ain2
      else
	azp = az2
	ainp = ain2
	azt = az1
	aint = ain1
      endif

c     print *, 'P and T azimuth and angle of incidence = '
c     print *, azp, ainp, azt, aint

      return
      end
