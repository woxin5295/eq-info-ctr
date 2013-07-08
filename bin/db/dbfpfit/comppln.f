      subroutine comppln (dpidg, pi, rad, strkdg, prot)
c
c plots fault plane on lower hemisphere stereo net
c
      real              dpidg                           ! dip angle in degrees
      real              pi                              ! pi
      real              rad                             ! pi/180
      real              strkdg                          ! strike angle in degrees

      real              ang                             ! angle in radians
      real              ainp(91)                        ! angle of incidence in radians
      real              arg                             ! dummy argument
      real              az                              ! azimuth
      real              con                             ! radius coefficient
      real              diprd                           ! dip angle in radians
      integer           i                               ! loop index
      integer           mi                              ! scratch index
      real              radius                          ! radius
      real              saz(91)                         ! azimuth in radians
      real              strkrd                          ! strike in radians
      real              taz                             ! scratch variable
      real              tpd                             ! scratch variable
      real              x                               ! x plot position
      real              y                               ! y plot position
      real              prot                            ! paper rotation: 0.00 --> +y up
c                                                       ! paper rotation:-pi/2 --> -x up

c
      strkrd = strkdg*rad
      diprd = dpidg*rad
      tpd = tan(pi*.5 - diprd)**2
c
c case of vertical plane
c
      if (dpidg .eq. 90.0) then
	print *, strkdg, -.1
	print *, strkdg, -1.
	print *, strkdg, -45.
	print *, strkdg, -90.
	endstr = strkdg + 180.
	if (endstr .gt. 360.) endstr = endstr - 360.
	print *, endstr, -45.
	print *, endstr, -1.
	print *, endstr, -.1
        return
      end if
c
c compute angle of incidence, azimuth
c
      do 10 i = 1, 90, 1
        ang = float(i - 1)*rad + 0.001
        arg = sqrt((cos(diprd)**2)*(sin(ang)**2))/cos(ang)
        saz(i) = atan(arg)
        taz = tan(saz(i))**2
        arg = sqrt(tpd + tpd*taz + taz)
        ainp(i) = acos(tan(saz(i))/arg)
  10  continue
      saz(91) = 90.*rad
      ainp(91) = pi*.5 - diprd
c
c plot plane
c
      do 20 i = 1, 180, 1
        if (i .le. 91) then
          mi = i
          az = saz(i) + strkrd
        else
          mi = 181 - i
          az = pi - saz(mi) + strkrd
        end if
	aaz = az/rad
	adip = (pi/2. -ainp(mi))/rad
	if(aaz .gt. 360.) then
	  aaz = aaz - 360.
	endif
c make sure dip is negative (julian convention)
	if(adip .ge. 0.) then
	  adip = -adip
	  aaz = aaz + 180.
	  if(aaz .gt. 360.) aaz = aaz - 360.
	endif
	print *, aaz, adip
20    continue
c
      return
      end
