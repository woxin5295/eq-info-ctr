      subroutine plotplcmp (cx, cy, dpidg, pi, rad, rmax, strkdg, prot)
c
c plots fault plane on lower hemisphere stereo net
c
      real              cx                              ! x position of circle center
      real              cy                              ! y position of circle center
      real              dpidg                           ! dip angle in degrees
      real              pi                              ! pi
      real              rad                             ! pi/180
      real              rmax                            ! radius of circle
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
        x = rmax*sin(strkrd + prot) + cx
        y = rmax*cos(strkrd + prot) + cy
	write(1, *) x, y
        x = rmax*sin(strkrd + pi + prot) + cx
        y = rmax*cos(strkrd + pi + prot) + cy
	write(1, *) x, y
        return
      end if
c
c compute angle of incidence, azimuth
c
      do 10 i = 1, 90
        ang = float(i - 1)*rad
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
      con = rmax*sqrt(2.)
      do 20 i = 1, 180
        if (i .le. 91) then
          mi = i
          az = saz(i) + strkrd
        else
          mi = 181 - i
          az = pi - saz(mi) + strkrd
        end if
        radius = con*sin(ainp(mi)*0.5)
        x = radius*sin(az + prot) + cx
        y = radius*cos(az + prot) + cy
	write(1, *) x, y
20    continue
c
      return
      end
