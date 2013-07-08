      subroutine get3(azn1wb, azb, dpb, dpr, az2, dp2, az3, dp3, rad)
c-----   given one direction and and angle, compute two additional
c        directions forming an orthogonal set
c              az0        direction to shift axis
c              azb        azimuth of null (b) axis - degrees
      real     azb
c              azn1wb     orientation of the n1 plane as it passes
c                         through the null (b) axis - degrees
      real     azn1wb    
c              blat       pseudo latitude of original axis - radians
      real     blat
c              blon       pseudo longitude of original azis - radians
      real     blon     
c              dpb        dip of null (b) axis - degrees
      real     dpb
c              shift      distance to move to reach next pole (degrees)
      real     shift      
c-----   compute true azimuth and dip of first nodal plane
c        travel 90 degrees along n2 from b axis
c      print *, 'azn1wb, azb, dpb ', azn1wb, azb, dpb
      shift = 90.
      blat = -dpb*rad
      blon = azb*rad
      call back1 (shift, azn1wb-90., p1lat, p1lon, blat, blon)
      az2 = p1lon*dpr
      dp2 = -p1lat*dpr
c      print *, 'az2, dp2 ', az2, dp2
      if (dp2 .lt. 0.) then
        dp2 = -dp2
        az2 = az2 + 180.
        if (az2 .gt. 360.) az2 = az2 - 360.
      endif
c      print *, 'az2, dp2 ', az2, dp2
      if (dp2 .gt. 90.) then
        dp2 = 180. - dp2
        az2 = az2 + 180.
        if (az2 .gt. 360.) az2 = az2 - 360.
      endif
c      print *, 'az2, dp2 ', az2, dp2
c
c-----   compute true azimuth and dip of second nodal plane
      if (abs(blat) .lt. 0.0001) then
c       print *, 'blat is small'
        if (abs(dp2) .lt. 0.0001) then
c         print *, 'dp2 is lt .0001'
          dp2 = 0.
          dp3 = 90.
          az3 = az2
        else if (abs(dp2-90.) .lt. 0.0001) then
c         print *, 'dp2 is close to 90.'
          dp2 = 90.
          dp3 = 0.
          call back1 (shift, azn1wb+180., p2lat, p2lon, blat, blon)
          az3 = p2lon*dpr
          az2 = az3
        else
c         print *, 'dp2 is not near 90 or 0.  dp2 = ', dp2
          call back1 (shift, azn1wb+180., p2lat, p2lon, blat, blon)
          az3 = p2lon*dpr
          dp3 = -p2lat*dpr
        endif          
      else
c       travel 90 degrees along n1 from b axis
        call back1 (shift, azn1wb+180., p2lat, p2lon, blat, blon)
        az3 = p2lon*dpr
        dp3 = -p2lat*dpr
      endif
      if (dp3 .lt. 0.) then
        dp3 = -dp3
        az3 = az3 + 180.
        if (az3 .gt. 360.) az3 = az3 - 360.
      endif
      if (dp3 .gt. 90.) then
        dp3 = 180. - dp3
        az3 = az3 + 180.
        if (az3 .gt. 360.) az3 = az3 - 360.
      endif
      return
      end
