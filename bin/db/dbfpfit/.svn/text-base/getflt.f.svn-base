      subroutine getflt(az1, dp1, az2, dp2, dpdir, dip, rad, rake)
c-----   given the poles to two perpendicular planes, compute the
c        dip direction, dip, and rake of the fault
c             az1            azimuth of pole to first plane
      real    az1
c             az2            azimuth of pole to second plane
      real    az2
c             dp1            dip of pole of first plane
      real    dp1
c             dp2            dip of pole of second plane
      real    dp2
c             dpdir          dip direction of fault plane
      real    dpdir
c             dip            dip of fault plane
      real    dip
c             rake           rake of fault plane    
      real    rake
c             strike         strike of fault plane
      real    strike
c           
      dip = 90. - dp1
      if(abs(dip-0.) .ge. .00001) then
        dpdir = az1 + 180.
        if(dpdir .gt. 360.) dpdir = dpdir - 360.
        if(dpdir .lt. 0.) dpdir = dpdir + 360.
        strike = dpdir - 90.
c
c use distaz to compute rake wrt first nodal plane
c note: input in radians, output in degrees
c
c       call distaz(p2lat,      p2lon, lat,        lon,dedeg,  az0,  
c    *  az1)
        call distaz(-dp2*rad, az2*rad, 0.0, strike*rad, rake, dum1, 
     *  dum2)
        if(dp2 .gt. 0.) rake = -rake
      else
c       if dip is near zero, use aux plane
        dip = 90. - dp2
        dpdir = az2 + 180.
        if(dpdir .gt. 360.) dpdir = dpdir - 360.
        if(dpdir .lt. 0.) dpdir = dpdir + 360.
        strike = dpdir - 90.
        call distaz(-dp1*rad, az1*rad, 0.0, strike*rad, rake, dum1, 
     *  dum2)
        if(dp1 .gt. 0.) rake = -rake
      endif
c
      return
      end

