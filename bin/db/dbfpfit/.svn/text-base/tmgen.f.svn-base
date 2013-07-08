      subroutine tmgen (azp, dpdir, dip, dpp, dpr, mxcase, nsec,
     * rad, rake, sechi, secinc, seclow, tm)

c j. c. lahr   10/26/86
c generate seismic moment tensor and fault dip direction, dip, and
c rake for a given p axis and a given range of t axes.
c
      dimension aazbi(100), nazb(100)
c              azp        (input) azimuth of p axis
      real     azp
c              dip(mxcase)   (output) dip of fault
      real     dip(mxcase)
c              dpdir(mxcase) (output) dip direction of fault
      real     dpdir(mxcase)
c              dpp        (input) dip of p axis
      real     dpp
c              mxcase     (input) maximum number of cases
      integer  mxcase
c              nsec       (input) number of t azis increments
      integer  nsec
c              rake(mxcase) (output) rake of fault motion
      real     rake(mxcase)
c              sechi      (input) hi limit of t axis range
      real     sechi
c              secinc     (input) increment in t axis
      real     secinc
c              seclow     (input) low limit of t axis range
      real     seclow
c              tm(6, mxcase) seismic moment tensor (up, south, east)
      real     tm(6, mxcase)
c              tmm(6)     one tm matrix
      real     tmm(6)
c
c
c-----    loop through t axis orientations
          sec = seclow - secinc
          nsec = (sechi-seclow)/secinc + .01
c         print *, 'p axis azp, dpp = ', azp, dpp
c         print *, 'number of orientations of t axis'
c         print *, 'for each p axis orientation is ', nsec+1
c         print *, 'the increment is ', secinc
c         print *, 'now computing the trial solutions.'
c         print *, 'ncase, dpdir, dip, rake, azp, dpp, azt, dpt'
          do 500 ncase = 1, nsec+1
            sec = sec + secinc
c
c call get3 to get the secondary axis
c
            call get3(sec, azp, dpp, dpr, azt, dpt, taz, tdp, rad)
c           print *, 'sec, azp, dpp, dpr, azt, dpt, taz, tdp, rad'
c           print *,  sec, azp, dpp, dpr, azt, dpt, taz, tdp, rad
c
c   call getpol to convert t and p axes into poles to nodal planes 
c
            call getpol(azt, dpt, azp, dpp, az2, dp2, az3, dp3, rad, 
     *      dpr)
c           print *, 'azt, dpt, azp, dpp, az2, dp2, az3, dp3'
c           print *,  azt, dpt, azp, dpp, az2, dp2, az3, dp3
c
c   call getflt to convert two poles into a shear fault
c
            call getflt(az2, dp2, az3, dp3, dpdir(ncase), dip(ncase), 
     *      rad, rake(ncase) )
c
c   call shrflt1 to convert this shear fault into a tm matrix
c
            call shrflt1 (rad*dpdir(ncase), rad*dip(ncase), 
     *      rad*rake(ncase), tmm)
            do 400 i = 1, 6
              tm(i, ncase) = tmm(i)
400         continue
c
c   print out tm matrix
c
c           print '(i5, 7f10.2)',  ncase, dpdir(ncase), dip(ncase),
c    *      rake(ncase), azp, dpp, azt, dpt
c
500   continue
      return
      end
