          subroutine aintodp(az1, ain1, az1p, dip1)
c switch from azimuth, angle of incidence to azimuth and plunge
c j. c. lahr
          if(ain1 .le. 90.) then
            dip1 = 90. - ain1
            az1p = az1
          else 
            dip1 = ain1 - 90.
            az1p = az1 + 180.
            if(az1p .ge. 360.) az1p = az1p - 360.
          endif
          if(az1p .lt. 0.) az1p = az1p + 360.
          return
          end
