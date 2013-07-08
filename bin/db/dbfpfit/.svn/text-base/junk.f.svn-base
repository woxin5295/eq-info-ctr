      parameter (pi = 3.1415926536)
      parameter (rad = pi/180.)

      idpdr2 = -77
      idip2 = 13
      islip2 = 90
      idpdr2 = iaskk('dip direction', idpdr2)
      idip2 = iaskk('dip', idip2)
      islip2 = iaskk('rake', islip2)

c get the other nodal plane and rake
            call auxpln (float(idpdr2), float(idip2), float(islip2),
     1      dd2, da2, sa2)
c get the p and t axes
            call tandp
     1        (ain3, ain1, az3, az1, float(idip2), da2,
     1        float(idpdr2), dd2, pi, rad)
c switch p and t if faulting is normal
            if(islip2 .lt. 0.) then
              tempv = ain1
              ain1 = ain3
              ain3 = tempv
              tempv = az1
              az1 = az3
              az3 = tempv
            endif
 
c switch from angle of incidence to dip
            pl1 = 90. - ain1
            pl3 = 90. - ain3
            if(pl1 .lt. 0.0) then
              pl1 = abs(pl1)
              az1 = az1 + 180.
              if(az1 .gt. 360.) az1 = az1 - 360.
            endif
            if(az1 .lt. 0.0) az1 = 360. + az1
            pl3 = 90. - ain3
            if(pl3 .lt. 0.0) then
              pl3 = abs(pl3)
              az3 = az3 + 180.
              if(az3 .gt. 360.) az3 = az3 - 360.
            endif
            if(az3 .lt. 0.0) az3 = 360. + az3
 
c cross multiply to get b (null) axis
            call cromult
     1        (az1, pl1, az3, pl3, az2, pl2)
	    write (6, '(a)') 'old method'
            write (6, 132) int(dd2), int(da2), int(sa2), fitlim,
     1        int(az1), int(pl1), int(az2), int(pl2),
     1        int(az3), int(pl3)
132         format(
     1' c*  dd dp  rk fitlim pbt = az1 pl1  az2 pl2  az3 pl3', /,
     1' c*'i4, i3, i4, f6.3, 7x, 3(i4, i4, 1x))

c use tandpnew to do what auxpln and tandp used to do
            call tandpnew(float(idpdr2), float(idip2), float(islip2),
     *        dd2, da2, sa2, az1, ain1, az3, ain3)
c
 
c switch from angle of incidence to dip
            pl1 = 90. - ain1
            pl3 = 90. - ain3
            if(pl1 .lt. 0.0) then
              pl1 = abs(pl1)
              az1 = az1 + 180.
              if(az1 .gt. 360.) az1 = az1 - 360.
            endif
            if(az1 .lt. 0.0) az1 = 360. + az1
            pl3 = 90. - ain3
            if(pl3 .lt. 0.0) then
              pl3 = abs(pl3)
              az3 = az3 + 180.
              if(az3 .gt. 360.) az3 = az3 - 360.
            endif
            if(az3 .lt. 0.0) az3 = 360. + az3
 
c cross multiply to get b (null) axis
            call cromult
     1        (az1, pl1, az3, pl3, az2, pl2)
	    write (6, '(a)') 'new method'
	    iaz1 = az1 + sign(0.5, az1)
	    iaz2 = az2 + sign(0.5, az2)
	    iaz3 = az3 + sign(0.5, az3)
	    idd2 = dd2 + sign(0.5, dd2)
	    ida2 = da2 + sign(0.5, da2)
	    isa2 = sa2 + sign(0.5, sa2)
            write (6, 133) idd2, ida2, isa2, fitlim,
     1        iaz1, int(pl1 + 0.5), iaz2, int(pl2 + 0.5),
     1        iaz3, int(pl3 + 0.5)
133         format(
     1' c*  dd dp  rk fitlim pbt = az1 pl1  az2 pl2  az3 pl3', /,
     1' c*'i4, i3, i4, f6.3, 7x, 3(i4, i4, 1x))

	stop
	end
