      subroutine refrmt (del, idip, idipdr, islip, phis, xlam)
c
c  reformat dip, strike, and rake angles to integer values and convert strike to down-dip direction
c
      real              del                             ! (input) fault dip angle in degrees
      integer           idip                            ! (output) fault dip angle in degrees
      integer           idipdr                          ! (output) dip direction in degrees
      integer           islip                           ! (output) rake in degrees
      real              phis                            ! (input) fault strike angle in degrees
      real              xlam                            ! (input) fault rake angle in degrees
c
      integer           istrk                           ! strike of best fit
c
      idip = ifix(del)
      istrk = ifix(phis)
      islip = ifix(xlam)
      if (idip .gt. 90) then
        idip = 180 - idip
        istrk = istrk + 180
        islip = -islip
      else if (idip .lt. 0) then
        idip = -idip
        istrk = istrk + 180
        islip = islip + 180
      end if
c     Mitch change jmod with mod
c     idipdr = jmod(istrk + 90, 360)
      idipdr = mod(istrk + 90, 360)
      if (idipdr .lt. 0) idipdr = idipdr + 360
c     Mitch change jmod with mod
c     islip = jmod(islip, 360)
      islip = mod(islip, 360)
      if (islip .gt. 180) islip = islip - 360
      if (islip .lt. -180) islip = islip + 360
c
      return
      end
c
c
c
c
c

