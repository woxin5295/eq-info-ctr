      subroutine vecpol(flip, tr, pl, vl)
c vecpol.for   9/29/86    computes the lower hemisphere polar 
c                         representation of a line 
c                         from the vector representation.
c
c coordinate convention   x=north, y=east, and z=down
c
c             flip    (output) true = point flipped from upper to lower hemi
      logical flip    
c             tr      (output) trend of line (degrees colckwise from north)
c                     will range from 0 to 360 degrees
      real    tr      
c             pl      (output) plunge of line (degrees below horizontal)
c                     will range from 0 to 90 degrees
      real    pl      
c             vl(3)   (input) vector representation of line
      real    vl(3)   
c
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (deg = 1./rad)
c
      hlen = sqrt( vl(1)*vl(1) + vl(2)*vl(2) )
      if (vl(3) .lt. 0.) then
        flip = .true.
        pl = deg*atan2( -vl(3), hlen )
        tr = deg*atan2( -vl(2), -vl(1) )
      else
        flip = .false.
        pl = deg*atan2( +vl(3), hlen )
        tr = deg*atan2( +vl(2), +vl(1) )
      endif
      if(tr .lt. 0.) tr = tr + 360.
      return
      end
