      subroutine polvec(tr, pl, vl)
c polvec.for   9/29/86    computes the vector representation of a line
c                         from its position defined by trend (degrees
c                         clockwise from north) and plunge (degrees below
c                         horizontal).
c
c coordinate convention   x=north, y=east, and z=down
c
c             tr      (input) trend of line (degrees colckwise from north)
      real    tr      
c             pl      (input) plunge of line (degrees below horizontal)
      real    pl      
c             vl(3)   (output) vector representation of line
      real    vl(3)   
c
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
c
      rpl = pl*rad
      rtr = tr*rad
      vl(1) = cos(rpl)*cos(rtr)
      vl(2) = cos(rpl)*sin(rtr)
      vl(3) = sin(rpl)     
      return
      end
