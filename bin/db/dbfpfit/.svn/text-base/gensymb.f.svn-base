      subroutine gensymb(xcenter, ycenter, ht, angzero, ityp, 
     &                   npts, x, y)
c     generate line draws for a figure with ncorn = npts-1 corners...
c     angzero is the initial angle for symbol.
      real x(*), y(*)
      parameter (pi=3.1415927, deg2rad=0.17453293e-1)

      anginc = deg2rad* 360./real(ityp)

      do 10 k = 1, ityp + 1
        npts = npts + 1
        ang = (k-1) * anginc + angzero
        x(npts) = xcenter + 0.5*ht*sin(ang)
        y(npts) = ycenter + 0.5*ht*cos(ang)
 10   continue

      return
      end
