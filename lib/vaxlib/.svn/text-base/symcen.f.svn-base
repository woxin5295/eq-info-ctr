c DRAWS ONE HERSHEY ROMSIM CHARACTER CENTERED ON POINT X,Y AND ROTATED 
c  ANG DEGREES CLOCKWISE.
c J. C. LAHR  6/4/86
c
      subroutine symcen(x, y, hite, symb, ang)
      character symb*1, symbi*1, font*6, svfont*6
      dimension funct(65:90)
c WIDTH FACTORS FOR LETTERS A THROUGH Z
      equivalence (isymb, symbi)
      data funct / .428, .52, .495, .52, 3*.495, .52, .19, .33, .51, .47
     &, .57, .521, 4*.525, .48, .38, .523, .43, .57, .475, .42, .475 /
# 14 "symcen.for"
      symbi = symb
      rad = asin(1.) / 90.
      call getfon(svfont)
      call setfon('ROMSIM')
c       FOR LETTERS A TO Z
# 18 "symcen.for"
      if ((isymb .lt. 91) .and. (isymb .gt. 64)) then
# 20 "symcen.for"
      xc = hite * funct(isymb)
      else
      xc = hite * .5
      end if
      yc = hite * 0.5
      if (ang .eq. 0.) then
      call symbol(x - xc, y - yc, hite, symb, 0., 1)
      else
      angr = ang * rad
      xcp = (xc * cos(- angr)) - (yc * sin(- angr))
      ycp = (yc * cos(- angr)) + (xc * sin(- angr))
      call symbol(x - xcp, y - ycp, hite, symb, - ang, 1)
      end if
      call setfon(svfont)
      return 
      end
