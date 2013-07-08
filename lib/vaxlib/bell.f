c--sends bell to terminal
      subroutine bell()
cvax
c     character a*1
c     equivalence (i, a)
c     i = 7
c     write(unit=6, fmt='(1x, a, $)') a
cvax
csun
      write(0, '(a)') char(7)
csun
      return 
      end
