c FORMF.FOR
c---- CONVERTS REAL NUMBER X INTO THE INTEGER IX TO BE
c---- WRITTEN WITH FORMAT(IN) AND READ WITH FORMAT(FN.NSIG)
c---- CORRECTIONS BY WILLY ASPINALL, PRINCIPIA TESTING  JULY 1983
      subroutine formf(x, ix, n, nsig)
      ix = (x * (10. ** nsig)) + sign(0.50001,x)
      imax = 10 ** n
      if (ix .ge. imax) ix = imax - 1
      if (ix .le. (- (imax / 10))) ix = (- (imax / 10)) + 1
      return 
c END FORMF
      end
