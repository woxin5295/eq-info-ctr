c     character fmit*6
c 2 "formall.for"
c     write(unit=*, fmt=*) 'TEST FORMALL'
c  30 n = iaskk('GIVE NUMBER OF COLUMNS FOR NUMBER',n)
c     write(unit=*, fmt=*) 'FORMAT F3.1 USES 1 DECIMAL DIGIT.'
c     nsig = iaskk('GIVE NUMBER OF DECIMAL DIGITS IN READ STATEMENT',
c    &nsig)
c     x = raskk('GIVE NUMBER TO BE PRINTED OUT',x)
c     call formall(x, ix, n, nsig, fmit, xout)
c     if (fmit .eq. ' ') then
c     write(unit=*, fmt=*) 'WRITE ', ix, ' WITH FORMAT I', n
c     else
c     write(unit=*, fmt=*) 'WRITE ', xout, ' WITH FORMAT ', fmit
c     end if
c     goto 30
c FORMALL.FOR
c 15 "formall.for"
c     end
c---- CONVERTS REAL NUMBER X INTO THE INTEGER IX TO BE
c---- WRITTEN WITH FORMAT(IN) AND READ WITH FORMAT(FN.NSIG)
c---- CORRECTIONS BY WILLY ASPINALL, PRINCIPIA TESTING  JULY 1983
c
c---- IF THE NUMBER IS TOO LARGE TO WRITE AS AN INTEGER AND
c---- THE NSIG IS GREATER THAN 1, THEN FIND XOUT AND FMIT
c---- SO THAT THE NUMBER MAY BE WRITTEN A XOUT WITH FMIT.
c
      subroutine formall(x, ix, n, nsig, fmit, xout)
      character fmit*6
# 27 "formall.for"
      ix = (x * (10. ** nsig)) + sign(0.50001,x)
      imax = 10 ** n
      fmit = '      '
c       POSITIVE NUMBER
# 30 "formall.for"
      if (x .gt. 0.) then
# 32 "formall.for"
      if (ix .ge. imax) then
      if (nsig .lt. 2) then
      ix = imax - 1
      return 
c           IN THIS CASE PASS BACK A FORMAT TO BE USED IN READING
# 36 "formall.for"
      else
# 38 "formall.for"
      call formit(x, xout, fmit, n, 1)
      return 
      end if
      end if
      if (ix .lt. (imax / 100)) then
c           IN THIS CASE PASS BACK A FORMAT TO BE USED IN READING
# 43 "formall.for"
      if ((n - nsig) .gt. 1) then
# 45 "formall.for"
      call formit(x, xout, fmit, n, 1)
      return 
      end if
      end if
c       NEGATIVE NUMBER
# 49 "formall.for"
      else
# 51 "formall.for"
      if (ix .le. (- (imax / 10))) then
      if (nsig .lt. 2) then
      ix = (- (imax / 10)) + 1
      return 
c           IN THIS CASE PASS BACK A FORMAT TO BE USED IN READING
# 55 "formall.for"
      else
# 57 "formall.for"
      call formit(x, xout, fmit, n, 1)
      return 
      end if
      end if
      if (ix .gt. (- (imax / 1000))) then
c           IN THIS CASE PASS BACK A FORMAT TO BE USED IN READING
# 62 "formall.for"
      if ((n - nsig) .gt. 2) then
# 64 "formall.for"
      call formit(x, xout, fmit, n, 1)
      return 
      end if
      end if
      end if
c END FORMALL
# 69 "formall.for"
      end
