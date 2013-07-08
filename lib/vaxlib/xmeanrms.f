c----- XMEANRMS  J. C. LAHR    9/19/85
      dimension x(100), key(100)
# 3 "xmeanrms.for"
      write(unit=*, fmt=*) 'TEST XMEAN SUBOUTINE.'
      write(unit=*, fmt=*) 
     &'THE INITIAL VALUES OF X(I, I= 1, 100) ARE X(I) = I'
# 5 "xmeanrms.for"
      do 10 i = 1, 100
   10 x(i) = i
   20 write(unit=*, fmt=*) 
     &'GIVE A POSITIVE INTEGER (J) TO CHANGE VALUE OF X(J)'
# 8 "xmeanrms.for"
      write(unit=*, fmt=*) 
     &'GIVE A NEGATIVE INTEGER (-J) TO COMPUTE MEAN'
# 9 "xmeanrms.for"
      write(unit=*, fmt=*) '  RMS FOR X(1) TO X(J)'
      write(unit=*, fmt=*) 'GIVE J = 0 TO STOP '
      j = iaskk('VALUE OF J',1)
      if (j .lt. 0) then
      ntot = - j
      call xmeanrms(x, ntot, xmean, rms)
      write(unit=*, fmt=*) 'MEAN, RMS, AND NTOT ARE ', xmean, rms, ntot
      goto 20
      else if (j .eq. 0) then
      stop 
      else
      x(j) = raskk('NEW VALUE FOR X(J)',x(j))
      goto 20
      end if
      end
      subroutine xmeanrms(x, ntot, xmean, rms)
c FIND THE MEAN VALUE (XMEAN) AND STANDARD DEVIATION (RMS) OF X 
c SET RMS = 99. IF THERE IS ONLY ONE DATA VALUE
c      PRINT *, 'NTOT = ', NTOT
      dimension x(ntot)
# 29 "xmeanrms.for"
      if (ntot .eq. 0) then
      xmean = 0.
      rms = 0.
      return 
c     PRINT *, X
# 33 "xmeanrms.for"
      end if
# 35 "xmeanrms.for"
      xsum = 0.0
      xsqsum = 0.0
      do 20 i = 1, ntot
      xsum = xsum + x(i)
      xsqsum = xsqsum + (x(i) ** 2)
   20 continue
      xmean = xsum / ntot
      rms = 99.
      if (ntot .gt. 1) then
      arg = (xsqsum - (ntot * (xmean ** 2))) / (ntot - 1)
      if (arg .lt. 0.) then
      write(unit=*, fmt=*) 'SQRT OF NEG NUMBER IN SUBROUTINE XMEANRMS.'
      do 30 i = 1, ntot
      write(unit=*, fmt=*) 'I, X(I) ', i, x(i)
   30 continue
      write(unit=*, fmt=*) 'ARG = ', arg, ' -- RESET TO ZERO --'
      arg = 0.0
      end if
      rms = sqrt(arg / (ntot - 1))
      end if
      return 
      end
