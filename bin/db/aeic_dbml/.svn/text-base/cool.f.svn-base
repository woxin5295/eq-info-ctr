c
c
      Subroutine cool(n, x, signi)
C.======================================================================
C.    PURPOSE
C     Cool                                                        RE<<
C     cool calculates either the forward or inverse finite fourier
C     transform of a complex series
C     nx
C     f(j) = sum x(k)*exp(i*signi*2*pi*(k-1)*(j-1)/nx)       j=1,nx
C     k=1
C     where nx must be an exact power of 2.
C.----------------------------------------------------------------------
C.    KEYWORDS
C.----------------------------------------------------------------------
C.    INPUT
C
C..    n   -    log(nx) to the base 2
C..    x   -    complex vector of dimension .ge. nx
C..    signi -  .eq. -1.0 for computation of forward transform
C               .eq. +1.0 for computation of inverse transform
C
C
C.    OUTPUT
C..    x   -   the transform is stored in the position of the
C              original series. the real part contains the cosine
C              series and is symmetric about the point 2**(n-1)+1.
C              the imaginary part contains the sine series and is
C              asymmetric about the point 2**(n-1)+1.  point 1 is
C              for zero frequency and point 2**(n-1)+1 is for the
C              nyquist frequency.
C.    notes  --
C          the scale factor 1/nx = 1/2**n is not applied.
C
C..   IRC     - Return code
C             = 0  No error
C             = ?  Fatal error
C.----------------------------------------------------------------------
C.    PROGRAMMER    R.A. Hansen
C.    CREATION_DATE 
C.    MADE_AT       NTNF/NORSAR
C                   Pb. 51
C                   N-2007 Kjeller
C
C.    MODIFICATION
C.    CORRECTION
C.======================================================================
c
c
      complex x(1), carg, cexp, cw, ctemp, cpi
      cpi = cmplx(0.0,3.14159265358979 * signi)
      lx = 2 ** n
      j = 1.
      do 30 i = 1, lx
      if (i .gt. j) goto 10
      ctemp = x(j)
      x(j) = x(i)
      x(i) = ctemp
   10 m = lx / 2
   20 if (j .le. m) goto 30
      j = j - m
      m = m / 2
      if (m .ge. 1) goto 20
   30 j = j + m
      l = 1
   40 istep = l + l
      do 50 m = 1, l
      carg = (cpi * (m - 1)) / l
      cw = cexp(carg)
      do 50 i = m, lx, istep
      ctemp = cw * x(i + l)
      x(i + l) = x(i) - ctemp
   50 x(i) = x(i) + ctemp
      l = istep
      if (l .lt. lx) goto 40
      return
      end
