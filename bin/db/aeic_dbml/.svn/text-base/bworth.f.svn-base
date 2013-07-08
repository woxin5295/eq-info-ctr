cc
cc
      subroutine bworth(f, fo, np, hs)
C.======================================================================
C.    PURPOSE
C     Bworth                                                      RE<<
C     bworth calculates the response of a np pole butterworth filter
C     up to as many poles as the arrays s and t are dimensioned(20)
C.----------------------------------------------------------------------
C.    KEYWORDS
C.----------------------------------------------------------------------
C.    INPUT
C
C..   f       - frequency(hz)
C..   fo      - the corner frequency of the filter
C..   np      - the number of poles, negative for high pass
C
C.    OUTPUT
C..   hs      - complex response of the filter
C.----------------------------------------------------------------------
C.    PROGRAMMER    Roger A. Hansen
C.    CREATION_DATE
C.    MADE_AT       NTNF/NORSAR
C     Pb. 51
C     N-2007 Kjeller
C
C.    MODIFICATION
C.    CORRECTION
C.======================================================================
c
c
c    the formula used -- h(s)=1/(s-s1)(s-s2)...(s-sk)
c                        i*pi*(1/2+((2*k-1)/(2*np)))
c    where         sk=exp
c                                   k=1,2, ... np
c                  s = i(f/fo)
c
c    ref theory and application of digital signal processing
c    rabiner and gold page 227 prentice-hall 1975
c
c    Adapted to Vax/VMS by R. A. Hansen
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      complex s(20), t(20), as, bk, hs
      hs = cmplx(1.0,0.0)
      n = iabs(np)
      if (np .eq. 0) goto 6
      if ((f .eq. 0.0) .and. (np .lt. 0)) hs = cmplx(0.0,0.0)
      if ((f .eq. 0.0) .and. (np .lt. 0)) goto 6
      do 1 k = 1, n
      an = float(k)
      ak = 3.141592654 * (0.5 + (((2. * an) - 1.) / (2. * float(n))))
 
      bk = cmplx(0.0,ak)
    1 s(k) = cexp(bk)
      ss = f / fo
      as = cmplx(0.0,ss)
      if (np .lt. 0) as = 1. / as
      t(1) = as - s(1)
      if (n .eq. 1) goto 5
      do 2 i = 2, n
    2 t(i) = (as - s(i)) * t(i - 1)
    5 continue
      hs = 1. / t(n)
    6 return
      end
