c        REGULAR SYMBOLS CENTERED ON (XO,YO)
c        ISYM   SYMBOL CODE NUMBER
c        ZIZ    SYMBOL DIAMETER (IF ZERO, JUST DROP THE PEN ONCE)
c        XO     CENTER OF SYMBOL
c        YO     CENTER OF SYMBOL
c        DASH   LENGTH OF DASH USED IN DRAWING SYMBOL
c        SPACE  LENGTH OF SPACE
c
c
c     GENERATE THE X(I), Y(I) POINTS NEEDED TO DRAW SYMBOL FOR
c     I = 1 TO NPTS.  X = Y = 9999. MEANS LIFT PEN DURING SYMBOL.
c
      subroutine newform(isym, ziz, xo, yo, dash, space)
c
      dimension x(125), y(125)
c
# 16 "newform.for"
      call makform(isym, ziz, xo, yo, npts, x, y)
# 18 "newform.for"
      iup = 3
      do 30 n = 1, npts
      if ((x(n) .eq. 9999.) .and. (y(n) .eq. 9999.)) then
      iup = 3
      goto 30
      end if
      call dashit(x(n), y(n), iup, dash, space, 0.)
      iup = 2
   30 continue
      return 
      end
c           CALL MAKFORM(ISYM, ZIZ,      XO,      YO, NPTS, X, Y)
c     J. C. LAHR    4/20/86
      subroutine makform(isym, ht, xcenter, ycenter, npts, x, y)
      real x(*), y(*), xx(5), yy(5)
c
c        REGULAR SYMBOLS CENTERED ON (xcenter,50YCENTER)
c        ISYM      SYMBOL CODE NUMBER
c        HT       SYMBOL DIAMETER (IF ZERO, JUST DROP THE PEN ONCE)
c
c        GENERATE THE X(I), Y(I) POINTS NEEDED TO DRAW SYMBOL FOR
c        I = 1 TO NPTS.  X = Y = 9999. MEANS LIFT PEN DURING SYMBOL.
c
c        SYMBOL CODE IS FORMED FROM:
c           %FILL + NPOINTS + ORIENTATION = CODE
c           IFIL      ITYP      IDIR
c                      0         0            0   JUST DROP PEN
c                      1         0           10   PLUS
c                      1         1           11   CROSS
c                      2         X           2X   LINE SEGMENT
c                       (POINTING (X/10)*360 DEGREES CLOCKWISE FROM UP)
c                      3         0           30   TRIANGLE (UP)
c                      3         1           31   TRINAGLE (RIGHT)
c                      3         2           32   TRIANGLE (DOWN)
c                      3         3           33   TRINAGLE (LEFT)
c                      4         0           40   DIAMOND
c                      4         1           41   SQUARE
c                      5         0           50   PENTAGON (POINT UP)
c                      5         1           51   PENTAGON (POINT RIGHT)
c                      5         2           52   PENTAGON (POINT DOWN)
c                      5         3           53   PENTAGON (POINT LEFT)
c                      6         0           60   HEXAGON (ON POINT)
c                      6         1           61   HEXAGON (RESTING FLAT)
c                     >6         0           70   OCTAGON
c
c     IFIL CAN RANGE FROM 0 TO 10:
c        0   NO FILL
c        1   ONE ADDITIONAL SYMBOL, 90% OF FULL SIZE
c        2   TWO ADDITIONAL SYMBOLS, 90% AND 80% OF FULL SIZE
c        3   ETC.
c     FOR EXAMPLE ISYM = 552 WOULD BE A 50 PERCENT FILLED PENTAGON 
c     POINTING DOWN.
c
      parameter (pi = 3.1415927, deg2rad = 0.17453293e-1)
# 72 "newform.for"
      factor = 1.
      npts = 0
c      PEN DROP
# 74 "newform.for"
      if (ht .eq. 0.) then
# 76 "newform.for"
    2 npts = 2
      x(1) = xcenter
      y(1) = ycenter
      x(2) = xcenter
      y(2) = ycenter
      return 
      end if
      ifil = iabs(isym) / 100
      itmp = iabs(isym) - (ifil * 100)
      ityp = itmp / 10
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cC
# 86 "newform.for"
      idir = itmp - (ityp * 10)
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cC
# 88 "newform.for"
      if (ityp .eq. 0) goto 2
c       SET SYMBOL TO OCTAGON
# 90 "newform.for"
      if (ityp .gt. 6) then
# 92 "newform.for"
      ityp = 8
      idir = 0
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cC
# 94 "newform.for"
      end if
# 96 "newform.for"
      if (ityp .eq. 1) then
c         ISYM = 10  PLUS
# 97 "newform.for"
      if (idir .eq. 0) then
# 99 "newform.for"
      angzero = 0
c         ISYM = 11  CROSS
# 100 "newform.for"
      else
# 102 "newform.for"
      angzero = pi / 4.
      end if
      call gensymb(xcenter, ycenter, ht, angzero, 4, npts, xx, yy)
# 106 "newform.for"
      x(1) = xx(1)
      y(1) = yy(1)
      x(2) = xx(3)
      y(2) = yy(3)
      x(3) = 9999.
      y(3) = 9999.
      x(4) = xx(2)
      y(4) = yy(2)
      x(5) = xx(4)
      y(5) = yy(4)
      factor = 0.
   24 if (ifil .ne. 0) then
      ifil = ifil - 1
      factor = factor + (.025 * ht)
      sinf = factor * sin(angzero)
      cosf = factor * cos(angzero)
      sinin = (factor * 1.414) * sin((45 * deg2rad) + angzero)
      cosin = (factor * 1.414) * cos((45 * deg2rad) + angzero)
      x(npts + 1) = x(5) + sinf
      y(npts + 1) = y(5) + cosf
      x(npts + 2) = xcenter - cosin
      y(npts + 2) = ycenter + sinin
      x(npts + 3) = x(1) - cosf
      y(npts + 3) = y(1) + sinf
      x(npts + 4) = x(1) + cosf
      y(npts + 4) = y(1) - sinf
      x(npts + 5) = xcenter + sinin
      y(npts + 5) = ycenter + cosin
      x(npts + 6) = x(4) + sinf
      y(npts + 6) = y(4) + cosf
      x(npts + 7) = x(4) - sinf
      y(npts + 7) = y(4) - cosf
      x(npts + 8) = xcenter + cosin
      y(npts + 8) = ycenter - sinin
      x(npts + 9) = x(2) + cosf
      y(npts + 9) = y(2) - sinf
      x(npts + 10) = x(2) - cosf
      y(npts + 10) = y(2) + sinf
      x(npts + 11) = xcenter - sinin
      y(npts + 11) = ycenter - cosin
      x(npts + 12) = x(5) - sinf
      y(npts + 12) = y(5) - cosf
      x(npts + 13) = x(5) + sinf
      y(npts + 13) = y(5) + cosf
      npts = npts + 13
      goto 24
      end if
      return 
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cC
# 154 "newform.for"
      end if
c       ITYP .EQ. 2, SO SYMBOL MUST BE "LINE SEGMENT"
# 156 "newform.for"
      if (ityp .eq. 2) then
# 158 "newform.for"
      ang = ((idir / 10.) * pi) * 2.
      call gensymb(xcenter, ycenter, ht / 5, ang, 3, npts, x, y)
      x(npts + 1) = 9999.
      y(npts + 1) = 9999.
      x(npts + 2) = xcenter - ((.5 * ht) * sin(ang))
      y(npts + 2) = ycenter - ((.5 * ht) * cos(ang))
      x(npts + 3) = xcenter + ((.5 * ht) * sin(ang))
      y(npts + 3) = ycenter + ((.5 * ht) * cos(ang))
      npts = npts + 3
      return 
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cC
# 168 "newform.for"
      end if
c       ISYM = 59  STAR
# 170 "newform.for"
      if ((ityp .eq. 5) .and. (idir .eq. 9)) then
# 172 "newform.for"
      zx = ht * .5
      zy = ht * .5
      factor = 1.
   25 x(npts + 1) = xcenter
      y(npts + 1) = ycenter + zy
      do 28 j = 1, 5
      x((npts + j) + 1) = xcenter + (zx * sin(((2. * pi) * 0.4) * j))
      y((npts + j) + 1) = ycenter + (zy * cos(((2. * pi) * 0.4) * j))
   28 continue
      npts = npts + 6
      if (ifil .eq. 0) return 
      npts = npts + 1
      x(npts) = 9999.
      y(npts) = 9999.
      ifil = ifil - 1
      factor = factor - .025
      zx = zx * factor
      zy = zy * factor
      goto 25
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cC
c     ALL OTHER CLOSED SYMBOLS
# 191 "newform.for"
      end if
# 194 "newform.for"
      angzero = (idir * 90) * deg2rad
      if (ityp .eq. 4) angzero = angzero / 2.
      sht = ht
      factor = 1.
   30 call gensymb(xcenter, ycenter, sht, angzero, ityp, npts, x, y)
      if (ifil .eq. 0) return 
      ifil = ifil - 1
      factor = factor - .1
      sht = ht * factor
      npts = npts + 1
      x(npts) = 9999.
      y(npts) = 9999.
      goto 30
      end
c     Generate line draws for a figure with ncorn = npts-1 corners...
c     Angzero is the initial angle for symbol.
      subroutine gensymb(xcenter, ycenter, ht, angzero, ityp, npts, x, y
     &)
      real x(*), y(*)
      parameter (pi = 3.1415927, deg2rad = 0.17453293e-1)
# 215 "newform.for"
      anginc = (deg2rad * 360.) / real(ityp)
# 217 "newform.for"
      do 10 k = 1, ityp + 1
      npts = npts + 1
      ang = ((k - 1) * anginc) + angzero
      x(npts) = xcenter + ((0.5 * ht) * sin(ang))
      y(npts) = ycenter + ((0.5 * ht) * cos(ang))
   10 continue
# 224 "newform.for"
      return 
      end
c----- CONNECTS A DASHED LINE FROM PREVIOUS POINT TO X2,Y2
c
c      J            PLOT CONTROL   (2 = DOWN;  3 = UP)
c      DASH         LENGTH OF DASHES
c      SPACE        LENGTH OF SPACES
c      DISTMIN      SQUARE OF MIN SPACING BETWEEN POINTS TO PLOT
c
c      SLOPUP       LEFT OVER SPACE FROM PREVIOUS CALL
c      SLOPDN       LEFT OVER DASH
c
      subroutine dashit(x2, y2, j, dash, space, distmin)
# 237 "newform.for"
      if ((dash .eq. 0.) .or. (space .eq. 0)) then
      if (distmin .ne. 0.) then
      if (j .eq. 3) then
      call pltt(x2, y2, j)
      xold = x2
      yold = y2
      return 
      else
      if ((((xold - x2) ** 2.) + ((yold - y2) ** 2.)) .lt. distmin) 
     &return 
# 246 "newform.for"
      call pltt(x2, y2, j)
      xold = x2
      yold = y2
      return 
      end if
      else
      call pltt(x2, y2, j)
      end if
      return 
      end if
      if (j .eq. 3) then
      call pltt(x2, y2, j)
      slopup = 0.
      slopdn = 0.
      xold = x2
      yold = y2
      return 
      end if
      idash = 1
      delx = x2 - xold
      dely = y2 - yold
      aleft = sqrt((delx ** 2) + (dely ** 2))
      if (aleft .eq. 0) return 
      dshx = (dash * delx) / aleft
      dshy = (dash * dely) / aleft
      spax = (space * delx) / aleft
      spay = (space * dely) / aleft
      if ((slopup .eq. 0.) .and. (slopdn .eq. 0.)) then
c
c         DRAW A DASH
# 274 "newform.for"
   10 if (idash .eq. 1) then
# 277 "newform.for"
      if (aleft .ge. dash) then
      xold = xold + dshx
      yold = yold + dshy
      call pltt(xold, yold, 2)
      idash = 0
      aleft = aleft - dash
      goto 10
      else
      call pltt(x2, y2, 2)
      slopdn = dash - aleft
      xold = x2
      yold = y2
      return 
      end if
c
c         DRAW A SPACE
# 291 "newform.for"
      else
# 294 "newform.for"
      if (aleft .ge. space) then
      xold = xold + spax
      yold = yold + spay
      call pltt(xold, yold, 3)
      idash = 1
      aleft = aleft - space
      goto 10
      else
      call pltt(x2, y2, 3)
      slopup = space - aleft
      xold = x2
      yold = y2
      return 
      end if
      end if
      end if
c
c         DRAW A SHORT DASH OF LENGTH SLOPDN
# 310 "newform.for"
      if (slopdn .ne. 0.) then
# 313 "newform.for"
      if (aleft .ge. slopdn) then
      xold = xold + ((dshx * slopdn) / dash)
      yold = yold + ((dshy * slopdn) / dash)
      call pltt(xold, yold, 2)
      idash = 0
      aleft = aleft - slopdn
      slopdn = 0.
      goto 10
      else
      call pltt(x2, y2, 2)
      slopdn = slopdn - aleft
      xold = x2
      yold = y2
      return 
      end if
c
c         DRAW A SHORT SPACE OF LENGTH SLOPUP
# 328 "newform.for"
      else
# 331 "newform.for"
      if (aleft .ge. slopup) then
      xold = xold + ((spax * slopup) / space)
      yold = yold + ((spay * slopup) / space)
      call pltt(xold, yold, 3)
      idash = 1
      aleft = aleft - slopup
      slopup = 0.
      goto 10
      else
      call pltt(x2, y2, 3)
      slopup = slopup - aleft
      xold = x2
      yold = y2
      return 
      end if
      end if
      end
