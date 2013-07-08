c     NEW IMROVED VERSION OF THIS IS CALLED NEWFORMS
      subroutine forms(i, ziz, xo, yo)
c     SEE PUB1:[LAHR.VGP3.SOURCE]FORMS FOR A DASHED VERSION OF THIS
c        REGULAR SYMBOLS OF SIZE ZIZ CENTERED ON (XO,YO)
c        IF SIZE (ZIZ) IS ZERO, THEN JUST DROP PEN ONCE.
c        LIST OF SYMBOLS   I=0 PLUS, 1 CROSS, 2 TRIANGLE(UP),
c             3 TRIANGLE(RIGHT), 4 TRIANGLE(DOWN), 5 TRIANGLE(LEFT),
c            6 DIAMOND, 7 SQUARE, 8 CIRCLE, 9 STAR.
c        ADD 100 TO I FOR SOLID SYMBOL, 50 FOR HALF-SOLID SYMBOL, ETC
c            FILL SYMBOL FROM OUTSIDE ADDING ONE SMALLER SYMBOL FOR
c            EVERY 10 ADDED TO I
c        "ARROW" SYMBOLS CONSIST OF A LINE SEGMENT ZIZ INCHES
c            LONG THAT STRIKES (I-9000) DEGREES CLOCKWISE FROM NORTH.
c            IF I IS LESS THAN 9000, THEN THE SYMBLO MUST BE OF THE
c            STANDARD TYPE.
c
c        SUBROUTINE WRITTEN BY J. C. LAHR
c                   MODIFIED BY: R. A. PAGE, NOV 85; J. C. LAHR, FEB 86
      common /all/ pi, rpd, dpr
# 20 "forms.for"
      ij = i
c      PEN DROP
# 21 "forms.for"
      if (ziz .eq. 0.) then
# 23 "forms.for"
      call plot(xo, yo, 3)
      call plot(xo, yo, 2)
      call plot(xo, yo, 3)
      return 
      end if
      if (i .ge. 9000) goto 200
      zizz = ziz
      ii = ij + 1
      k = ij / 10
      ii = ii - (k * 10)
      z = zizz * 0.5
      zx = zizz * 0.5
      zy = zizz * 0.5
      z1x = .866 * z
      z1y = .866 * z
c        I=0  PLUS
# 38 "forms.for"
    1 goto (5, 10, 20, 30, 40, 50, 60, 70, 80, 90), ii
# 40 "forms.for"
    5 y = yo + zy
      call plot(xo, y, 3)
      y = yo - zy
      call plot(xo, y, 2)
      x = xo - zx
      call plot(x, yo, 3)
      x = xo + zx
      call plot(x, yo, 2)
      call plot(x, yo, 3)
c        I=1  CROSS
# 49 "forms.for"
      return 
# 51 "forms.for"
   10 zx = .707 * z
      zy = .707 * z
      x = xo - zx
      y = yo + zy
      call plot(x, y, 3)
      x = xo + zx
      y = yo - zy
      call plot(x, y, 2)
      y = yo + zy
      call plot(x, y, 3)
      x = xo - zx
      y = yo - zy
      call plot(x, y, 2)
      call plot(x, y, 3)
c        I=2  TRIANGLE, POINTING UP
# 65 "forms.for"
      return 
# 67 "forms.for"
   20 y = yo + zy
      call plot(xo, y, 3)
      x = xo - z1x
      y = yo - (zy / 2)
      call plot(x, y, 2)
      x = xo + z1x
      call plot(x, y, 2)
      y = yo + zy
      call plot(xo, y, 2)
      call plot(xo, y, 3)
c        I=3  TRIANGLE, POINTING TO RIGHT
# 77 "forms.for"
      goto 100
# 79 "forms.for"
   30 x = xo + zx
      call plot(x, yo, 3)
      y = yo + z1y
      x = xo - (zx / 2)
      call plot(x, y, 2)
      y = yo - z1y
      call plot(x, y, 2)
      x = xo + zx
      call plot(x, yo, 2)
      call plot(x, yo, 3)
c        I=4  TRIANGLE, POINTING DOWN
# 89 "forms.for"
      goto 100
# 91 "forms.for"
   40 y = yo - zy
      call plot(xo, y, 3)
      x = xo + z1x
      y = yo + (zy / 2)
      call plot(x, y, 2)
      x = xo - z1x
      call plot(x, y, 2)
      y = yo - zy
      call plot(xo, y, 2)
      call plot(xo, y, 3)
c        I=5  TRIANGLE, POINTING TO LEFT
# 101 "forms.for"
      goto 100
# 103 "forms.for"
   50 x = xo - zx
      call plot(x, yo, 3)
      x = xo + (zx / 2)
      y = yo - z1y
      call plot(x, y, 2)
      y = yo + z1y
      call plot(x, y, 2)
      x = xo - zx
      call plot(x, yo, 2)
      call plot(x, yo, 3)
c        I=6  DIAMOND
# 113 "forms.for"
      goto 100
# 115 "forms.for"
   60 x = xo - zx
      call plot(x, yo, 3)
      y = yo - zy
      call plot(xo, y, 2)
      x = xo + zx
      call plot(x, yo, 2)
      y = yo + zy
      call plot(xo, y, 2)
      x = xo - zx
      call plot(x, yo, 2)
      call plot(x, yo, 3)
c        I=7  SQUARE
# 126 "forms.for"
      goto 100
# 128 "forms.for"
   70 zx = .707 * z
      zy = .707 * z
      x = xo - zx
      y = yo - zy
      call plot(x, y, 3)
      x = xo + zx
      call plot(x, y, 2)
      y = yo + zy
      call plot(x, y, 2)
      x = xo - zx
      call plot(x, y, 2)
      y = yo - zy
      call plot(x, y, 2)
      call plot(x, y, 3)
c        I=8  CIRCLE
# 142 "forms.for"
      goto 100
# 144 "forms.for"
   80 x = xo + zx
      call plot(x, yo, 3)
      n = 20 * sqrt(z * 20.)
      if (n .lt. 10) n = 10
      an = n
      do 81 j = 1, n
      x = xo + (zx * cos((6.2832 * j) / an))
      y = yo + (zy * sin((6.2832 * j) / an))
      call plot(x, y, 2)
   81 continue
      call plot(x, y, 3)
c        I=9 STAR
# 155 "forms.for"
      goto 100
# 157 "forms.for"
   90 y = yo + zy
      call plot(xo, y, 3)
      do 91 j = 1, 5
      x = xo + (zx * sin((6.2832 * 0.4) * j))
      y = yo + (zy * cos((6.2832 * 0.4) * j))
      call plot(x, y, 2)
   91 continue
      call plot(x, y, 3)
  100 if (k .le. 0) return 
      factor = k / 11.
      z = z * factor
      zx = zx * factor
      zy = zy * factor
      z1x = z1x * factor
      z1y = z1y * factor
      k = k - 1
c        I .GE. 9000, SO SYMBOL MUST BE "ARROW"
# 173 "forms.for"
      goto 1
# 175 "forms.for"
  200 call plot(xo, yo, 3)
      ang = (i - 9000) * rpd
      x = xo + ((.5 * ziz) * sin(ang))
      y = yo + ((.5 * ziz) * cos(ang))
      call plot(x, y, 3)
      x = xo - ((.5 * ziz) * sin(ang))
      y = yo - ((.5 * ziz) * cos(ang))
      call plot(x, y, 2)
      return 
      end
