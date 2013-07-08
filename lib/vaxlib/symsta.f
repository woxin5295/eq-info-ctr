      subroutine symsta(isym, xx, yy, ssiz, title, tsiz, angle, nchar)
      character title*(*)
# 3 "symsta.for"
      jsym = iabs(isym)
      x0 = xx
c-----  PLOT SYMBOL
# 5 "symsta.for"
      y0 = yy
c
c-----  PLOT TITLE; IF ISYM < 0, THEN DO NOT PLOT TITLE
# 7 "symsta.for"
      call forms(jsym, ssiz, x0, y0)
c-----  SUBROUTINE SYMBOL ALIGNS LOWER LEFTHAND CORNER OF STRING WITH X,
cY;
c-----  THEREFORE, ADJUST X,Y POINT OF TITLE TO ACCOUNT FOR LETTER SIZE
c-----  AND SIZE OF PLOT SYMBOL
# 10 "symsta.for"
      if (isym .lt. 0) return 
# 14 "symsta.for"
      delx = (0.5 * ssiz) + 0.05
c-----  CORRECT FOR ANGLE
# 15 "symsta.for"
      dely = 0.5 * tsiz
# 17 "symsta.for"
      xp = (x0 + (delx * cos(angle))) + (dely * sin(angle))
c-----  PLOT
# 18 "symsta.for"
      yp = (y0 + (delx * sin(angle))) - (dely * cos(angle))
# 20 "symsta.for"
      call symbol(xp, yp, tsiz, title, angle, nchar)
      return 
      end
