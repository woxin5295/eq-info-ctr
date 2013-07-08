      SUBROUTINE wtplot (x2, y2, ipen, iwt)

      IMPLICIT UNDEFINED(a-z)

      REAL thick, pi, half_pi
      PARAMETER (thick = 0.005)
      PARAMETER (pi = 3.1415926)
      PARAMETER (half_pi = pi / 2.0)

      INTEGER i, iwt, ipen
      REAL x1, y1, x2, y2, x1s, y1s, x2s, y2s, sumsqr
      REAL angle, sin_angle, cos_angle, half, dx, dy, t, hyp

      IF (ipen.NE.3) THEN
C         Proceed to plot the vector.
        IF (x1.NE.x2) THEN
          angle = ATAN2((y1-y2),(x1-x2))
        ELSE
          angle = half_pi
        END IF
        IF (ABS(angle).GT.half_pi) THEN
          sin_angle = -SIN(angle)
          cos_angle =  COS(angle)
        ELSE
          sin_angle =  SIN(angle)
          cos_angle = -COS(angle)
        END IF
        i = MOD((iwt-1),2)
        half = i * thick / 2.0

        IF (i.EQ.0) THEN
          CALL PLOT (x1,y1,3)
          CALL PLOT (x2,y2,2)
        END IF

        DO i=1, iwt/2
          t = i * thick - half
          dx = t * sin_angle
          dy = t * cos_angle
          x1s = x1 + dx
          y1s = y1 + dy
          x2s = x2 + dx
          y2s = y2 + dy
          CALL PLOT (x1s,y1s,3)
          CALL PLOT (x2s,y2s,2)
          x1s = x1 - dx
          y1s = y1 - dy
          x2s = x2 - dx
          y2s = y2 - dy
          CALL PLOT (x1s,y1s,3)
          CALL PLOT (x2s,y2s,2)
        END DO

      END IF

      x1 = x2
C     Remember current point for next time.
      y1 = y2

      RETURN
      END
