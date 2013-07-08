      SUBROUTINE PLOTPL (CX, CY, DPIDG, PI, RAD, RMAX, STRKDG, PROT)
C
C PLOTS FAULT PLANE ON LOWER HEMISPHERE STEREO NET
C
      REAL              CX                              ! X POSITION OF CIRCLE CENTER
      REAL              CY                              ! Y POSITION OF CIRCLE CENTER
      REAL              DPIDG                           ! DIP ANGLE IN DEGREES
      REAL              PI                              ! PI
      REAL              RAD                             ! PI/180
      REAL              RMAX                            ! RADIUS OF CIRCLE
      REAL              STRKDG                          ! STRIKE ANGLE IN DEGREES

      REAL              ANG                             ! ANGLE IN RADIANS
      REAL              AINP(91)                        ! ANGLE OF INCIDENCE IN RADIANS
      REAL              ARG                             ! DUMMY ARGUMENT
      REAL              AZ                              ! AZIMUTH
      REAL              CON                             ! RADIUS COEFFICIENT
      REAL              DIPRD                           ! DIP ANGLE IN RADIANS
      INTEGER           I                               ! LOOP INDEX
      INTEGER           MI                              ! SCRATCH INDEX
      REAL              RADIUS                          ! RADIUS
      REAL              SAZ(91)                         ! AZIMUTH IN RADIANS
      REAL              STRKRD                          ! STRIKE IN RADIANS
      REAL              TAZ                             ! SCRATCH VARIABLE
      REAL              TPD                             ! SCRATCH VARIABLE
      REAL              X                               ! X PLOT POSITION
      REAL              Y                               ! Y PLOT POSITION
      REAL              PROT                            ! PAPER ROTATION: 0.00 --> +Y UP
C                                                       ! PAPER ROTATION:-PI/2 --> -X UP

C
      STRKRD = STRKDG*RAD
      DIPRD = DPIDG*RAD
      TPD = TAN(PI*.5 - DIPRD)**2
C
C CASE OF VERTICAL PLANE
C
      IF (DPIDG .EQ. 90.0) THEN
        X = RMAX*SIN(STRKRD + PROT) + CX
        Y = RMAX*COS(STRKRD + PROT) + CY
        CALL PLOT (X, Y, 3)
        X = RMAX*SIN(STRKRD + PI + PROT) + CX
        Y = RMAX*COS(STRKRD + PI + PROT) + CY
        CALL PLOT (X, Y, 2)
        RETURN
      END IF
C
C COMPUTE ANGLE OF INCIDENCE, AZIMUTH
C
      DO 10 I = 1, 90
        ANG = FLOAT(I - 1)*RAD
        ARG = SQRT((COS(DIPRD)**2)*(SIN(ANG)**2))/COS(ANG)
        SAZ(I) = ATAN(ARG)
        TAZ = TAN(SAZ(I))**2
        ARG = SQRT(TPD + TPD*TAZ + TAZ)
        AINP(I) = ACOS(TAN(SAZ(I))/ARG)
  10  CONTINUE
      SAZ(91) = 90.*RAD
      AINP(91) = PI*.5 - DIPRD
C
C PLOT PLANE
C
      CON = RMAX*SQRT(2.)
      DO 20 I = 1, 180
        IF (I .LE. 91) THEN
          MI = I
          AZ = SAZ(I) + STRKRD
        ELSE
          MI = 181 - I
          AZ = PI - SAZ(MI) + STRKRD
        END IF
        RADIUS = CON*SIN(AINP(MI)*0.5)
        X = RADIUS*SIN(AZ + PROT) + CX
        Y = RADIUS*COS(AZ + PROT) + CY
        IF (I .EQ. 1) THEN
          CALL PLOT (X, Y, 3)
        ELSE
          CALL PLOT (X, Y, 2)
        END IF
20    CONTINUE
C
      RETURN
      END
