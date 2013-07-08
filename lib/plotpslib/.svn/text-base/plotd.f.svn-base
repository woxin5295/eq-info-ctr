      subroutine setline (cyclen, icode, ierr)
C--THIS PLOTD.FOR FILE COMBINES PLOTD.FOR AND GETDIG.FOR ASSEMBLED
C--BY JOANNE VINTON IN 6/86.
c
c     Code in capital letters is taken from Paul Spudich's CRVPLT
c     subroutine.
C
C THE CURVE CAN BE PLOTTED AS A SOLID LINE, A DOTTED LINE (DOTS NOT
C CONNECTED BY LINES), A DASHED LINE, OR AN ARBITRARILY COMPLICATED 
C CHAINED LINE.  NOTE.... IN THE FOLLOWING DISCUSSION, THE WORD "LINK"
C REFERS TO A SINGLE SEQUENCE OF DASHES AND SPACES WHICH IS REPEATED
C TO FORM A CHAINED LINE.
C
C        CYCLEN  -  IF YOU ARE PLOTTING A DOTTED, DASHED, OR CHAINED
C                    LINE, THIS NUMBER IS AN ARC LENGTH IN INCHES ALONG
C                    THE LINE.  FOR A DOTTED LINE IT IS THE DISTANCE
C                    DESIRED BETWEEN DOTS.  FOR A DASHED LINE IT IS THE
C                    DISTANCE DESIRED BETWEEN THE START OF SUCCESSIVE 
C                    DASHES.  FOR A CHAINED LINE IT IS THE LENGTH OF 
C                    EACH "LINK" OF THE CHAIN.
C         ICODE  -  A FOUR-BYTE INTEGER CONTAINING A CODE SPECIFYING THE
C                    TYPE OF LINE DESIRED.
C                   =0  -  SOLID LINE
C                   =1  -  DOTTED LINE (DOTS NOT CONNECTED BY LINES)
C                   = ANY OTHER INTEGER ( IABS(ICODE) .LE. 2147483647)
C                    CODE FOR CHAINED LINE PATTERN.  GENERAL FORM:
C                        ICODE = +/-ABCD...  WHERE A, B, C, D ARE 
C                                         INTEGER DIGITS BETWEEN 1 AND 9
C                    IF ICODE POSITIVE, THEN
C                    "A" SPECIFIES THE RELATIVE LENGTH OF THE FIRST DASH
C                    "B"     "      "     "        "   "   "   "   SPACE
C                    "C" SPECIFIES THE RELATIVE LENGTH OF THE 2ND  DASH
C                    "D"     "      "     "        "   "   "   "   SPACE
C                                      .
C                                      .
C                                     ETC
C                                      .
C                                      .
C                    EXAMPLE: IF CYCLEN = .6 AND ICODE = 11 YOU GET A 
C                              LINE CONSISTING OF A DASH .3 INCHES LONG
C                              FOLLOWED BY A SPACE .3 INCHES LONG.
C                             IF CYCLEN = .6 AND ICODE = 22 YOU GET SAME
C                              AS ABOVE
C                             IF CYCLEN = .6 AND ICODE = 3111 YOU GET A
C                              DASH .3" LONG, A SPACE .1" LONG, A DASH 
C                              .1" LONG, AND A SPACE .1" LONG
C                    IF ICODE NEGATIVE - REVERSE SPACES AND DASHES FOR 
C                     A,B,C,D ABOVE.
C
      LOGICAL LINE, DOT
      INTEGER PU, PD
      DATA PU /3/, PD /2/
c
      common /dash/ oldx, oldy, jpenpos, jcode, remarc,
     1              idig(10), seglen(10), ncode, line, dot
c
      ierr = 0
c
      if (cyclen .le. 0.0) then
        ierr = 1
        cyclen = 1.0
      end if
c
      LINE   = .FALSE.
      DOT    = .FALSE.
C
      IF (ICODE .EQ. 1) THEN
         DOT = .TRUE.
         NCODE = 1
      END IF
      IF (ICODE .EQ. 0) LINE = .TRUE.
C
      IF (.NOT. DOT .AND. .NOT. LINE) THEN
C........FIGURE OUT CHAINED LINE SEGMENT LENGTH
         CALL GETDIG (abs(ICODE), NCODE, IDIG)
C........MAKE SURE CODE HAS EVEN # OF DIGITS
         IF (JMOD(NCODE,2) .NE. 0) THEN
            NCODE = NCODE + 1
            IDIG(NCODE) = 1
            IERR = 1
         END IF
         SUMDIG = 0.
         DO 100 I = 1,NCODE
C...........FIX ANY ZERO VALUES OF IDIG
            IF (IDIG(I) .EQ. 0) THEN
               IDIG(I) = 1
               IERR = 1
            END IF
  100       SUMDIG = SUMDIG + IDIG(I)
         DSEG = CYCLEN / SUMDIG
C........SEGLEN CONTAINS LENGTH (INCHES) OF LINE SEGMENTS IN CHAIN LINK
         DO 101 I = 1,NCODE
  101       SEGLEN(I) = IDIG(I) * DSEG
      END IF
C
      IF (DOT) SEGLEN(1) = CYCLEN
      IF (ICODE .GT. 0) jpenpos = PD
      IF (ICODE .LE. 0) jpenpos = PU
C
C........INITIALIZE CHAIN PATTERN. REMARC IS REMAINING ARC IN A SEGMENT OF
C        THE CHAIN LINK
         JCODE = 1
         REMARC = SEGLEN(1)
c
      return
      end
c
c
      subroutine plotd (x, y, jpen)
c
c     This algorithm came out of talks with Fred Klein; code in capital
c     letters came from Paul Spudich's CRVPLT subroutine.
c
      LOGICAL LINE, DOT
      INTEGER PU, PD
      DATA PU /3/, PD /2/
c
      common /dash/ oldx, oldy, jpenpos, jcode, remarc,
     1              idig(10), seglen(10), ncode, line, dot
c
c
      if (line  .or.  (jpen.ne.2 .and. jpen.ne.3)) then
c        If ipen isn't a draw, then just pass the call
c        through to the regular plotting software.
         call plot (x, y, jpen)
      else
C
C........CHAINED OR DOTTED LINE
C
         if (jpen .eq. 3) then
            oldx = x
            oldy = y
            IF (DOT) then
C               CALL SYMBOL (oldx, oldy, .02, 1, 0., -1)
		CALL PLOT (OLDX,OLDY,3)
C		CALL SYMBL (14,2)
	        call plot(oldx,oldy,2)
            else
               CALL PLOT (oldx, oldy, jpen)
            end if
            go to 300
         end if
C
  301    DXIN = X - oldx
         DYIN = Y - oldy
         STPLEN = SQRT ( DXIN**2 + DYIN**2 )
C
C
         IF (STPLEN .EQ. 0.) GO TO 300
C
         IF (STPLEN .LT. REMARC) THEN
               IF (.NOT. DOT) CALL PLOT (X, Y, jpenpos)
               oldx = x
               oldy = y
               REMARC = REMARC - STPLEN
               GO TO 300
         END IF
C
         IF (STPLEN .GE. REMARC) THEN
               DYARC = REMARC * DYIN / STPLEN
               DXARC = REMARC * DXIN / STPLEN
               XINNOW = oldx + DXARC
               YINNOW = oldy + DYARC
               IF (DOT) THEN
C		CALL SYMBOL (XINNOW, YINNOW, .02, 1, 0., -1)
		CALL PLOT (XINNOW,YINNOW,3)
C		CALL SYMBL (14,2)
		call plot(xinnow,yinnow,2)
	       ELSE
		CALL PLOT (XINNOW, YINNOW, jpenpos)
	       END IF
C
C..............WE HAVE REACHED THE END OF THIS PLOTTED SEGMENT IN ONE
C              OF THE LINKS OF THE CHAIN.  CHANGE PEN POSITION AND 
C              RESET REMARC, REMAINING LENGTH OF ARC LEFT TO BE PLOTTED.
               JCODE = JCODE + 1
               IF (JCODE .GT. NCODE) JCODE = 1
               REMARC = SEGLEN (JCODE)
               oldx = xinnow
               oldy = yinnow
               IF (jpenpos .EQ. PU) THEN
                  jpenpos = PD
               ELSE
                  jpenpos = PU
               END IF
C
C..............START ANOTHER LINE SEGMENT IN CHAIN LINK
               GO TO 301
         END IF
C
  300    CONTINUE
      endif            !Chained or dotted line
C
C
      RETURN 
      END
C-----------------------------------------------------------------------
      SUBROUTINE GETDIG (N, NDIG, IDIG)
C-----------------------------------------------------------------------
C  Written by Paul Spudich, OEVE, ESG.
C
C  GIVEN AN INTEGER N, GETDIG RETURNS THE INDIVIDUAL DIGITS OF
C N (BASE 10) IN ARRAY IDIG, IN ORDER OF DESCENDING SIGNIFICANT DIGITS,
C AND NDIG, WHICH IS THE NUMBER OF DIGITS IN N.
C 
C EXAMPLE ... INPUT:    N = -3498
C
C             OUTPUT:   NDIG = 4
C                       IDIG(1)=3, IDIG(2)=4, IDIG(3)=9, IDIG(4)=8
C---------
C
      DIMENSION IDIG(1)
C
      IF (N .EQ. 0) THEN
         NDIG = 1
         IDIG(1) = 0
         RETURN
      END IF
C
      NN = N
      NDIG = ALOG10 (FLOAT(NN))  +  1
      DO 100 I = 1,NDIG
         IDIG(I) = NN / 10**(NDIG-I)
  100    NN = NN - IDIG(I) * 10**(NDIG-I)
      RETURN
      END
