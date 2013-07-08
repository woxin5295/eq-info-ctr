      Subroutine symbl(is,k)
c--symbol plotting subroutine used by QPLOT AND OTHER PROGRAMS
c  is symbol type code as follows:
c   0 no symbol (blank)
c   1 square
c   2 'x'
c   3 diamond
c   4 octagon
c   5 plus
c   6 square and 'x'
c   7 diamond and plus
c   8 triangle
c   9 triangle (point down)
c  10 star
c  11 solid square
c  12 SMALL DOT
c  13 solid diamond
c  14 solid octagon
C  15 BOLD SQUARE
C  16 BOLD OCTAGON

c  k symbol size (DIAMETER) in .01 inches
c
      if (is.le.0 .or. k.lt.0) return
      call where (x,y,a)
      a=k*.005
	KK=2*K
      go to (10,20,30,40,50,10,30,80,90,100,110,120,130,140,150,160),is

c--square
10    call plot (x+a,y+a,3)
      call plot (x+a,y-a,2)
      call plot (x-a,y-a,2)
      call plot (x-a,y+a,2)
      call plot (x+a,y+a,2)
      if (is.eq.6) go to 22
      go to 99

c--'x'
20    call plot (x+a,y+a,3)
22    call plot (x-a,y-a,2)
      call plot (x-a,y+a,3)
      call plot (x+a,y-a,2)
      go to 99
c--diamond
30	A=A*1.414
      call plot (x,y+a,3)
      call plot (x+a,y,2)
      call plot (x,y-a,2)
      call plot (x-a,y,2)
      call plot (x,y+a,2)
      if (is.NE.7) go to 99
	CALL PLOT (X,Y-A,2)
	CALL PLOT (X-A,Y,2)
	CALL PLOT (X+A,Y,2)
      go to 99
c--octagon
40    c=.38*a
      call plot (x-c,y+a,3)
      call plot (x+c,y+a,2)
      call plot (x+a,y+c,2)
      call plot (x+a,y-c,2)
      call plot (x+c,y-a,2)
      call plot (x-c,y-a,2)
      call plot (x-a,y-c,2)
      call plot (x-a,y+c,2)
      call plot (x-c,y+a,2)
      go to 99
c--plus
50    call plot (x,y+a,3)
52    call plot (x,y-a,2)
      call plot (x-a,y,3)
      call plot (x+a,y,2)
      go to 99
c--triangle (point up)
80    call plot (x,y+a,3)
      call plot (x+a,y-a,2)
      call plot (x-a,y-a,2)
      call plot (x,y+a,2)
      go to 99
c--triangle (point down)
90    call plot (x+a,y+a,3)
      call plot (x,y-a,2)
      call plot (x-a,y+a,2)
      call plot (x+a,y+a,2)
      go to 99
c--star
100   call plot (x,y+a,3)
	CALL PLOT (X+.59*A,Y-.81*A,2)
	CALL PLOT (X-.95*A,Y+.31*A,2)
	CALL PLOT (X+.95*A,Y+.31*A,2)
	CALL PLOT (X-.59*A,Y-.81*A,2)
      call plot (x,y+a,2)
      go to 99

c--solid square
110	DO I=1,KK
	  A=I*.0025
	  call plot (x+a,y+a,2)
	  call plot (x+a,y-a,2)
	  call plot (x-a,y-a,2)
	  call plot (x-a,y+a,2)
	  call plot (x+a,y+a,2)
	END DO
      go to 99

c--SMALL DOT
C120   call plot (x+.01,y,2)
C      call plot (x+.01,y+.01,2)
C      call plot (x,y+.01,2)
C      call plot (x,y,2)
120	CALL PLOT (X,Y,2)
      go to 99

c--solid diamond
130   do 135 i=1,KK/2
      a=i*.005
      call plot (x,y+a,2)
      call plot (x+a,y,2)
      call plot (x,y-a,2)
      call plot (x-a,y,2)
      call plot (x,y+a,2)
135   continue
      go to 99

c--solid octagon
140   do 145 i=1,KK/2
      a=i*.005
      c=.38*a
      call plot (x-c,y+a,2)
      call plot (x+c,y+a,2)
      call plot (x+a,y+c,2)
      call plot (x+a,y-c,2)
      call plot (x+c,y-a,2)
      call plot (x-c,y-a,2)
      call plot (x-a,y-c,2)
      call plot (x-a,y+c,2)
      call plot (x-c,y+a,2)
145   continue
	GOTO 99

C--BOLD SQUARE
150	DO I=KK-1,KK
	  A=I*.0025
	  call plot (x+a,y+a,3)
	  call plot (x+a,y-a,2)
	  call plot (x-a,y-a,2)
	  call plot (x-a,y+a,2)
	  call plot (x+a,y+a,2)
	END DO
      go to 99

160	DO I=KK-1,KK
	  A=I*.0025
	  C=.38*A
	  call plot (x-c,y+a,3)
	  call plot (x+c,y+a,2)
	  call plot (x+a,y+c,2)
	  call plot (x+a,y-c,2)
	  call plot (x+c,y-a,2)
	  call plot (x-c,y-a,2)
	  call plot (x-a,y-c,2)
	  call plot (x-a,y+c,2)
	  call plot (x-c,y+a,2)
	END DO

99    call plot (x,y,3)
	return
	end
