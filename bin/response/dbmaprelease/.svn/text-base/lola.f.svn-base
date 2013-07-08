      SUBROUTINE LOLA(D,AZI,LATLR,LONLR,LATE,LONE,NUM)
C
C  Compute lat, lon of a point at a distance and angle from the start
C  point
C
C Input :
C     D       : Distance from start point in degrees
C     AZI     : Angle between start point - north and the wanted point
C     LATLR   : Geocentric latitude of start point in degrees
C     LONLR   : Longitude of start point in degrees
C Output :
C     LATE    : Geodetic latitude of computed point in degrees
C     LONE    : Longitude of computed point in degrees
C     NUM     : Error indicator
C               1 - Everything is OK
C               0 - Input error
C
      REAL LATER,LATE,LONER,LONE,LONLR,LATLR
      DOUBLE PRECISION XD,XSLAT,XLAT,XAZ
      XKM=111.195*D
      PI=4.0*atan(1.0)
      dtr=pi/180.
      TWOPI=2*PI
      XD=(D/180)*PI
      XAZ=(AZI/180)*PI
      XSLAT=LATLR*dtr
      NUM=1
C
C  COMPUTE THE LATITUDE AND LONGITUDE OF THE EVENT
C
      AUG=DSIN(XSLAT)*DCOS(XD)+DCOS(XSLAT)*DSIN(XD)*DCOS(XAZ)
      IF(ABS(AUG).GT.1.0) GO TO 891
      ARSIN=ASIN(AUG)
      GO TO 890
  891 CONTINUE
C
      NUM=NUM-1
      LONE=0
      LATE=0
      GO TO 27
  890 LATER=ARSIN
      XLAT=LATER
      IF(COS(LATER))45,46,45
   45 IF(COS(LATLR*dtr))47,46,47
   46 LONER=0.
      GO TO 34
   47 CONTINUE
      CK1=(DCOS(XD)-DSIN(XSLAT)*DSIN(XLAT))/(DCOS(XSLAT)*DCOS(XLAT))
      CK2=(DSIN(XAZ)*DSIN(XD))/DCOS(XLAT)
      IF(CK1)30,37,37
   30 IF(CK2)880,882,882
  880 IN=1
      GO TO 881
   33 LONER=dtr*LONLR-PI-ARSIN
      GO TO 34
  882 IN=2
      GO TO 883
   32 LONER=dtr*LONLR+ARCOS
      GO TO 34
   37 IF(CK2)884,885,885
  884 IN=3
      GO TO 881
   31 LONER=dtr*LONLR+ARSIN
      GO TO 34
  885 IN=4
  883 IF(ABS(CK1).GT.1.0) GO TO 887
      ARCOS=ACOS(CK1)
      GO TO 888
  881 IF(ABS(CK2).GT.1.0) GO TO 887
      ARSIN=ASIN(CK2)
  888 GO TO (33,32,31,36),IN
  887 CONTINUE
C
      LONER=0.0
      GO TO 34
   36 LONER=dtr*LONLR+ARCOS
C
C     CONVERTING LONGITUDE AND LATITUDE FROM RADIANS TO DEGREES
C     AND REDUCING LONGITUDE TO A FORM LESS THAN 360 DEGREES
C
34    CONTINUE
      ALON=ABS(LONER)
506   IF(ALON-2.*PI)116,115,115
115   ALON=ALON-2.*PI
      GO TO 506
116   LONER=SIGN(ALON,LONER)
      IF(ABS(LONER)-PI)211,211,210
  210 LONER=LONER-SIGN(TWOPI,LONER)
  211 LONE=(LONER*180.)/PI
C
C     LATITUDE IS ALSO CONVERTED FROM GEOCENTRIC TO GEODETIC COORDINATES
C
      LATE=GEOD(LATER)
   27 RETURN
      END
      FUNCTION GEOD(X)
C
C     GEOCENTRIC TO GEODETIC LATITUDE
C          X IN RADIANS   GEOD IN DEGREES
C
      PI=4.0*atan(1.0)
      ARD=X
      SX=SIN(ARD)
      CX=COS(ARD)*0.993277
      ANG=ATAN2(SX,CX)
      GEOD=ANG*180./PI
      RETURN
 
      END

