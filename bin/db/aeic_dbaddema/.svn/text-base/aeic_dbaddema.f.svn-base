      program aeic_dbaddema

c    Program to compute angle of emergence and write to table arrival.
c    Two velocity models are used depending on location of the earthquake
c    scak and northak south and north of 62.5N, respectively.
c    The program needs one argument: database name.

	include "db.i"
	DIMENSION DTDS(30),SLO(30),STOP(30)

      common/smodel/nl,slo,stop,eslo(30),angle,rps
      common /ttdrv/ dtdr,dtdz,dtds,INDPTH,TEST,LOWLR,JGO

        integer argc 
        character argv 
	character dbin*256, tempdb*50,expr*100,iphase*5,arr*1, station*5
	real*8 delta, ema, lat, depth, pi
	integer	rc
	integer db(4), dbev(4), dbl(4), dbo(4), dbass(4), dbarr(4)
	integer dbtemp(4), dbtemp1(4), dbtemp2(4), dbout(4)
	integer	orid, table_writeable, result
	real dist, depth_tmp

c	FILE	*fp, *qp, *bp, *ep;
c	Tbl	*sortkeys, *joinkeys, *joinkeys2;

c	Parse command line 

	pi = 3.14159265358979323846264338327950288
	m = iargc()
	call getargs (dbin)
	
c	Open database

	idb = dbopen(dbin, "r+", db)

      	if (idb .lt. 0) then
		call die("aeic_dbaddema: Unable to open database ")
	endif

c	loop through origins

	call dblookup(dbo,db, "", "origin", "", "")
	call dbquery(dbo, dbRECORD_COUNT, nrows)
	if (nrows .lt. 1) then
		call die("aeic_dbaddema: No events to process")
	endif

c	start loop for origins

c	print*,'TEST: start loop through origins'

	do i = 0, nrows-1
		dbo(4) = i
		idb = dbgetv(dbo,"","lat",lat,"depth",depth,"orid",orid,0)
        	print*, 'orid = ', orid,', lat = ',real(lat), 
     *        	', depth = ',real(depth)

c		subset view by current orid
	 	write (expr,100) orid
  100 		format('orid == ',i10)
c	 	print*,'expr ',expr

c		join origin, assoc, arrival tables
	  	call dbsubset (dbev,dbo, expr, "")
	  	call dbquery( dbev, dbRECORD_COUNT, n )
	  	print*, n, ' origins for orid: ',orid

		call dblookup(dbtemp1, db, "", "assoc", "", "" )
		call dbquery( dbtemp1, dbRECORD_COUNT, n )
		print*, n, ' records in assoc table '
		
		call dblookup(dbtemp2, db, "", "arrival", "", "" )
		call dbquery( dbtemp2, dbRECORD_COUNT, n )
		print*, n, ' records in arrival table '

		call dbjoin(dbtemp, dbev, dbtemp1, 0,0,0,0,0 )
c		call dbquery( dbtemp1, dbRECORD_COUNT, n )
c		print*, n, ' records in dbtemp1 table '


		call dbjoin(dbl, dbtemp, dbtemp2, 0,0,0,0,0 )
	  	call dbquery( dbl, dbRECORD_COUNT, n )
       		print*, n, ' arrivals for orid: ',orid	  
	  	
	  	print*, 'Computing emas for orid: ', orid
c		print*,'TEST: calling MODLIN'
      
        	call modlin(lat)	
        
	  	if (n .lt. 1) then
			print*, 'aeic_dbaddema: No arrivals for orid ', orid
	  	else
c			call dblookup(dbarr, dbl, 0, "arrival", 0, 0)
c			call dbquery(db,dbTABLE_IS_WRITABLE,table_writeable)
      			table_writeable=1
      			
			if(table_writeable .ne. 0) then
		
c			start loop for arrivals

		  	do j = 0,n-1
		  	dbl(4) = j

c			get delta value 
		  	idb = dbgetv(dbl,"","delta",delta,"sta",station,0)

c			check for null value and compute angle of emergence 

		  	if (delta .eq. -1.0) then
      			print*, 'aeic_dbaddema error: no delta entry for orid',
     *			 orid,' station ',station
		  	
		  	else 
c 			print*, 'station ', station,' at delta', delta
c     * 		,'and depth', depth, 'and lat', lat 
     	
        		dist = real(delta)*111.195
        		depth_tmp = real(depth)

			TTIME = TTINVR(dist,depth_tmp,1)
			if (angle.gt.0) ema=180-(asin(angle)*180.0/pi)
			if (angle.lt.0) ema=-(asin(angle)*180.0/pi)
			if (dist.ge.1000.0) ema =-1
	
c     			print*, 'station ', station,' at delta ',real(delta),
c     *     		' dist=',dist,' angle=', real(ema), ema 

c     			write ema to arrival table 

		     	call dblookup(dbarr, dbl, 0, "arrival", 0, 0)
		     	result = dbputv (dbarr,"", "ema", ema, "")

		   	endif
		   	enddo

		else
		call die("aeic_dbaddema: arrival table not writeable")
		endif
	endif
	
c	clear_register (1)
	print*,' '
	enddo

	stop
	end

c**********************************************************************
	subroutine usage()

	print*,"USAGE: aeic_dbaddema dbin"
	print*,"where: dbin = input database"

	return
	end
c**********************************************************************
      subroutine getargs (dbin )

      character*(*)       dbin

      n = iargc()
      if (n .lt. 1) then
	print *,
     +  'usage: aeic_dbaddema database '
	stop
      end if
      call getarg (1, dbin)

      return
      end
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      REAL function ttinvr(r,hpz,jps)               
c                     
c --- ttinvr calculates minimum traveltime in arbitrary layered model.         
c     source is depth -hpz- and receiver is offset distance -r- from           
c     source.  model consists  of horizontal , plane parallel layers           
c     (n-1 layers over homogeneous halfspace).              
c --- model is specified by              
c --- routine also calculates array containing nodes of minimum                
c     traveltime path, an array of partial derivatives of travel               
c     time with respect to all model slownesses, and partial derivatives       
c     with respect to source depth and range                
c --- program modified from ttlvz and including some conventions               
c     used by chee wu chou for use in three-dimensional traveltime             
c     calculations    
c                     
c       by robert s. crosson             
c       geophysics program               
c       university of washington         
c       seattle, washington  98195       
c       april, 1979   
c                     
      DIMENSION D(50),h(50)             
     1,slo(30),stop(30),dtds(30),rnod(60),znod(60)      
      common /smodel/ nslay,slo,stop,eslo(30),angle,rps 
      common /ttdrv/ dtdr,dtdz,dtds,INDPTH,TEST,LOWLR,JGO
c      common/DIM/XDIM,VDIM
C      data test/0.0002/
C	TEST=TEST/XDIM   !XDIM NON-DIMENSIONALIZING FACTOR             
c      jps = 1 for p wave                
c            2 for s wave                
c      angle = sin takeoff angle         
c --- note following conventions         
c       indpth = 2 if path is refracted path                
c                1 if path is direct path                   
c       lowlr = index of lowest layer penetrated by raypath 
c --- test is convergence criterion for direct ray path calculation             
c     when the direct ray gets within distance -test- of -r-, iteration        
c      stops.         
c --- if source is above receiver, interchange elevations of source            
c     and receiver and set flag -sflg-   
c
C      print*,'start TTINVR'
      sflg = 1     
      if (hpz.ge.stop(1)) go to 95       
      t1 = hpz        
      rr = r          
      hpz = stop(1)   
      stop(1) = t1    
      sflg = -1    
   95 continue        
c     reverse p and s slowness for s waves    
C      print*,'call REV'              
      if(jps.eq.2)call rev(slo,eslo,nslay)  
C      print*,'back from REV'                
c --- locate layer containing hypocenter and store index in lhpz.              
c     note that if hypocenter on a boundary, it is placed in overlying         
c     layer for refraction path calculation                 
      do 100 i=1,nslay                   
        if (hpz.le.stop(i)) go to 110    
  100 continue        
      lhpz = nslay    
      go to 120       
  110 lhpz = i-1      
      if(lhpz.eq.0) lhpz=1               
  120 continue        
c --- assign layer thicknesses to internal array h only for those              
c     layers above source.               
      lim = lhpz-1    
      do 130 i=1,lim                     
        h(i) = stop(i+1)-stop(i)         
 130  continue        
      h(lhpz) = abs(hpz-stop(lhpz))   !WAS DOUBLE PRECISION
c --- initialize for refraction path calculations           
      indpth = 2      
      ttime = 1.0d+10                    
c --- if source in halfspace, skip refracted ray calculations and              
c     go to direct ray calculation       
      if (lhpz.eq.nslay) go to 220       
c --- prepare for loop which examines for all possible refracted               
c     paths (paths going below source)   
c --- find maximum velocity (minimum slowness) from source to surface          
      smin = 1.0d+35                     
      do 140 i=1,lhpz                    
        smin = min(slo(i),smin)        
 140  continue        
C     print*,'step 1'
c --- no refraction path can exist off any deeper layer which has              
c     slowness greater than all slownesses between layer and surface           
c     (this would be low velocity zone)  
      istrt = lhpz + 1                   
      do 210 lay=istrt,nslay             
c --- check to see if low velocity criterion excludes refracted path           
        if (slo(lay).ge.smin) go to 210  
      smin=slo(lay)   
      p=smin          
c --- calculate partial slant distnaces, d(k)               
        lim = lay-1   
        do 150 i=1,lim                   
          t1 = p/slo(i)                  
          t2 = t1*t1                     
c         t3 = SQrt(1-t2)            
          t3=sqrt((1-t1)*(1+t1))  
          d(i) = 2*(stop(i+1)-stop(i))/t3                
  150 continue        
c --- now calculate that part of slant distance from source to surface         
c     using h array, and subtract out appropriate part from twice above        
C     print*,'step 2'
       do 160 i=1,lhpz                   
          t1 = p/slo(i)                  
          t2 = t1*t1                     
c         t3 = SQrt(1-t2)            
          t3 = SQrt((1-t1)*(1+t1))                   
          d(i) = d(i)-h(i)/t3            
 160    continue      
c --- now d(i) array contains correct distribution of partial slant            
c      distances for all layers.  Calculate total offset distance              
         sum = 0   
         do 170 i=1,lim                  
           t1 = p/slo(i)                 
           sum = d(i)*t1+sum             
 170  continue        
c --- sum now contains total offset distance.  Compare against r               
        if (sum-r) 180,180,210           
 180  continue        
c --- refraction path possible, assign final partial distance in lay           
      d(lay)=r-sum    
c --- can now calculate total travel time by inner product of slant            
c     path distances and slowness vector  
C     print*,'step 3'
                  
        tt = dotf(d,1,slo,1,lay)         
c --- if travel time is a minimum, save partial path distances in              
c     derivative array and go on to next possible refracted path               
        if (ttime.le.tt) go to 210       
        ttime = tt    
        do 190 i=lay,nslay               
          dtds(i) = 0                 
  190 continue        
        do 200 i=1,lay                   
          dtds(i) = d(i)                 
  200 continue        
        dtdr = p      
      angle=p/slo(lhpz)                  
        lowlr = lay   

c --- note that we can construct final node array from partial                 
c     derivative array, as well as reconstructing d array   
  210 continue        
c --- refraction path calculations are now completed.  variable lowlr          
c     contains the minimum time raypath bottom layer index and                 
c     dtds array contains the partial slant distances in all layers            
c --- proceed to calculate direct ray path for comparison.  
  220 continue        
C     print*,'step 4'
      do 230 i=1,nslay                   
        d(i) = 0   
  230 continue        
c --- if source in layer 1, use homogeneous layer calculation                  
      if (lhpz.gt.1) go to 240           
c     d(1) = SQrt(r*r+h(lhpz)*h(lhpz))  
      d(1) = SQrt((r+h(lhpz))*(r+h(lhpz)) - 2*h(lhpz)*r)                   
      p = 0        
      if (d(1).eq.0) go to 320        
      p = slo(lhpz)*r/d(1)               
      go to 320       
c --- proceed to calculate direct path by iteration         
  240 continue        
c --- find minimum slowness between surface and source      
      smin = slo(1)   
      do 250 i=2,lhpz                    
        smin = min(smin,slo(i))        
  250 continue        
c --- find maximum permissable value of p (p must lay between 0.               
c     and pmax)       
      pmax = smin     
      p = 0.5d0*pmax                     
c --- find a value of p which is greater than true p by half range             
c     divide and test to see when ray emerges at distance greater              
c     than r          
C      print*,'step 5'
      icount=0
 260  p = (p+pmax)/2   
      icount=icount+1  
C      print*,'p=',p,'pmax=',pmax     
    
      sum = 0      
      do 270 i=1,lhpz                    
      isub=270        
      aslo=slo(i)     
        t1 = p/slo(i)                    
        t2 = t1*t1    
c       t3 = sqrt(1.-t2)                 
        t3 = SQrt((1-t1)*(1+t1))   
	IF(t3.EQ.0.or.icount.gt.50)then
	jgo=1
C        print*,'return to main program from TTINVR'
	RETURN
	end if
        d(i) = h(i)/t3       
c	CALL ERRTST(73,JGO)
      ad=d(i)         
        sum = d(i)*t1+sum                
 270  continue        
      isub=2      
      if (r-sum) 280,310,260             
 280  continue        
C      PRINT*,'          start newton convergence'
c --- perform newton convergence on true p from top down.  permit              
c     no more than 20 iterations by nesting in do loop      
      isub=6          
      ar=r            
      do 300 i=1,20   
      isub=300        
      sder=0       
      sum=0        
        do 290 j=1,lhpz                  
          t1 = p/slo(j)                                                        
          t2 = t1*t1                                                           
c         t3 = SQrt(1-t2)                                                  
          t3 = SQrt((1-t1)*(1+t1))                                      
          d(j) = h(j)/t3                                                       
          ad=d(j)                                                              
          sum = d(j)*t1+sum                                                    
          den = (t3*t3*t3)*slo(j)                                              
          aslo=slo(j)                                                          
      sder=h(j)/den+sder                                                       
 290  continue                                                                 
      isub=4                                                                   
      dmdpo = r-sum                                                            
      if (abs(dmdpo).lt.test) go to 310    !WAS DOUBLE                                   
      p = dmdpo/sder+p                                                         
 300  continue                                                                 
 310  continue                                                                 
      isub=310                                                                 
c --- p has now been found to sufficient accuracy, calculate direct            
c     wave travel time and compare with refracted time to find minimum         
 320  continue       
                                                  
      tt = dotf(d,1,slo,1,lhpz)                                                
      if (tt.gt.ttime) go to 350                                               
c --- following code assumes direct wave time is minimum.  Variables           
c     are set and nodes are filled on this basis                               
C      PRINT*,'        assume direct wave'          
      ttime = tt                                                               
      indpth = 1                                                               
      nnod = lhpz+1                                                            
      znod(1) = hpz                                                            
      rnod(1)=0                                                             
      znod(nnod) = stop(1)                                                     
      rnod(nnod) = r                                                           
c --- accumulates progressive range increments from source                     
      rsum = 0                                                              
      lidx = lhpz                                                              
      lim = lhpz-1                                                             
      do 330 i=1,lim                                                           
        lidx = lhpz-i+1                                                        
        rsum = d(lidx)*p/slo(lidx)+rsum                                        
        rnod(i+1) = rsum                                                       
      znod(i+1)=stop(lidx)                                                     
  330  continue                                                                
      dtdr = p                                                                 
      t1=p/slo(lhpz)                                                           
      angle=t1                                                                 
c     t1=t1*t1                                                                 
c     dtdz = SQrt(1.d0-t1)*slo(lhpz)                                          
	IF(T1.GT.1)THEN
	PRINT*,' >>>>>>>>>>>>>> T1:',T1
	T1=1
	END IF
      dtdz = SQrt((1 -t1)*(1 +t1))*slo(lhpz)                              
      t1=t1*t1                                                                 
      lowlr = lhpz                                                             
      do 340 i=1,nslay                                                         
        dtds(i) = d(i)                                                         
 340  continue                                                                 
      ttinvr = ttime                                                           
      if (sflg.eq.1 ) go to 400                                              
      t1 = hpz                                                                 
      hpz = stop(1)                                                            
      stop(1) = t1                                                             
      dtdz = -dtdz                                                             
      call revarr(nnod,rnod)                                                   
      t1 = rnod(1)                                                             
      do 345 i=1,nnod                                                          
        rnod(i) = t1-rnod(i)                                                   
 345  continue                                                                 
      call revarr(nnod,znod)                                                   
      go to 400                                                                
 350  continue                                                                 
c --- following code assumes refraction path is minimum time.                  
c     nodes and derivatives are assigned on this basis                         
C      PRINT*,'        assume refraction wave'          
      p = dtdr                                                                 
      angle = -p/slo(lhpz)                                                     
      nnod = 1                                                                 
      znod(1) = hpz                                                            
      rnod(1) = 0                                                            
c --- reconstruct correct d(i) array from dtds array                           
      do 360 i=1,nslay                                                         
        d(i) = dtds(i)                                                         
 360  continue                                                                 
c --- calculate offset to first layer below source                             
      t1 = stop(lhpz+1)-stop(lhpz)                                             
      t2 = stop(lhpz+1)-hpz                                                    
      dtemp = t2/(t1+t2)*d(lhpz)                                               
      rsum = 0                                                               
c --- now assign nodes for points below source                                 
      lim = lowlr-1                                                            
      do 370 i=lhpz,lim                                                        
        rsum = dtemp*p/slo(i)+rsum                                             
        nnod = nnod+1                                                          
        rnod(nnod) = rsum                                                      
        znod(nnod) = stop(i+1)                                                 
        dtemp = d(i+1)/2                                                     
 370  continue                                                                 
c --- assign nodes for part of path in lowlr                                   
      rsum = rsum+d(lowlr)                                                     
      nnod=nnod+1                                                              
      rnod(nnod) = rsum                                                        
      znod(nnod) = stop(lowlr)                                                 
c --- work way back upgoing side of raypath                                    
      lim=lowlr-1                                                              
      do 380 i=1,lim                                                           
        lidx = lowlr-i                                                         
        t1 = p/slo(lidx)                                                       
        t2 = t1*t1                                                             
c       t3 = SQrt(1 -t2)                                                    
        t3 = SQrt((1 -t1)*(1 +t1))                                        
        rsum = (stop(lidx+1)-stop(lidx))*t1/t3+rsum                            
      nnod=nnod+1                                                              
        rnod(nnod) = rsum                                                      
        znod(nnod) = stop(lidx)                                                
 380  continue                                                                 
c --- all nodes assigned, now assign derivatives                               
      t1 = (p/slo(lhpz))*(p/slo(lhpz))                                         
c     dtdz = -SQrt(1 -t1)*slo(lhpz)                                         
      dtdz = -SQrt((1 -(p/slo(lhpz)))*(1 +(p/slo(lhpz))))*slo(lhpz)       
      ttinvr = ttime                                                           
      isub=380                                                                 
      if (sflg.eq.1 ) go to 400                                              
      t1 = hpz                                                                 
      hpz = stop(1)                                                            
      stop(1) = t1                                                             
      call revarr(nnod,rnod)                                                   
      t1=rnod(1)                                                               
      do 385 i=1,nnod                                                          
        rnod(i) = t1-rnod(i)                                                   
 385  continue                                                                 
      call revarr(nnod,znod)                                                   
  400 if(jps.eq.2)call rev(slo,eslo,nslay)                                     
c	print*,'            indpth,lowlr:',indpth,lowlr
c	print*,'            ttime,angle:',ttime,angle
      return
      end                                                                      
                                                                              
c**********************************************************************
      subroutine rev(x,y,n)                                                    
c	reverse p ans s slownesess
      dimension x(100),y(100)                                                      
      do 100 i=1,n                                                             
      temp=x(i)                                                                
      x(i)=y(i)                                                                
      y(i)=temp
  100 continue                                                                 
      return                                                                   
      end                                                                      

c**********************************************************************
      subroutine revarr(n,a)                                                   

c --- revarr reverses or inverts the order of elements in array a              
      dimension a(n)                                                           
      lim = n/2                                                                
      do 100 i=1,lim                                                           
        temp = a(i)                                                            
        index = n-i+1                                                          
        a(i) = a(index)                                                        
        a(index) = temp                                                        
 100  continue                                                                 
      return                                                                   
      end                                                                      

c**********************************************************************
      REAL function dotf(d,i,slo,j,n)                              
      DIMENSION d(100),slo(100)                                         
      sum=0                                                                  
      do 100 ii=j,n                                                            
  100 sum=sum + d(ii)*slo(ii)                                                  
      dotf=sum                                                                 
      return                                                                   
      end                                                                      

c**********************************************************************
      subroutine modlin(olat)                                                        

      common/smodel/nl,slo,stop,eslo(30),angle,rps
      dimension vp(30),vs(30),d(30),slo(30),stop(30)
      real scak(9,2),northak(4,2)
      real*8 olat
      data scak /0, 4,  10, 15, 20, 25, 33, 47, 65,
     *		5.3,5.6,6.2,6.9,7.4,7.7,7.9,8.1,8.3/
      data northak /0,24.4,40.2,76,5.9,7.4,7.9,8.29/
      vpvs=1.76

c      print*,'MODLIN olat = ',olat
      
      if(olat.le.62.5) then

		nl=9
      		do 25 i=1,nl      
      		d(i)=scak(i,1)
      		vp(i)=scak(i,2)
      		vs(i)=vp(i)/VPVS                   
   25 		continue  
                                                                  
      		do 100 l=1,nl  
      		stop(l)=d(l)   
      		slo(l)=1./vp(l)
      		eslo(l)=1./vs(l)                                                         
100		CONTINUE

		print*,'MODLIN: Using scak model'

        else
      
		nl=4
      		do 26 i=1,nl      
      		d(i)=northak(i,1)
      		vp(i)=northak(i,2)
      		vs(i)=vp(i)/VPVS                   
   26 		continue  
                                                                  
      		do 101 l=1,nl  
      		stop(l)=d(l)   
      		slo(l)=1./vp(l)
      		eslo(l)=1./vs(l)                                                         
101		CONTINUE

		print*,'MODLIN: Using northak model'

      endif

      return                                                                   
      end                                                                      

c**********************************************************************
