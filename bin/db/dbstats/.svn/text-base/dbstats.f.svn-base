      program dbstats
c************************************************************************
c     August, 2002. N.Ratchkovski
c     Program calculates magnitude and travel-time station residuals 
c     for an input database. It needs site, stamag, netmag, and assoc 
c     tables.
c     Output:
c     sta_ttres?.ps - postscript file with travel time station residuals,
c     sta_magres?.ps - postscript file with magnitude station residuals,
c     dbstats.ttres - ASCII file with travel time station residuals:
c                 sta_name ave_ttres min_ttres max_ttres stdev Nobs',
c     dbstats.magres - ASCII file with travel time station residuals:
c               sta_name ave_magres min_magres max_magres stdev Nobs.
c************************************************************************

      include "db.i"
      parameter (MAXSTA = 500)

      integer argc
      character argv

      character new_sta_name*9
      character dbin*256
      character station*4, expr*80, title*36, plot_fig_name*20 
      character*4 mag_station_name(500)
      character*4 tt_station_name(500)
      character*20 time_start_str, time_end_str
      real*8 magres, sum_magres, sum_sqr, var, stdev
      real*8 ave_magres(500), magres_stdev(500)
      real*8 ave_ttres_array(500), ttres_stdev(500)
      real*8 sighi, siglow
      real*8 ttres, sum_ttres, sum_ttsqr, ave_ttres
      real*8 stamag, mag, slon, slat
      real*8 time_start, time_end
      real temp_end, temp_start, jj, min, max, corr_tmp
      integer dbsrt(4), dbs(4), dbsm(4), dbl(4), db(4), dbout(4), 
     *dbtemp(4), dbo(4), dba(4)
      integer ttrescnt, num_ttres(500), num_magres(500)
      integer orid, lincnt
      integer n, nsm, nrows, nsta, nres, nor
      integer magstacnt, ttstacnt
      integer fp, ep, mp, tp
      integer rc,i, keys, n_tt_fig, n_mag_fig

c     Parse command line 
c     get mandatory command line arguments 
      call getargs (dbin)

c     Open database 
 
      rc = dbopen(dbin, "r+", db)
      if (rc .lt. 0) then
        call die("dbstats: Unable to open database.")
      endif

      lincnt=1
      magstacnt=1
      ttstacnt=1

c     find time span of the input database

      call dblookup (dbl, db, "", "origin", "", "")
      call dbquery(dbl,dbRECORD_COUNT, nor)
      print*,'Number of origins in origin table nor = ', nor
      if (nor .lt. 1) then
        print*, 'dbstats: No origins in origin table'
      else
        expr = "time"
        call strtbl(keys, expr, 0)
        call dbsort(dbsrt, dbl, keys, 0, "")
        dbsrt(4) = 0
        idb = dbgetv(dbsrt,0,"time",time_start,0)
        dbsrt(4) = nor-1
        idb = dbgetv(dbsrt,0,"time",time_end,0)
        call strdate(time_start_str,time_start)
        print*,'time_start_str = ', time_start_str
        call strdate(time_end_str,time_end)
        print*,'time_end_str = ', time_end_str
      endif


c     open site table and look for matches in stamag table

      call dblookup (dbs, db, "", "site", "", "")
      expr = "offdate == -1"
      call dbsubset (dbs, dbs, expr, 0)
      call dbquery(dbs,dbRECORD_COUNT, nsta)
      if (nsta .lt. 1) then
        call die ("dbstats: No stations to process in site table")
      endif
      print*,'Nsta in site database to process = ',nsta

c     open output files
      fp=1
      ep=2
      mp=7
      tp=4
      open(unit=fp,file="dbstats.out")
      open(unit=ep,file="dbstats.err")
      open(unit=mp,file="dbstats.magres")
      open(unit=tp,file="dbstats.ttres")

c     loop through stations and compute mag. and tt residual
      print*
      print*,'Calculating magnitude and travel time residuals...'

      do i = 0, nsta-1
      dbs(4) = i
      sum_magres=0
      sum_sqr=0
      nsm_new=0

c     get station name and coordinates
      idb=dbgetv(dbs,0,"site.lat",slat,"site.lon",slon,"sta",station,0)
      call dblookup(dbsm, db, "", "stamag", "", "")

c     subset stamag for station
        if(station(4:4) .eq. " ") then
          write(expr,100) station
  	else
          write(expr,101) station
  	endif
  	
        call dbsubset (dbsm,dbsm, expr, 0)
        call dbquery (dbsm, dbRECORD_COUNT, nsm)
        
c     calculating magnitude residuals
	  if (nsm .gt. 0) then

            do j = 0, nsm-1
	      dbsm(4) = j

  	      jdb = dbgetv(dbsm,0,"magnitude",stamag,"orid",orid,0)

c     check if stamag value corresponds to orid in origin table
		if (orid .gt. 0) then
  	          write(expr,200) orid

c     subset netmag table for orid
  	          call dblookup(dbo, db, "", "netmag", "", "")
                  call dbsubset (dbo, dbo, expr, 0)
                  call dbquery (dbo, dbRECORD_COUNT, nrows)
                
c     get event magnitude
		  dbo(4)=0
  	          kdb = dbgetv (dbo, 0, "magnitude", mag, 0)

c     calculate sum. res.
		  sum_magres=sum_magres+mag-stamag
	          sum_sqr=sum_sqr+((mag-stamag)*(mag-stamag))
	          nsm_new=nsm_new+1
	        endif

	    enddo
	      
c     calculate ave. res. and std
	    if (nsm_new.ne.0) then
	      nsm=nsm_new
	      magres=sum_magres/nsm
	      var=(nsm*sum_sqr-sum_magres*sum_magres)/(nsm*(nsm-1.0))
	      stdev=sqrt(var)
	      siglow=magres-stdev
	      sighi=magres+stdev
	    endif

c 	write arrays station_name, ave_magres, magres_stdev, num_magres  
	    if (nsm.gt.1 .and. nsm_new.ne.0) then
	      mag_station_name(magstacnt)=station
	      ave_magres(magstacnt)=magres
	      magres_stdev(magstacnt)=stdev
	      num_magres(magstacnt)=nsm
	      magstacnt=magstacnt+1
c	      write(*,302) mag_station_name(magstacnt-1),magres,
c     *	      stdev,nsm

c 	write results to flat files
	      write(mp,300) station,magres,siglow,sighi,stdev,nsm
            endif

          endif    
c     end if (nsm .gt. 0)    
      
c     subset assoc for station to get ttime residuals
	call dblookup(dba,db, 0, "assoc", 0, 0)

	  if(station(4:4) .eq. " ") then
	    write(expr,400) station
          else
	    write(expr,401) station
          endif

	call dbsubset (dba, dba, expr, 0)
        call dbquery (dba, dbRECORD_COUNT, nres)

c     calculate tt residuals
	   if (nres .gt. 0) then

	     sum_ttres=0
	     sum_ttsqr=0
	     ttrescnt=0

             do j = 0,nres-1
             dba(4) = j

             jdb = dbgetv(dba,0,"timeres",ttres,"orid",orid,
     *       "sta",station,0)
c     check for null timeres values 
	       if (ttres .eq. -999.0) then
                 print*,'dbstats error: no timeres entry for orid ',
     *		 orid,' station ',station
c     write(unit=ep, 'dbstats error: no timeres entry for orid %d station %4s \n", orid, station)
	       else
		  ttrescnt=ttrescnt+1
		  sum_ttres=sum_ttres+ttres
	          sum_ttsqr=sum_ttsqr+(ttres*ttres)
                endif

	      enddo

	      ave_ttres=sum_ttres/ttrescnt
	      var=(ttrescnt*sum_ttsqr-sum_ttres*sum_ttres)/
     *		(ttrescnt*(ttrescnt-1.0))
	      stdev=sqrt(var)
	      siglow=ave_ttres-stdev
	      sighi=ave_ttres+stdev

c      write arrays station_name, ave_ttres, ttres_stdev, num_ttres 

	      if(ttrescnt.gt.1.and.sum_ttres.ne.0.and.stdev.ne.0)then
	        tt_station_name(ttstacnt)=station
	        ave_ttres_array(ttstacnt)=ave_ttres
	        ttres_stdev(ttstacnt)=stdev
	        num_ttres(ttstacnt)=ttrescnt
	        ttstacnt=ttstacnt+1

c	ouput to flat files
c 	        write(*,301) station, ave_ttres, stdev, ttrescnt
                write(tp,300) station,ave_ttres,siglow,sighi,stdev,
     *                        ttrescnt
	      endif
	      
	    endif
c     end if (nres .gt. 0) 
	
c	dbfree(dbsm)
c	dbfree(dba)
	
      enddo
c     end do loop for stations

	close(fp)
	close(ep)
	close(mp)
	close(tp)
 
      print*,'Number of stations for travel time residuals plot = ',
     *ttstacnt-1
c     sort stations according to travel time residuals
      call sort_on_first_vector(ttstacnt-1,ave_ttres_array,
     &  tt_station_name,ttres_stdev,num_ttres)
 
c     plot results with niceplot 
c       do l = 1,ttstacnt-1
c       print*,tt_station_name(l),ave_ttres_array(l),ttres_stdev(l),
c     *num_ttres(l)
c       enddo

c     making separate plots for 50 stations each

      print*
      print*,'Making plots of station travel time residuals...'
      
      if ((ttstacnt-1).lt.50) then
        n_tt_fig = 1
      elseif (mod((ttstacnt-1),50).eq.0) then
        n_tt_fig = (ttstacnt-1)/50
      else
        n_tt_fig = (ttstacnt-1)/50+1
      endif
      
      print*,'Number of figures = ', n_tt_fig

      do l = 0, n_tt_fig-1
      
c     creating figure title
      write(title,502) time_start_str,time_end_str,l+1,n_tt_fig
c      print*, title

c     creating plotfile name
      write(plot_fig_name,501) l
      print*,'    plot_fig_name = ',plot_fig_name

      call clear()
      call initt(1,plot_fig_name,' ','sta_ttres',.7,0.,0.)

      if ((ttstacnt-1).lt.50) then
        istart = 1
        iend = ttstacnt-1
        nttsta = ttstacnt-1
      elseif (l.eq.(n_tt_fig-1)) then
        istart = 1+l*50
        iend = ttstacnt-1   
        nttsta = ttstacnt-1-l*50
      else
        istart = 1+l*50
        iend = istart+49   
        nttsta = 50
      endif 

c      print*,'istart=',istart
c      print*,'iend=',iend 
      print*,'        Number of stations to plot  = ',nttsta 

c      do i = istart,iend
c      print*,tt_station_name(i),ave_ttres_array(i),ttres_stdev(i),
c     *num_ttres(i)
c      enddo

c      call plot_tt_residuals(tt_station_name(istart:iend), nttsta,  
c     *     ave_ttres_array(istart:iend), ttres_stdev(istart:iend), 
c     *     num_ttres(istart:iend), title)
     

      call ltype(0)
      temp_end = iend+1
      temp_start = istart-1
      write(expr,102) title

c New changes to include number of readings for each station residual (N.R. Feb.2005)
c old plot dimensions
c      call axis(8.,6.,.8,.6,1.4,.9,temp_end,temp_start,1.,-1.,1.,5.,
c     &.1,.2,'(f0.0)','(f4.1)','Station','Station Residual (sec)',
c     &expr,0)
      call axis(8.,5.5,1.2,.6,1.4,1.4,temp_end,temp_start,1.,-1.,1.,5.,
     &.1,.2,'(f0.0)','(f4.1)','Station','Station Residual (sec)',
     &expr,0)
      call line(temp_start,0.,temp_end,0.,0.01,1,0)
      call chrsiz(.09,1.,0.)

      do 317 i=istart,iend
           jj=i
           min=ave_ttres_array(i)-ttres_stdev(i)
           max=ave_ttres_array(i)+ttres_stdev(i)
            if (num_ttres(i).lt.1000) then
              write(new_sta_name,105) tt_station_name(i),num_ttres(i)
            else
              write(new_sta_name,105) tt_station_name(i),999
            endif
           call line(jj,min,jj,max,0.01,1,0)
c           call text(jj,-1.2,90.,1,tt_station_name(i),1)
           call text(jj,-1.35,90.,1,new_sta_name,1)
 317  continue

      do 319 i=istart,iend
           jj=i
		corr_tmp=ave_ttres_array(i)
           call symbol(jj,corr_tmp,.03,.03,0.,5,0,1,.01,0)
  319 continue

      call finitt()

      enddo
      
c       do l = 1,magstacnt-1
c       print*,tt_station_name(l),ave_ttres_array(l),ttres_stdev(l),
c     *num_ttres(l)
c       enddo

c      call initt(1,'sta_magres.ps',' ','sta_magres',.7,1.,1.)
c      call plot_mag_residuals(mag_station_name,magstacnt-1, 
c     *		ave_magres, magres_stdev, num_magres, title)
c      call finitt()

      print*
      print*,'Number of stations for magnitude residuals plot = ',
     *magstacnt-1
c     sort stations according to magnitude residuals
      call sort_on_first_vector(magstacnt-1,ave_magres,
     &  mag_station_name,magres_stdev,num_magres)

      print*
      print*,'Making plots of station magnitude residuals...'
      
      if ((magstacnt-1).lt.50) then
        n_mag_fig = 1
      elseif (mod((magstacnt-1),50).eq.0) then
        n_mag_fig = (magstacnt-1)/50
      else
        n_mag_fig = (magstacnt-1)/50+1
      endif
      
      print*,'Number of figures = ', n_mag_fig

      do l = 0, n_mag_fig-1
      
c     creating figure title
      write(title,502) time_start_str,time_end_str,l+1,n_mag_fig
c      print*, title

c     creating plotfile name
      write(plot_fig_name,505) l
      print*,'    plot_fig_name = ',plot_fig_name

      call clear()
      call initt(1,plot_fig_name,' ','sta_magres',.7,0.,0.)

      if ((magstacnt-1).lt.50) then
        istart = 1
        iend = magstacnt-1
        nmagsta = magstacnt-1
      elseif (l.eq.(n_mag_fig-1)) then
        istart = 1+l*50
        iend = magstacnt-1   
        nmagsta = magstacnt-1-l*50
      else
        istart = 1+l*50
        iend = istart+49   
        nmagsta = 50
      endif 

c      print*,'istart=',istart
c      print*,'iend=',iend 
      print*,'        Number of stations to plot  = ',nmagsta 

      call ltype(0)
      temp_end = iend+1
      temp_start = istart-1
      write(expr,103) title

c New changes to include number of readings for each station residual (N.R. Feb.2005)
c old plot dimensions
c      call axis(8.,6.,.8,.6,1.4,.9,temp_end,temp_start,1.,-1.,1.,5.,
c     &.1,.2,'(f0.0)','(f4.1)','Station','Station Residual (sec)',
c     &expr,0)
      call axis(8.,5.5,1.2,.6,1.4,1.4,temp_end,temp_start,1.,-1.,1.,5.,
     &.1,.2,'(f0.0)','(f4.1)','Station',
     &'Station Magnitude Residual',expr,0)
      call line(temp_start,0.,temp_end,0.,0.01,1,0)
      call chrsiz(.09,1.,0.)

      do 316 i=istart,iend
           jj=i
           min=ave_magres(i)-magres_stdev(i)
           max=ave_magres(i)+magres_stdev(i)
c           new_sta_name(1:4) = mag_station_name(i)
            if (num_magres(i).lt.1000) then
              write(new_sta_name,105) mag_station_name(i),num_magres(i)
            else
              write(new_sta_name,105) mag_station_name(i),999
            endif
            call line(jj,min,jj,max,0.01,1,0)
c           call text(jj,-1.2,90.,1,mag_station_name(i),1)
            call text(jj,-1.35,90.,1,new_sta_name,1)
 316  continue

      do 318 i=istart,iend
           jj=i
		corr_tmp=ave_magres(i)
           call symbol(jj,corr_tmp,.03,.03,0.,5,0,1,.01,0)
  318 continue

      call finitt()

      enddo
      
      write(*,601)
      write(*,602)
      write(*,603)
      write(*,604)
      write(*,605)
      write(*,606)
      write(*,607)
      write(*,608)
      write(*,609)
      write(*,610)
      write(*,611)
      write(*,612)
  601 format(/,'...dbstats genereted following figures:')
  602 format('sta_ttres?.ps - ') 
  603 format('postscript files with travel time station residuals')
  604 format('sta_magres?.ps - ')
  605 format('postscript files with magnitude station residuals')
  606 format(/,'...dbstats genereted following files:')
  607 format('dbstats.ttres - ')
  608 format('ASCII file with travel time station residuals:')
  609 format('    sta_name ave_ttres min_ttres max_ttres stdev Nobs')
  610 format('dbstats.magres - ')
  611 format('ASCII file with travel time station residuals:')
  612 format('    sta_name ave_magres min_magres max_magres stdev Nobs')

  100 format('sta == "',a3,'"')
  101 format('sta == "',a4,'"')
  102 format('Station travel time residuals for ',a36)
  103 format('Station magnitude residuals for ',a36)
  105 format(a4,'(',i3,')')
  200 format('orid == ',i8)
  300 format(a10,f10.5,f10.5,f10.5,f10.5,i8)
  301 format('station = ',a4,' ttres = ',f10.5,' stdev = ',f10.5,
     *' npts = ',i5)
  302 format('station = ',a4,' magres = ',f10.5,' stdev = ',f10.5,
     *' npts = ',i5)
  400 format('sta == "',a3,'" && phase == "P"')
  401 format('sta == "',a4,'" && phase == "P"')
  501 format('sta_ttres',i1,'.ps')
  502 format(a10,'-',a10,'. Plot ',i1,' of ',i1,'.')
  505 format('sta_magres',i1,'.ps')
	stop
	end

c**********************************************************************
	subroutine usage()

	print*,"USAGE: dbstats dbin"
	print*,"where: dbin = input database"

	return
	end
c**********************************************************************
      subroutine getargs (dbin )

      character*(*)       dbin

      n = iargc()
      if (n .lt. 1) then
	print *,
     +  'usage: dbstats database '
	stop
      end if
      call getarg (1, dbin)

      return
      end
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
