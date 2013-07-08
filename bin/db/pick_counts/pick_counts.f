      program pick_counts
c************************************************************************
c     March, 2010. N.Ruppert
c     Program counts number of picks for each station
c     in an input database. It needs arrival table.
c     Output:
c     dbpickreport.dat - ASCII file station picks:
c                 sta_name Npicks
c************************************************************************

      include "db.i"
      parameter (MAXSTA = 1000)

      integer argc
      character argv

      character new_sta_name*9
      character dbin*256
      character station*4, expr*80, title*36, plot_fig_name*20 
      character station_name*4(1000)
      character*10 time_start_str, time_end_str
      character*7 yearday_start, yearday_end
      real*8 slon, slat
      real*8 time_start, time_end
      real temp_end, temp_start, jj, min, max, corr_tmp
      integer dbsrt(4), dbs(4), dbsm(4), dbl(4), db(4), dbout(4), 
     *dbtemp(4), dbo(4), dba(4)
      integer ttrescnt, num_picks(1000)
      integer orid, lincnt
      integer n, nsm, nrows, nsta, nsta_plot, nres, nor
      integer stacnt 
      integer jtime_start, jtime_end
      integer fp, ep, mp, tp
      integer rc,i, keys, n_tt_fig, n_mag_fig, rf

c     Parse command line 
c     get mandatory command line arguments 
      call getargs (dbin)

c     Open database 
 
      rc = dbopen(dbin, "r+", db)
      if (rc .lt. 0) then
        call die("dbstats: Unable to open database.")
      endif

      lincnt=1
      stacnt=1

c     find time span of the input database

      call dblookup (dbl, db, "", "arrival", "", "")
      call dbquery(dbl,dbRECORD_COUNT, nor)
      print*,'Number of arivals in arrival table = ', nor
      if (nor .lt. 1) then
        print*, 'dbstats: No arrivals in arrival table'
      else
        expr = "time"
        call strtbl(keys, expr, 0)
        call dbsort(dbsrt, dbl, keys, 0, "")
        dbsrt(4) = 0
        idb = dbgetv(dbsrt,0,"time",time_start,0)
        dbsrt(4) = nor-1
        idb = dbgetv(dbsrt,0,"time",time_end,0)
          call strdate(time_start_str,time_start)
c          call yearday(jtime_start,time_start)
         yearday_start(1:4) = time_start_str(7:10)
         yearday_start(5:7) = '001'
        print*,'time_start_str = ', time_start_str
        print*,'yearday_start = ', yearday_start
c          call yearday(jtime_end,time_end)
          call strdate(time_end_str,time_end)
         yearday_end(1:4) = time_end_str(7:10)
         yearday_end(5:7) = '366'
        print*,'time_end_str = ', time_end_str
        print*,'yearday_end = ', yearday_end
      endif

c     open site table and look for matches in stamag table

      call dblookup (dbs, db, "", "site", "", "")
      write(expr,404)
  404 format('(lat>50&&lat<72&&lon<-130)||(lat>50&&lat<72&&lon>170)')
      call dbsubset (dbs, dbs, expr, 0)
      call dbquery(dbs,dbRECORD_COUNT, nsta)
      print*,'Nsta after region subset =',nsta
      write(expr,405) yearday_end,yearday_end,yearday_start
  405 format('(ondate<',a7,'&&offdate==-1)||(ondate<',a7,'&&offdate>',a7
     &,')')
      call dbsubset (dbs, dbs, expr, 0)
      call dbquery(dbs,dbRECORD_COUNT, nsta)
      print*,'Nsta after time subset =',nsta
      if (nsta .lt. 1) then
        call die ("dbstats: No stations to process in site table")
      endif
      print*,'Nsta in site database to process = ',nsta

c     open output files
      fp=1
      open(unit=fp,file="pickcounts.out")

c     loop through stations and count number of picks
      print*
      print*,'Count number of picks for each station...'

c     start loop for each station
      do i = 0, nsta-1
      dbs(4) = i

c     get station name and coordinates
      idb=dbgetv(dbs,0,"site.lat",slat,"site.lon",slon,"sta",station,0)
      
      call dblookup(dbsm, db, "", "arrival", "", "")

c     subset arrival table for station
        if(station(4:4) .eq. " ") then
          write(expr,100) station
  100 format('sta == "',a3,'"')
  	else
          write(expr,101) station
  101 format('sta == "',a4,'"')
  	endif
  	
        call dbsubset (dbsm,dbsm, expr, 0)
        call dbquery (dbsm, dbRECORD_COUNT, nsm)
        
	  num_picks(i+1)=nsm
	  station_name(i+1)=station
	  write(*,302) station,nsm
  302 format('station = ',a4,' npts = ',i5)
	        
      rf = dbfree (dbsm)
      
      enddo
c     end do loop for stations

 
      print*,'Number of stations for pick count plot = ',
     *nsta
     
c     sort stations according to pick counts
      call sort_on_first_vector(nsta,num_picks,station_name)
      
      do i = 1, nsta
      	write(fp,300) station_name(i),num_picks(i)
  300 format(a10,i8)
      end do
      
      close(fp)
            
      write(*,606)
  606 format(/,'...dbpickreport genereted following files:')
      write(*,607)
  607 format(' pickcounts.out - ')
      write(*,608)
  608 format('ASCII file with station pick counts:')
      write(*,609)
  609 format('    sta_name Npicks')

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
