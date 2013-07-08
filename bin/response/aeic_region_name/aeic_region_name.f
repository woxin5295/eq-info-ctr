      program aeic_region_name

      character*100 regnam
      character*20 latstring, lonstring

      nargs = iargc()

      if( nargs .ne. 2 ) then
         write(*,*) 'Usage: aeic_region_name lat lon'
         go to 99
      else
	 call getarg( 1, latstring )
	 call getarg( 2, lonstring )
	 read(latstring,*) elat
	 read(lonstring,*) elon
      endif

      if( elat .lt. 48 .or. elat .gt. 75 ) go to 99
      if( elon .gt. 0. .and. elon .lt. 170 ) go to 99
      if( elon .lt. 0. .and. elon .gt. -130 ) go to 99

C I'm not asking and I don't want to know ('colatitude' defined from South pole):

      colat = elat + 90.

      if( elon .lt. 0 ) elon = 360. + elon

      call aeic_reg( colat, elon, regnam )
      write(*,*) regnam
      stop
99    end
