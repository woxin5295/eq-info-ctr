	subroutine gmt_ast(iyear, imon, iday, ihr)
c convert from gmt to alaska standard time
c j.c. lahr
	integer elday, date_s1960

      	elday = date_s1960(iyear, imon, iday)

	ihr = ihr - 9
	if(ihr .lt. 0) then
	  ihr = ihr + 24
	  elday = elday - 1
	endif
	call s1960_date(elday, iyear, imon, iday)
	return
	end
