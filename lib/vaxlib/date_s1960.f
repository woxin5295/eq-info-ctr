      	integer function date_s1960(jy, jm, jd)
c---- 	calculates the number of days elapsed since january 1, 1960
c j.c. lahr
      	dimension ms(12)

      	data ms/0,31,59,90,120,151,181,212,243,273,304,334/

      	date_s1960 = 
     *   365*(jy-1960) + jy/4 - 475 + ms(jm) + jd - jy/100 + jy/400
      	if(jy - (jy/4)*4 .ne. 0) return
      	if(jy - (jy/400)*400 .eq. 0) go to 10
      	if(jy - (jy/100)*100 .eq. 0) return
10 	if(jm .ge. 3) return
      	date_s1960 = date_s1960 - 1
      	return
      	end
