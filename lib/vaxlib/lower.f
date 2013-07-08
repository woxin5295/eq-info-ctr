      character record*132
c
      character fname*50, aask*50, string*100
      character*1 ans
# 4 "lower.for"
    1 write(unit=6, fmt=2) 
    2 format(37h Welcom to LOWER. This program will:/
     &46h READ A FILE WITH UP TO 132 CHARACTER RECORDS /
     &44h AND CREATE A NEW FILE WITH ONLY LOWER CASE.,/
     &53h LETTERS BLANK LINES AND TRAILING BLANKS ARE DELETED.)
      fname = aask('NAME OF INPUT FILE','IN.DAT',-50)
      call openit('read', fname, 1, ierr)
	if (ierr .ne. 0) then
	  call perror(string)
	  goto 1
	endif
# 13 "lower.for"
    3 fname = aask('NAME OF OUTPUT FILE','OUT.DAT',-50)
      call openit('write', fname, 2, ierr)
	if (ierr .ne. 0)  then
	  ie = ierrno(string)
	  call perror(string)
	  if(ie .eq. 117) then

	    ans = aask('Do you wish to delete this file (y/n)', 'y', -1)

	    if (ans .eq. 'y') then
	      call system('rm '//fname)
	      if(ierrno() .ne. 117) call perror(string)
	    endif
	  endif
	  goto 3
	endif 
# 17 "lower.for"
   10 read(unit=1, fmt='(A)', end=9000) record
      lent = lentru(record)
      if (lent .eq. 0) then
      write(unit=2, fmt='(A)') 
      else
      call locase(record)
      write(unit=2, fmt='(A)') record(1:lent)
      end if
      goto 10
 9000 stop 
      end
