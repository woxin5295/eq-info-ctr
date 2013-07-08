      character string*50
      character aask*50
# 4 "testarg.for"
   10 string = aask('Enter string',' ',-50)
      narg = numarg(string)
      write(unit=*, fmt=*) 'NARG = ', narg
      if (string(1:3) .eq. 'END') stop 
      goto 10
      end
