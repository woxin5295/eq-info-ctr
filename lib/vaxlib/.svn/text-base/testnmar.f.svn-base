      character string*50
      character aask*50
      dimension nbeg(100)
# 4 "testnmar.for"
      write(unit=*, fmt=*) 'Enter END to stop'
   10 string = aask('Enter string',' ',-50)
      call nmarg(string, narg, nbeg)
      write(unit=*, fmt=*) 'NARG = ', narg
      write(unit=*, fmt=*) (nbeg(i),i = 1, narg)
      if (string(1:3) .eq. 'END') stop 
      goto 10
      end
