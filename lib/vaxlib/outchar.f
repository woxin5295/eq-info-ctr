c--send character to terminal
      i = 1	
10    i = iaskk('number to send to terminal', i)
      call outchar(i)
      if(i .eq. 0) stop
      goto 10
      end
      subroutine outchar(j)
      character a*1
      equivalence (i, a)
      i = j
      write(6, '(a)') char(j)
      return 
      end
