      r = 10
10    r = raskk('value', r)
      print *, 'r = ', r
      if (r .gt. 0.) goto 10
      stop
      end
