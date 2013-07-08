      character aask*20, one*20, two*20, askc*20
# 2 "testaask.for"
      write(unit=*, fmt=*) 'LEFT JUSTIFY'
      one = aask('INPUT ONE','input',-10)
      write(unit=*, fmt=*) '-', one, '-'
      write(unit=*, fmt=*) 'RIGHT JUSTIFY'
      one = aask('INPUT ONE','input',10)
      write(unit=*, fmt=*) '-', one, '-'
      write(unit=*, fmt=*) 'LEFT JUSTIFY'
      one = aask('INPUT ONE','input',-30)
      write(unit=*, fmt=*) '-', one, '-'
      write(unit=*, fmt=*) 'RIGHT JUSTIFY'
      one = aask('INPUT ONE','input',30)
      write(unit=*, fmt=*) '-', one, '-'
      write(unit=*, fmt=*) 'LEFT JUSTIFY'
      one = askc('INPUT ONE','input',-10)
      write(unit=*, fmt=*) '-', one, '-'
      write(unit=*, fmt=*) 'RIGHT JUSTIFY'
      one = askc('INPUT ONE','input',10)
      write(unit=*, fmt=*) '-', one, '-'
      write(unit=*, fmt=*) 'LEFT JUSTIFY'
      one = askc('INPUT ONE','input',-30)
      write(unit=*, fmt=*) '-', one, '-'
      write(unit=*, fmt=*) 'RIGHT JUSTIFY'
      one = askc('INPUT ONE','input',30)
      write(unit=*, fmt=*) '-', one, '-'
      stop 
      end
