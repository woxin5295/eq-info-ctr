      subroutine gettab (unam, upen, uplt, usiz, uscal, usym)
      character*80      line      ! line of input file
      real              unam(16)  ! 1 or 2 for do or don't plot station names
      real              upen(16)  ! symbol pen size
      real              uplt(16)  ! for wt > 0, sets largest wt to exclude,
c                                   for wt = 0, 1 or 2 for plot or no-plot
      real              uscal(8)  ! 1 or 2 for do or don't scale by weight
      real              usiz(16)  ! symbol size (inches)
      real              usym(16)  ! symbol type 
c
c skip over first 7 lines 
c
      do 30 i = 1, 7
        read (8, 20, end = 90) line
20      format (a)
        print *, line
30    continue
c
c read data for weight > 0.
c
      nvar = 6
      do 40 i = 1, 8
        read (8, 20, end = 90) line
        print *, line
        read (line(16:80), *, err=80) uplt(i), usym(i), upen(i), 
     &        usiz(i), uscal(i), unam(i)
40    continue
c
c skip over next 7 lines 
c
      do 50 i = 1, 7
        read (8, 20, end = 90) line
        print *, line
50    continue
c
c read data for weight = 0.
c 
      nvar = 5
      do 60 i = 9, 16
        read (8, 20, end = 90) line
        print *, line
        read (line(16:80), *, err=80) uplt(i), usym(i), upen(i), 
     &        usiz(i),  unam(i)
60    continue
      return
80    print *, 'could not decode this line from the symbol table:'
      print *, line
      print *, 'it should have included ', nvar, ' variables,'
      print *, ' beginning in column 16 to be read with free format.'
      print *, '******** stop ***********'
      stop
90    print *, 'unexpected end of symbol table file.'
      print *, '******** stop ***********'
      stop
      end              
