c     real              amaxx     ! vertical page size
c     real              amaxy     ! horizontal page size
c     real              charht    ! title character height
c     character*132     line      ! line of input file
c     integer           nepsph    ! pen size for spheres
c     integer           neptit    ! pen size for title
c     integer           ncol      ! number of columns of eqs
c     integer           nrow      ! number of rows of balls 
c     character*1       pc        ! p or blank
c     real              psymb     ! symbol code for p
c     real              rmax1     ! size of beach balls
c     real              sizbst    ! size for alternate p&t axes that are
c                                   equivalent to the best
c     real              sizrst    ! size for alternate p&t axes for rest
c     real              spacep    ! space between pairs
c     real              spacenp   ! space between non-pairs
c     real              spacev    ! vertical space
c     real              tsymb     ! symbol code for t
c     real              pctcht    ! p and t character heights
c     integer           neppctc   ! pen for p and t and alternates
c     character*1       tc        ! t or blank
c     real              unam(16)  ! 1 or 2 for do or don't plot station names
c     real              upen(16)  ! symbol pen size
c     real              uplt(16)  ! for wt > 0, sets largest wt to exclude,
c                                   for wt = 0, 1 or 2 for plot or no-plot
c     real              uscal(8)  ! 1 or 2 for do or don't scale by weight
c     real              usiz(16)  ! symbol size (inches)
c     real              usym(16)  ! symbol type 
c
c     call openit('read', 'pageone.ctl', 8, ierr)
c     call gettab2(amaxx, amaxy, rmax1, nepsph, ncol, nrow, 
c    *       spacep, spacenp, spacev, charht, neptit,
c    *  psymb, tsymb, sizbst, sizrst, pc, tc, pctcht, neppctc,
c    *  unam, upen, uplt, usiz, uscal, usym)
c     print *, psymb, tsymb, sizbst, sizrst, pc, tc, pctcht, neppctc
c     stop
c     end
      subroutine gettab2(amaxx, amaxy, rmax1, nepsph, ncol, nrow, 
     *       spacep, spacenp, spacev, charht, neptit,
     *  psymb, tsymb, sizbst, sizrst, pc, tc, pctcht, neppctc,
     *  unam, upen, uplt, usiz, uscal, usym)
      real              amaxx     ! vertical page size
      real              amaxy     ! horizontal page size
      real              charht    ! title character height
      character*132     line      ! line of input file
      integer           nepsph    ! pen size for spheres
      integer           neptit    ! pen size for title
      integer           ncol      ! number of columns of eqs
      integer           nrow      ! number of rows of balls 
      character*1       pc        ! p or blank
      real              psymb     ! symbol code for p
      real              rmax1     ! size of beach balls
      real              sizbst    ! size for alternate p&t axes that are
c                                   equivalent to the best
      real              sizrst    ! size for alternate p&t axes for rest
      real              spacep    ! space between pairs
      real              spacenp   ! space between non-pairs
      real              spacev    ! vertical space
      real              tsymb     ! symbol code for t
      real              pctcht    ! p and t character heights
      integer           neppctc   ! pen for p and t and alternates
      character*1       tc        ! t or blank
      real              unam(16)  ! 1 or 2 for do or don't plot station names
      real              upen(16)  ! symbol pen size
      real              uplt(16)  ! for wt > 0, sets largest wt to exclude,
c                                   for wt = 0, 1 or 2 for plot or no-plot
      real              uscal(8)  ! 1 or 2 for do or don't scale by weight
      real              usiz(16)  ! symbol size (inches)
      real              usym(16)  ! symbol type 
c
30    read(8, '(a)', end = 90) line
      if(line(1:1) .eq. '!') goto 30
c record 1
      nvar = 11
      read(line, *, err = 80) amaxx, amaxy, rmax1, nepsph, ncol, nrow, 
     * spacep, spacenp, spacev, charht, neptit

32    read(8, '(a)', end = 90) line
      if(line(1:1) .eq. '!') goto 32
c record 2
      nvar = 8
      read(line, *, err = 80) psymb, tsymb, sizbst, sizrst, pc, tc,
     *pctcht, neppctc

34    read(8, '(a)', end = 90) line
      if(line(1:1) .eq. '!') goto 34
c read data for weight > 0.
c
      nvar = 6
      do 40 i = 1, 8
        read (line(16:80), *, err=80) uplt(i), usym(i), upen(i), 
     &        usiz(i), uscal(i), unam(i)
        read (8, '(a)', end = 90) line
40    continue

      if(line(1:1) .ne. '!') goto 44
42    read(8, '(a)', end = 90) line
      if(line(1:1) .eq. '!') goto 42
c read data for weight = 0.
c 
44    nvar = 5
      do 60 i = 9, 16
        read (line(16:80), *, err=80) uplt(i), usym(i), upen(i), 
     &        usiz(i),  unam(i)
        if (i .lt. 16) read (8, '(a)', end = 90) line
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
