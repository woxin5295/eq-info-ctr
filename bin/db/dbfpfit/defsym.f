      subroutine defsym (ain, doplt, fm, name, unam, upen, 
     &                   uplt, usiz, uscal, 
     &                   uspen, ussiz, ussym, usym, wt)
c pick symbol type, size, and pen thickness based on symbol table and
c ain, fm, and wt.
      character*1       dnstrng
      real              ain       ! ray angle of incidence
      logical           doplt     ! .true. to plot this symbol
      character*1       fm        ! first motion prmk(3:3)
      character*4       name      ! station name
      real              unam(16)  ! 1 or 2 for do or don't plot station names
      real              upen(16)  ! symbol pen size
      real              uplt(16)  ! for wt > 0, sets largest wt to exclude,
c                                   for wt = 0, 1 or 2 for plot or no-plot
      real              uscal(8)  ! 1 or 2 for do or don't scale by weight
      real              usiz(16)  ! symbol size (inches)
      real              usym(16)  ! symbol type 
      real              uspen     ! symbol pen size
      real              ussiz     ! symbol size (inches)
      real              ussym     ! symbol type 
      real              wt        ! weight assigned to pick quality in program fpfit
c
c	write(95,*) '---> called defsym with:'
c	write(95,*) 'ain, doplt, fm, name, uspen, ussiz, ussym, 
c     &wt ='
c	write(95,*) ain, doplt, fm, name, uspen, ussiz, ussym, wt
c
      doplt = .true.
c
c find position in symbol table
c
      if (wt .gt. 0.) then
        if ((dnstrng(fm) .eq. 'c') .or. (dnstrng(fm) .eq. 'u')) then
          if (ain .ge. 90.) then
            itab = 1
          else
            itab = 2
          endif
        endif
        if (fm .eq. '+') then
          if (ain .ge. 90.) then
            itab = 3
          else
            itab = 4
          endif
        endif
        if (dnstrng(fm) .eq. 'd') then
          if (ain .ge. 90.) then
            itab = 5
          else
            itab = 6
          endif
        endif
        if (fm .eq. '-') then
          if (ain .ge. 90.) then
            itab = 7
          else
            itab = 8
          endif
        endif
        ussiz = usiz(itab)
        if (uscal(itab) .eq. 1) then
          ussiz = ussiz * (wt**.3)/1.5
        endif
      else if (wt .eq. 0.) then
        if ((dnstrng(fm) .eq. 'c') .or. (dnstrng(fm) .eq. 'u')) then
          if (ain .ge. 90.) then
            itab = 9
          else
            itab = 10
          endif
        endif
        if (fm .eq. '+') then
          if (ain .ge. 90.) then
            itab = 11
          else
            itab = 12
          endif
        endif
        if (dnstrng(fm) .eq. 'd') then
          if (ain .ge. 90.) then
            itab = 13
          else
            itab = 14
          endif
        endif
        if (fm .eq. '-') then
          if (ain .ge. 90.) then
            itab = 15
          else
            itab = 16
          endif
        endif
        ussiz = usiz(itab)
      endif
      ussym = usym(itab)
      uspen = upen(itab)
      if (unam(itab) .eq. 2.) name = '    '
      if (uplt(itab) .eq. 2.) doplt = .false.
c	write(95,*) '---> returned from defsym'
      return
      end
