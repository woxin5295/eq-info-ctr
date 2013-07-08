ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine helpit(sub, helpfil)
      logical more, torf
      character helprec*80, helpfil*(*), sub*(*), space*1
      character subj*5, dnstrng*6
      data space / ' ' /
      subj = sub
c
c----- subroutine to print parts of the file helpfil   
c      lines starting with a line that has $ in column 1 and 
c      subj in columns 2:6 are printed until a record with 
c      a $ in column 1 is found.
c
      if (subj .eq. '     ') subj = 'info'
c     open the file named helpfil
      write(unit=*, fmt=*) 'Open the file named:  ', helpfil
      call openit('read', helpfil, 41, ierr)
      if(ierr .ne. 0) goto 88
   20 read(unit=41, fmt=22, end=25) helprec
   22 format(a)
      if (dnstrng(helprec(1:6)) .ne. ('$' // dnstrng(subj))) 
     * goto 20
      npr = 0
      write(unit=*, fmt=*) space, helprec(2:80)
      goto 30
   25 write(unit=*, fmt=*) 'could not find help on ', subj
c
c----- print out this help message
      goto 99
   30 npr = npr + 1
      read(unit=41, fmt=22, end=99) helprec
      if (helprec(1:1) .eq. '$') goto 99
      if (npr .eq. 23) then
        more = torf('continue this listing',.true.)
        if (.not. more) goto 99
        npr = 0
        write(unit=*, fmt=*) helprec(1:80)
        goto 30
      end if
      write(unit=*, fmt=*) helprec(1:80)
      goto 30
   88 write(unit=*, fmt=*) ' error in opening helpfile named: ', helpfil
   99 close(unit=41) 
      return 
      end
