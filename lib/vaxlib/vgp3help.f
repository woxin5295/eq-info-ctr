c TEST HELPIT OR USE IT OUTSIDE OF VGP3
      character subj*5, helpfil*50, aask*17
# 3 "vgp3help.for"
      write(unit=*, fmt=*) 
     &'WELCOME TO THE VGP3 HELP PROGRAM!  USE CAPS LOCK FOR COMMANDS.'
      helpfil = 'PUB1:[LAHR.VGP3]VGP3.INF'
      subj = 'INFO '
      call helpit(subj, helpfil)
   10 subj = aask('Next TOPIC? (HELP for general help)',
     &'EXIT HELP PROGRAM',-5)
      if (subj .eq. 'EXIT') stop 
      call helpit(subj, helpfil)
      goto 10
cCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
# 13 "vgp3help.for"
      end
      subroutine helpit(subj, helpfil)
      logical more, torf
      character helprec*80, helpfil*50, subj*5, space*1
c
c----- SUBROUTINE TO PRINT PARTS OF THE FILE HELPFIL   
c      LINES STARTING WITH A LINE THAT HAS $ IN COLUMN 1 AND 
c      SUBJ IN COLUMNS 2:6 ARE PRINTED UNTIL A RECORD WITH 
c      A $ IN COLUMN 1 IS FOUND.
c
      data space / ' ' /
# 25 "vgp3help.for"
      if (subj .eq. '    ') subj = 'INFO'
c     OPEN THE FILE NAMED HELPFIL
# 26 "vgp3help.for"
      call upcase(subj)
# 28 "vgp3help.for"
      write(unit=*, fmt=*) 'OPEN THE FILE NAMED:  ', helpfil
      open(unit=41, file=helpfil, form='FORMATTED', status='OLD', err=88
     &) 
# 30 "vgp3help.for"
   20 read(unit=41, fmt=22, end=25) helprec
   22 format(a)
      if (helprec(1:6) .ne. ('$' // subj)) goto 20
      goto 28
   25 write(unit=*, fmt=*) 'COULD NOT FIND HELP ON ', subj
c
c----- PRINT OUT THIS HELP MESSAGE
# 35 "vgp3help.for"
      goto 99
# 38 "vgp3help.for"
   28 npr = 0
   30 npr = npr + 1
      write(unit=*, fmt=*) space, helprec(2:80)
      read(unit=41, fmt=22, end=99) helprec
      if (helprec(1:1) .eq. '$') goto 99
      if (npr .eq. 23) then
      more = torf('CONTINUE THIS LISTING','Y')
      if (more) goto 28
      goto 99
      end if
      goto 30
   88 write(unit=*, fmt=*) ' ERROR IN OPENING HELPFILE NAMED: ', helpfil
   99 close(unit=41) 
      return 
      end
