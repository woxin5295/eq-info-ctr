c
c torf issues prompt at the terminal and then reads an charactr value fr
com
c the terminal using a1 format and a default value of ldflt.
c
c written by j. c. lahr and c. d. stephens
c
c *** machine dependent code (works on pdp vax): ***
c *** $ and q format descriptors ***
c
c torf = torf (prompt,ldflt)
c
c       ival   = integer response.
c       prompt = prompt string.
c       ldflt   = default supplied for carriage return response
c-
c
      logical function torf(prompt, ldflt)
      character prompt*(*), adflt*1, ans
      integer status
c
      logical ldflt
      adflt = 'n'
c
c     print *, 'first argument = ', prompt
c     print *, 'second argument = ', ldflt
      if (ldflt) adflt = 'y'

    5 if (prompt .ne. ' ') then
 1040   write(unit=6, fmt=501) prompt, adflt
  501   format(1x, a, ' [y/n; CR =', a, ']? '$)
      end if

      READ (5, '(A)', IOSTAT=status) ans
      call locase(ans)
      IF ((status.NE.0) .OR. (ans.EQ.' ')) then	 
        torf = .false.
        if (ldflt) torf = .true.
        return
      endif
c
      if ((ans .eq. 'y') .or. (ans .eq. 'n')) then 
        torf = .false.
        if (ans .eq. 'y') torf = .true.
      else
        write(unit=6, fmt=9010) ans
 9010   format(30h response must be y or n, not:,a1)
        goto 5
      endif

      return 
      end
