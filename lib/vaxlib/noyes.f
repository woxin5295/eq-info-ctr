c
c
c NOYES optionally prompts on lunout then reads a CHARACTER value from
c lunin using A20 format and a default value of idflt.
c
c MODIFIED FROM JASK BY JOHN LAHR -- MAY 1982
c
c *** Machine dependent CODE (WORKS ON VAX):
c *** $ and Q Format descriptors ***
c *** FINDS END OF PROMPT BY SEARCHING FOR AN OCTAL ZERO CHARACTER ***
c
c ival = NOYES (lunin,lunout,prompt,idflt,eof)
c
c       ival   = CHARACTER response.
c       lunin  = Input logical unit number.
c       lunout = Output logical unit number; no prompt if lunout le 0.
c       prompt = Prompt string.
c       idflt  = Default supplied for carriage return response
c                or EOF (displayed using A20 format)
c       eof    = .TRUE. if end of file detected, otherwise .FALSE.
c
c-
c
      character *4function noyes(lunin, lunout, prompt, idflt, eof)
      logical eof
      character prompt, idflt*4, octalz*1, iyes*4, ino*4
      equivalence (ioctalz, octalz)
      data iyes / 'YES' /
      data ino / 'NO' /
# 29 "noyes.for"
      ioctalz = 0
      write(unit=5, fmt=1) idflt
c
# 31 "noyes.for"
    1 format(1x,a4)
# 33 "noyes.for"
      do 10 j = 1, 512
      if (prompt(j:j) .eq. octalz) goto 20
   10 continue
   20 length = j - 1
      if (lunout .gt. 0) then
 1020 write(unit=lunout, fmt=501) prompt(1:length), idflt
  501 format(1h$,a,9h [A20;CR=,a4,3h]? )
c
# 40 "noyes.for"
      end if
# 42 "noyes.for"
  502 read(unit=lunin, fmt=503, end=9900) nch, noyes
  503 format(q,a4)
      eof = .false.
c
# 45 "noyes.for"
      if (nch .gt. 0) goto 9000
c
# 47 "noyes.for"
 8000 noyes = idflt
# 49 "noyes.for"
 9000 write(unit=5, fmt=9876) noyes, iyes, idflt
 9876 format(1x,3a5)
      if ((noyes .eq. iyes) .or. (noyes .eq. ino)) return 
      write(unit=lunout, fmt=9010) 
 9010 format(27h RESPONSE MUST BE YES OR NO)
      write(unit=lunout, fmt=501) prompt(1:length), idflt
c
# 55 "noyes.for"
      goto 502
# 57 "noyes.for"
 9900 eof = .true.
      goto 8000
      end
