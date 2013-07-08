c upstrng.for    []
      character*(*) function upstrng(array)
c
c  program to change string to uppercase
c
c  array - a character variable
      character*(*) array
      integer offset
      data offset/32/
c
c  get length of array
c
      upstrng = ' '
      lenstr = len(array)
      if (lenstr .eq. 0) return
      do 10 i = 1, lenstr
        ic = ichar(array(i:i))
        if ((ic .ge. 97) .and. (ic .le. 122)) then
          upstrng(i:i) = char(ic - offset)
        else
          upstrng(i:i) = array(i:i)
        endif
   10 continue
      return
      end
c end upstrng
