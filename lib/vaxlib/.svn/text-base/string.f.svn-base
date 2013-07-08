c  ascii string manipulation

c -------------------------------------------------------------------  locase

	subroutine locase(string)

c  convert ascii string to lower case

	parameter (ioff = ichar('a') - ichar('A'))

	character*(*) string

	i = 0
	do while (i .lt. len(string))
	  i = i + 1
	  if (string(i:i) .ge. 'A' .and. string(i:i) .le. 'Z')
     *		string(i:i) = char(ichar(string(i:i)) + ioff)
	enddo

	return

	end

c -------------------------------------------------------------------  upcase

	subroutine upcase(string)

c  convert ascii string to upper case

	parameter (ioff = ichar('A') - ichar('a'))

	character*(*) string

	i = 0
	do while (i .lt. len(string))
	  i = i + 1
	  if (string(i:i) .ge. 'a' .and. string(i:i) .le. 'z')
     *		string(i:i) = char(ichar(string(i:i)) + ioff)
	enddo

	return

	end

c -------------------------------------------------------------------  right

	subroutine right(string)

c  right-justify characters in ascii string

	character*(*) string

	lens = len(string)

	ie = lens
	do while (ie .gt. 0 .and. string(ie:ie) .eq. ' ')
	  ie = ie - 1
	enddo

	if (ie .eq. 0) return

	ishift = lens - ie
	i = lens
	j = i - ishift
	do while (i .gt. lens-ie)
	  string(i:i) = string(j:j)
	  i = i - 1
	  j = j - 1
	enddo
	do while (i .gt. 0)
	  string(i:i) = ' '
	  i = i - 1
	enddo

	return

	end

c -------------------------------------------------------------------  left

	subroutine left(string)

c  left-justify characters in ascii string

	character*(*) string

	lens = len(string)

	ib = 1
	do while (ib .le. lens .and. string(ib:ib) .eq. ' ')
	  ib = ib + 1
	enddo

	if (ib .eq. 1 .or. ib .gt. lens) return

	i = 1
	j = ib
	do while (i .lt. lens-ib+2)
	  string(i:i) = string(j:j)
	  i = i + 1
	  j = j + 1
	enddo
	do while (i .le. lens)
	  string(i:i) = ' '
	  i = i + 1
	enddo

	return

	end

c -------------------------------------------------------------------  lastc

	integer function lastc(string)

c  return position of last non-blank character in an ascii string
c  a value of 0 is returned for all blanks

	character*(*) string

	lastc = len(string)

	do while (lastc .gt. 0 .and. string(lastc:lastc) .eq. ' ')
	  lastc = lastc - 1
	enddo

	return

	end

c -------------------------------------------------------------------  iargs

	integer function iargs(string)

c  return the number of arguments in an ascii string.  arguments are delimited
c  by 'white' space (blanks or tabs) or by using commas.  single (') or double
c  (") quotes can be used to delimit contingous substrings that have embedded 
c  blanks or tabs (e.g. 'this example'); to preserve a single-quote character 
c  within a substring, use a double single-quote, (e.g. 'can''t' will be parsed 
c  as can't).

c  -1 is returned if an error is detected (e.g. format, missing quote delimiter)

	character*1 tab, quote, delim
	parameter (tab = char(9))
	parameter (quote = char(39))

	character*(*) string

	iargs = 0
	lens = len(string)
	ic = 0
	iarg = 0
	delim = ' '

c  -- begin main loop
	
	do while (ic .le. lens)

	  ib = ic + 1

c -- skip initial white space before argument

	  do while (ib .le. lens .and.
     *	 	(string(ib:ib) .eq. ' '	.or. string(ib:ib) .eq. tab))
	    ib = ib + 1
	  enddo

	  if (ib .gt. lens) then
	    if (delim .eq. ',') iargs = iargs + 1
	    return
	  endif

c -- implied argument (e.g. ',,').  not the same as null (ascii 0)!

	  if (string(ib:ib) .eq. ',') then
	    ic = ib

c -- quoted string using single-quote delimiter

	  else if (string(ib:ib) .eq. quote) then

	    ib = ib + 1
	    if (ib .gt. lens) go to 90
	    ic = ib
20	    do while (ic .le. lens .and.  string(ic:ic) .ne. quote)
	      ic = ic + 1
	    enddo
	    if (ic .gt. lens) go to 90
	    if (ic .lt. lens .and. string(ic+1:ic+1) .eq. quote) then
	      ic = ic + 2
	      if (ic .gt. lens) go to 90
	      go to 20
	    endif

	    if (ic .lt. lens) then
	      ic = ic + 1
	      if (string(ic:ic) .ne. ' ' .and. 
     * string(ic:ic) .ne. tab .and.
     * string(ic:ic) .ne. ',') go to 90
	    else
	      ic = ic + 1
	    endif
	
c -- quoted string using double-quote delimiter

	  else if (string(ib:ib) .eq. '"') then

	    ib = ib + 1
	    if (ib .gt. lens) go to 90
	    ic = ib

	    do while (ic .le. lens .and.  string(ic:ic) .ne. '"')
	      ic = ic + 1
	    enddo

	    if (ic .lt. lens) then
	      ic = ic + 1
	      if (string(ic:ic) .ne. ' ' .and. 
     * string(ic:ic) .ne. tab .and.
     * string(ic:ic) .ne. ',') go to 90

	    else if (ic .gt. lens) then
	      go to 90

	    else
	      ic = ic + 1
	    endif

c -- other string

	  else

	    ic = ib + 1
	    do while (ic .le. lens .and. string(ic:ic) .ne. ',' .and.
     *		string(ic:ic) .ne. ' ' .and. string(ic:ic) .ne. tab)
	      ic = ic + 1
	    enddo

	  endif

c -- found next argument

	  iargs = iargs + 1

c -- skip white space before delimiting comma or start of next argument

	  do while (ic .le. lens .and. (string(ic:ic) .eq. ' ' .or. 
     *		string(ic:ic) .eq. tab))
	    ic = ic + 1
	  enddo

	  if (ic .gt. lens) return

	  if (string(ic:ic) .ne. ',') ic = ic - 1

	  delim = string(ic:ic)

	enddo

	return

c -- error detected

90	iargs = -1

	return

	end

c -------------------------------------------------------------------  getstr

	subroutine getstr (narg, string, substr, nchar)

c  return the narg'th argument in an ascii string.  arguments are delimited
c  by 'white' space (blanks or tabs) or by using commas.  single (') or double
c  (") quotes can be used to delimit contingous substrings that have embedded 
c  blanks or tabs (e.g. 'this example'); to preserve a single-quote character 
c  within a substring, use a double single-quote, (e.g. 'can''t' will be parsed 
c  as can't).

c  on return, nchar = -1 if an error is detected (e.g. format, missing quote
c			 delimiter, length of target argument exceeds size of
c			 substr).  
c		    =  0 if narg exceeds the number of arguments in the
c			 search string
c                   =  index of last non-blank character in the argument, unless
c			 the argument is all blank, in which case substr is
c			 all blank characters and nchar = length of substr

c  the function iargs returns the number of arguments in an ascii string

c  cds 08-04-93

	character*1 tab, quote, delim
	parameter (tab = char(9))
	parameter (quote = char(39))

	character*(*) string	! search string
	character*(*) substr	! sub-string with target argument

	substr = ' '
	nchar = -1
	if (narg .lt. 1) return

	lens = len(string)
	ic = 0
	ie = 0
	iarg = 0
	delim = ' '

c  -- begin main loop
	
	do while (iarg .lt. narg .and. ic .le. lens)

	  ib = ic + 1
	  nchar = -1

c -- skip initial white space before argument

	  do while (ib .le. lens .and.
     *	 	(string(ib:ib) .eq. ' '	.or. string(ib:ib) .eq. tab))
	    ib = ib + 1
	  enddo

	  if (ib .gt. lens) then
	    if (delim .eq. ',' .or. iarg .eq. 0) then
	      iarg = iarg + 1
	      if (iarg .eq. narg) nchar = len(substr)
	    endif
	    return
	  endif

c -- implied argument (e.g. ',,').  not the same as null (ascii 0)!

	  if (string(ib:ib) .eq. ',') then
	    ic = ib
	    nchar = len(substr)

c -- quoted string using single-quote delimiter

	  else if (string(ib:ib) .eq. quote) then

	    ib = ib + 1
	    if (ib .gt. lens) return
	    ic = ib
20	    do while (ic .le. lens .and.  string(ic:ic) .ne. quote)
	      ic = ic + 1
	    enddo
	    if (ic .gt. lens) return
	    if (ic .lt. lens .and. string(ic+1:ic+1) .eq. quote) then
	      ic = ic + 2
	      if (ic .gt. lens) return
	      go to 20
	    endif

	    ie = ic - 1

	    if (ic .lt. lens) then
	      ic = ic + 1
	      if (string(ic:ic) .ne. ' ' .and. 
     * string(ic:ic) .ne. tab .and.
     * string(ic:ic) .ne. ',') return
	    else
	      ic = ic + 1
	    endif
	
c -- quoted string using double-quote delimiter

	  else if (string(ib:ib) .eq. '"') then

	    ib = ib + 1
	    if (ib .gt. lens) return
	    ic = ib

	    do while (ic .le. lens .and.  string(ic:ic) .ne. '"')
	      ic = ic + 1
	    enddo

	    ie = ic - 1

	    if (ic .lt. lens) then
	      ic = ic + 1
	      if (string(ic:ic) .ne. ' ' .and. 
     * string(ic:ic) .ne. tab .and.
     * string(ic:ic) .ne. ',') return

	    else if (ic .gt. lens) then
	      return

	    else
	      ic = ic + 1
	    endif

c -- other string

	  else

	    ic = ib + 1
	    do while (ic .le. lens .and. string(ic:ic) .ne. ',' .and.
     *		string(ic:ic) .ne. ' ' .and. string(ic:ic) .ne. tab)
	      ic = ic + 1
	    enddo

	    ie = ic - 1

	  endif

c -- found end of argument

	  iarg = iarg + 1

c -- is this the target?

	  if (iarg .eq. narg) then

	    if (nchar .gt. -1) then
	      return
	    else
	      nchar = ie - ib + 1
	      if (nchar .gt. len(substr)) then
		nchar = -1
	      else
		substr = string(ib:ie)
	      endif
	      return
	    endif

	  endif

c -- skip white space before delimiting comma or start of next argument

	  do while (ic .le. lens .and. (string(ic:ic) .eq. ' ' .or. 
     *		string(ic:ic) .eq. tab))
	    ic = ic + 1
	  enddo

	  if (ic .gt. lens) return

	  if (string(ic:ic) .ne. ',') ic = ic - 1

	  delim = string(ic:ic)

	enddo

	end
