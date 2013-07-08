	logical function lask (prompt,ldflt)
c--lask prompts using the string "prompt", then reads a logical value
c--from the terminal. the default value ldflt is returned on a cr response.
c	lask= logical response.
c	prompt= prompt string.
c	ldflt= default displayed in prompt & returned on cr.
c
	character prompt*(*), temp
	logical ldflt
	integer outunt
	data outunt/0/
5	write (outunt,1001) prompt,ldflt
1001	format (1x,a' [t or f, cr='l1']? '$)
	read (5,1000,err=5,end=9) temp
1000	format (a)
	if((temp.eq.'t').or.(temp.eq.'t')) then
		lask = .true.
	else if ((temp.eq.'f').or.(temp.eq.'f')) then
		lask = .false.
	else
		lask=ldflt
	end if
9	return
	end
