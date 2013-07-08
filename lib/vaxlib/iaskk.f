	function iaskk(prompt,idflt)
c
c--jask prompts then reads an integer value from the terminal.
c--the default value is returned on a cr response.
c	jask   = integer response
c	prompt = prompt string
c	idflt  = default supplied on carriage return, and
c		 displayed in prompt.
c
	character prompt*(*), temp*20, temp2*20
	integer outunt
	data outunt/0/
	entry jask(prompt, idflt)
	write (temp,1002) idflt
1002	format (i20)
	do 3 i=1,20
	if (temp(i:i).ne.' ') goto 5
3	continue
5	write (outunt,1001) prompt,temp(i:20)
1001	format (1x,a,' [cr=',a,']? ',$)
	read (5,1000,err=5,end=9) temp2
1000	format (a)
	do 6 i=20, 1, -1
	if (temp2(i:i).ne.' ') goto 7
6	continue
	nch = 0
	goto 8
7	nch = i
8	if (nch.eq.0) then
		iaskk = idflt
	else
		read (temp2,1003) iaskk
1003		format (i20)
	end if
9	return
	end
