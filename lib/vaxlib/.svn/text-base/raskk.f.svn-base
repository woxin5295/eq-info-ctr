	function raskk (prompt,dflt)
c
c--askr prompts then reads a real value from the terminal.
c--the default value is returned on a cr response.
c	askr   = real response
c	prompt = prompt string
c	dflt   = default supplied on carriage return and
c		 displayed in prompt.
c
	character prompt*(*), temp*20, temp2*20
	integer outunt
	data outunt/0/
	entry askr(prompt,dflt)
	write (temp,1000) dflt
1000	format (g20.5)
	do 2 i=1,20
	  if (temp(i:i).ne.' ') goto 3
2	continue
3	do 4 j=20,1,-1
	  if (temp(j:j).ne.' ')goto 5
4	continue
5	write (outunt,1001) prompt, temp(i:j)
1001	format (1x,a' [cr='a']? '$)
	read (5,1002,err=5,end=9) temp2
1002	format (a)
	do 6 i=20, 1, -1
	  if (temp2(i:i).ne.' ') goto 7
6	continue
	nch = 0
	goto 8
7	nch = i
8	if (nch.eq.0) then
	  raskk = dflt
	else
	  read (temp2,1003,err=5) raskk
1003	  format (f20.0)
	end if
9	return
	end
