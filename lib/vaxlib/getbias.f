	subroutine getbias(seis, key, ndata, bias)

c -- determine dc offset by first performing a keyed sort of the data and
c    then averaging the central 50 percent of the sorted distribution.

	real*4 seis(1), bias
	integer*4 key(1), ndata

	bias = 0.

	if (ndata .lt. 1) then
	  return
	else if (ndata .eq. 1) then
	  bias = seis(1)
	  return
	else if (ndata .eq. 2) then
	  bias = 0.5*(seis(1)+seis(2))
	  return
	endif

	i = 0
	do while (i .lt. ndata)
	  i = i + 1
	  key(i) = i
	enddo

c  -- sort data

	mo=ndata
2	if(mo-15) 21,21,23
21	if(mo-1) 29,29,22
22	mo=2*(mo/4)+1
	go to 24
23	mo=2*(mo/8)+1
24	ko=ndata-mo
	jo=1
25	i=jo
26	if(seis(key(i))-seis(key(i+mo))) 28,28,27
27	kemp=key(i)
	key(i)=key(i+mo)
	key(i+mo)=kemp
	i=i-mo
	if(i-1) 28,26,26
28	jo=jo+1
	if(jo-ko) 25,25,2
29	continue

c -- determine mean of central part of distribution

	na = nint(0.5*ndata)
	if (na .lt. 3) na = 3

c    ensure that ndata and na are both odd or are both even

	if (ndata - 2*(ndata/2) .eq. 1) then		!odd
	  if (na - 2*(na/2) .eq. 0) na = na + 1

	else						!even
	  if (na - 2*(na/2) .eq. 1) na = na + 1
	endif

	i1 = (ndata-na)/2 + 1
	i2 = i1 + na - 1
	i = i1 - 1
	do while (i .lt. i1+na-1)
	  i = i + 1
	  bias = bias + seis(key(i))
	enddo

	bias = bias/na

	return
	end
