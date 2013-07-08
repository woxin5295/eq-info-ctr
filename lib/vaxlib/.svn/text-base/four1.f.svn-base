c  routine written by b. chouet

      subroutine four1(data,n,isign)
      dimension data(1)
      ip0=2
      ip3=ip0*n
      i3rev=1
      do 50 i3=1,ip3,ip0
      if(i3-i3rev)10,20,20
10    tempr=data(i3)
      tempi=data(i3+1)
      data(i3)=data(i3rev)
      data(i3+1)=data(i3rev+1)
      data(i3rev)=tempr
      data(i3rev+1)=tempi
20    ip1=ip3/2
30    if(i3rev-ip1)50,50,40
40    i3rev=i3rev-ip1
      ip1=ip1/2
      if(ip1-ip0)50,30,30
50    i3rev=i3rev+ip1
      ip1=ip0
60    if(ip1-ip3)70,100,100
70    ip2=ip1*2
      theta=6.283185307
      theta=theta/float(isign*ip2/ip0)
      sinth=sin(theta/2.0)
      wstpr=-2.0*sinth*sinth
      wstpi=sin(theta)
      wr=1.
      wi=0.
      do 90 i1=1,ip1,ip0
      do 80 i3=i1,ip3,ip2
      i2a=i3
      i2b=i2a+ip1
      tempr=wr*data(i2b)-wi*data(i2b+1)
      tempi=wr*data(i2b+1)+wi*data(i2b)
      data(i2b)=data(i2a)-tempr
      data(i2b+1)=data(i2a+1)-tempi
      data(i2a)=data(i2a)+tempr
80    data(i2a+1)=data(i2a+1)+tempi
      tempr=wr
      wr=wr*wstpr-wi*wstpi+wr
90    wi=wi*wstpr+tempr*wstpi+wi
      ip1=ip2
      go to 60
100   return
      end
