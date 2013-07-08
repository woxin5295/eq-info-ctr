#!/bin/sh
 
r="-"
case $1 in
# algorithm
hypoquality)    case $2 in
\&)     r=furnished ;;
\%)     r=single_network ;;
\*)    r=less_reliable ;;
\?)    r=poor_solution ;;
*)    r="-" ;;
esac ;;

# etype
atectonic) case $2 in
E)    r=ex ;;
*)    r="-" ;;
esac ;;

# dtype
depthquality)   case $2 in
N)    r=r ;;
D)    r=d ;;
G)    r=g ;;
\*)   r=F ;;
\?)    r=P ;;
*)    r=f ;;
esac ;;
 
esac
echo $r
 
