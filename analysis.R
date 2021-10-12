library("deSolve")
Omega <- -0.15
source("usefulfuncs.R")

## This solves the special case of a centrifugal force field plus
## Euler force field.  The general case is solved in anal2.R


## to go from the "no turn" case (black) to the turning case (red), we
## need to match three things: (1) black line to intersect red line at
## x~=-0.8,y=0; (2) black line to intersect red lne at x~=+0.8,y=0;
## (3) arc lengths to match.


## In the following, the third order ODE is solved with three
## independent variables, y,yd,ydd [for y,dydt, and d2y/dx2] but also
## arc length s.  We know that ds/dx = sqrt(1+yd^2) and that is the
## fourth variable in the numerics.


## following we calculate skipping rope for turning and no turning.
## plot the no-turning case, do not plot the turning case until after
## we have found the optimal parameters

## First find where the no-turn case intersects y=0 (hands):

jj <- c(0,seq(from=0.73,to=0.74,len=1000))
o <- ode(c(y1=-1, y2=+0  , y3=-4.0,s=0),func=vdpol,times=jj, parms=c(1, 0.0))
colnames(o) <- c("x","y","ydash","ydashdash","arclength")
wanted <- sum(o[,2]<0)
just <- data.frame(o[(wanted-3):(wanted+3),])
fy <- approxfun(just$x,just$y)
x_root <- uniroot(fy,c(min(just$x),max(just$x)))$root



## Can also find arclength from (-1,0) to this point:
fs <- approxfun(just$x,just$arclength)
s <- fs(x_root)


## So a "good" turning solution matches the no-turn case in terms of:
## f(x_root)=0,f(-xroot)=0, and total arclength = x.  This is three
## requirements and we have three degrees of freedom corresponding to
## the three constants of integration in our third order ODE.



op_right <- optim(c(-1,-0.1,-1.4),badness)$par
op_left <- op_right
op_left[2] <- -op_left[2]

tt <-seq(0,x_root,by=0.01)
right <- data.frame(ode(c(op_right,s=0),func=vdpol,times=tt, parms=c(1,+Omega)))
left  <- data.frame(ode(c(op_left ,s=0),func=vdpol,times=tt, parms=c(1,-Omega)))

no_turn_right_plot <- ode(c(y1=-1, y2=+0, y3=-4.0, s=0),func=vdpol,times=tt, parms=c(1,0))
no_turn_left_plot  <- ode(c(y1=-1, y2=-0, y3=-4.0, s=0),func=vdpol,times=tt, parms=c(1,0))


## Plot commands start
plot(NA,asp=1,xlim=c(-0.1,0.1),ylim=c(-1.1,-0.9))

abline(v=0)
abline(h=c(0,-1))
abline(v=c(x_root,-x_root))
for(r in seq(from=0.2,to=1,by=0.2)){circ(0,0,r,type='l',lwd=0.1,col='gray')}

points(M(no_turn_right_plot),type='l',lwd=3)
points(rev(M(no_turn_left_plot)),type="l",lwd=3)

points(-left$time, left$V2,type="l",col="red",lwd=2)
points(right$time,right$V2,type="l",col="red",lwd=2)
