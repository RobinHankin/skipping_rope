library("deSolve")
source("usefulfuncs.R")  # defines free_cord()


lf_gravity <- list(
    Fx     = function(x,y){0},
    Fy     = function(x,y){-1},
    Fxdash = function(x,y){0},
    Fydash = function(x,y){0}
)

lf_centrifugal <- list(
    Fx     = function(x,y){0},
    Fy     = function(x,y){-y},
    Fxdash = function(x,y){0},
    Fydash = function(x,y){-1}
)


sol_gravity <- data.frame(
    ode(y=c(y=1,yd=0,ydd=1),       # initial conditions
        times=seq(0,2,by=0.01),    # values on x-axis
        func=free_cord, parms=lf_gravity))
colnames(sol_gravity) <- c("x","y","yd","ydd")

sol_centrifugal <- data.frame(
    ode(y=c(y=1,yd=0,ydd=1),       # initial conditions
        times=seq(0,2,by=0.01),    # values on x-axis
        func=free_cord, parms=lf_centrifugal))
colnames(sol_centrifugal) <- c("x","y","yd","ydd")


plot(sol_gravity[,1:2],asp=1,col='black')
points(-sol_gravity[,1],sol_gravity[,2],col='black')

points(sol_centrifugal[,1],sol_centrifugal[,2],col='red')
legend("topleft",pch=1,col=c("black","red"),legend=c("gravity","centrifugal"))
