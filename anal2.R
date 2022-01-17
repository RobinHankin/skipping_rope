library("deSolve")
source("usefulfuncs.R")  # defines free_cord()


lf_gravity <- list(
    F  = function(x,y){0},
    Fx = function(x,y){0},
    Fy = function(x,y){0},
    G  = function(x,y){-1},
    Gx = function(x,y){0},
    Gy = function(x,y){0})

lf_centrifugal <-  list(
    F  = function(x,y){0},
    Fx = function(x,y){0},
    Fy = function(x,y){0},
    G  = function(x,y){-y},
    Gx = function(x,y){-1},
    Gy = function(x,y){0})

lf_odd <-  list(
    F  = function(x,y){0},
    Fx = function(x,y){0},
    Fy = function(x,y){0},
    G  = function(x,y){-x},
    Gx = function(x,y){-1},
    Gy = function(x,y){0})



lf_euler <- function(Omega = 0.1){
    list(
        F  = function(x,y){-Omega*y},
        Fx = function(x,y){0},
        Fy = function(x,y){-Omega},
        G  = function(x,y){-y+Omega*x}, # centrifugal, _not_ gravity!
        Gx = function(x,y){-1+Omega},
        Gy = function(x,y){0}
    )
}

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


sol_euler_a <- data.frame( # in green
    ode(y=c(y=1,yd=0,ydd=1),       # initial conditions
        times=seq(0,1,len=100),    # values on x-axis
        func=free_cord, parms=lf_euler(Omega=0.1)))
colnames(sol_euler_a) <- c("x","y","yd","ydd")

sol_euler_b <- data.frame( # in green
    ode(y=c(y=1,yd=0,ydd=1),       # initial conditions
        times=seq(0,1,len=100),    # values on x-axis
        func=free_cord, parms=lf_euler(Omega=-0.1)))
colnames(sol_euler_b) <- c("x","y","yd","ydd")

sol_odd <- data.frame(
    ode(y=c(y=1,yd=-0.1,ydd=0.01),       # initial conditions
        times=seq(0,0.81,by=0.01),    # values on x-axis
        func=free_cord, parms=lf_odd))
colnames(sol_odd) <- c("x","y","yd","ydd")

plot(NA,xlim=c(-1,1),ylim=c(1,3),asp=1)
if(FALSE){
    points(sol_gravity[,1:2],asp=1,col='black',ylim=c(0,4))
    points(-sol_gravity[,1],sol_gravity[,2],col='black')
    points(sol_odd[,1:2],asp=1,col='blue')
    points(-sol_odd[,1],sol_odd[,2],col='blue')
}

points(sol_centrifugal[,1:2],col='red',type="l",lwd=3)
points(-sol_centrifugal[,1],sol_centrifugal[,2],type="l",col='red',lwd=3)

points( sol_euler_a[,1],sol_euler_a[,2],col='green',type="l",lwd=3)
points(-sol_euler_b[,1],sol_euler_b[,2],col='green',type="l",lwd=3)
