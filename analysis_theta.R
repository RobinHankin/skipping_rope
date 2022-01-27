library("deSolve")
source("usefulfuncs.R")  # defines free_cord()


lf_euler <- function(Omega = 1){
    list(
        F  = function(x,y){-Omega*y},
        Fx = function(x,y){0},
        Fy = function(x,y){-Omega},
        G  = function(x,y){Omega*x}, 
        Gx = function(x,y){Omega},
        Gy = function(x,y){0}
    )
}

lf_gravity <- function(g=1){
    list(
        F  = function(x,y){0},
        Fx = function(x,y){0},
        Fy = function(x,y){0},
        G  = function(x,y){-g}, 
        Gx = function(x,y){0},
        Gy = function(x,y){0}
    )
}

lf_centrifugal <- function(omega=1){
    list(
        F  = function(x,y){0},
        Fx = function(x,y){0},
        Fy = function(x,y){0},
        G  = function(x,y){-omega*y}, 
        Gx = function(x,y){0},
        Gy = function(x,y){-omega}
    )
}

lf_2 <- function(g,Omega){  # both Euler force and gravity
    list(
        F  = function(x,y){-Omega*y},
        Fx = function(x,y){0},
        Fy = function(x,y){-Omega},
        G  = function(x,y){Omega*x-g}, 
        Gx = function(x,y){Omega},
        Gy = function(x,y){0}
    )
}

sol <- data.frame(
    ode(y=c(theta0=pi*1.4,    # initial angle
            thetadash0=-1.8,   # initial curvature(?)
            x0=0.9,             # initial x coord
            y0=-0.5          # initial y coord
            ),
        times=seq(0,0.6,len=50),    # times -> arc length s
        func=free_cord_theta, parms=lf_euler(Omega=1)))
colnames(sol) <- c("s","theta","thetadash","x","y")
plot(sol[,4:5],asp=1,pch=16,xlim=c(0,1),ylim=c(-1,0))

x <- seq(from=0,to=1,len=30)
y <- seq(from=-1,to=0,len=30)
jj <- outer(x,1i*y,"+")
u <- Im(jj)/10
v <- -Re(jj)/10
quiver(x,y,u,v,scale=0.1,add=T)
           
