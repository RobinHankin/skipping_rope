library("deSolve")
source("usefulfuncs.R")  # defines helper functions free_cord() [used
                         # in anal2.R and anal3.R] and
                         # free_cord_theta() [used here]

lf_euler <- function(Omega = 1){
    list(
        F  = function(x,y){-Omega*y},   # F is the upward force
        Fx = function(x,y){0},
        Fy = function(x,y){-Omega},
        G  = function(x,y){Omega*x},    # G is the force to the right
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
    ode(y=c(theta0     = 0, # initial angle
            thetadash0 = 0.5,  # initial curvature(?)
            x0         = 0.1,   # initial x coord
            y0         = -0.9 # Initial y coord
            ),
        times=seq(0,1,len=10),    # times -> arc length s
        func=free_cord_theta, parms=lf_euler()))
colnames(sol) <- c("s","theta","thetadash","x","y")
sol <- tension(sol,f=lf_euler())

plot(sol[,4:5],asp=1,pch=16,cex=2,xlim=c(0,1),ylim=c(-1,0))

if(T){
x <- seq(from=0,to=1,len=30)
y <- seq(from=-1,to=0,len=30)
quiver(x,y,lf_euler(),scale=0.07,add=T)
           
}
