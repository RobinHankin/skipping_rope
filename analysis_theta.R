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

sol_gravity <- data.frame(
    ode(y=c(theta0=0,      # initial angle
            thetadash0=1,  # initial curvature(?)
            x0=0,          # initial x coord
            y0=-1          # initial y coord
            ),
        times=seq(0,0.12,len=50),    # times -> arc length s
        func=free_cord_theta, parms=lf_gravity(g=1)))
colnames(sol_gravity) <- c("s","theta","thetadash","x","y")

