library("deSolve")
source("usefulfuncs.R")  # defines free_cord()

tt <-seq(0,8,by=0.01)

lf <- list(
    function(x,y){1},
    function(x,y){1},
    function(x,y){0},
    function(x,y){0}
)

sol <- data.frame(
    ode(y=c(0,-4.2,13.7),  # initial conditions
        times=tt,    # values on x-axis
        func=free_cord, parms=lf)
)

