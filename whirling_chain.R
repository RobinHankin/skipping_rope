library("deSolve")

## Here, "Time" means "distance along the x axis"

    
LVmod <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dy        <- ydash
    dydash    <- (1-2*a*Time*ydash)*(1+ydash^2)/(a*Time^2 + y)
    dl        <- sqrt(1+ydash^2)
    return(list(c(dy,dydash,dl)))
  })
}

pars  <- c(a=1) # rotation rate
yini  <- c(y=1,ydash=1,l=0)

times <- seq(1, 10, by = 0.1)
out   <- ode(yini, times, LVmod, pars)
summary(out)


## Object 'out' has 4 columns: Time [x distance], y,ydash [y distance
## and gradient], and "l" which is string length coordinate.

## Physical problem has a known string length, say "L".  The end of
## the string is at point (x_e,y_e) ["e" for "end"], and this is to be
## found.  We know the gradient at that point is is simply
## 1/(2*a*x_e), and we also know that integral of sqrt(1+y'^2)dx =
## l(x).  We can use 'out' to find the appropriate row where l=L to
## determine (x_e,y_e).  Then we need to try different starting values
## of y and ydash to make ydash(x_e) = 1/(2ax_e).  I am not 100% sure
## how changing the start value of y [the first element of yini] can
## possibly alter the form of the solution apart from moving it up and
## down.  Actually I don't really understand this aspect of the
## catenary either.

