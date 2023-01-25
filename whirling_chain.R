library("deSolve")

## Here, "Time" means "distance along the x axis"


EL <- function(y,ydash){   # EL = Euler-Lagrange
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
  
  times <- seq(1, 16, by = 0.1)
  return(ode(yini, times, LVmod, pars))
}


jj <- EL(1,1)
x <- jj[,1]
y <- -jj[,2]
ydash <- jj[,3]
l <- jj[,4]

plot(x,y,asp=1)

## Now find what happens if we have a string of length L=10:
L <- 10 # string length

findx <- function(L){
  f <- function(xv){approxfun(x,l)(xv) - L}
  uniroot(f,c(10,15))$root
}

findy <- function(L){
  f <- function(yv){approxfun(y,l)(yv) - L}
  uniroot(f,c(-2,-3.9))$root
}

findg <- function(L){
  f <- function(ydv){approxfun(ydash,l)(ydv) - L}
  uniroot(f,c(1,0.07))$root
}

x_end <- findx(L)
y_end <- findy(L)
g_end <- findg(L)  # gradient

dx <- 0.3
arrows(
    x0=x_end, y0=y_end,
    x1=x_end + dx,
    y1=y_end - dx*g_end,col='red',length=0.03)




## Object returned by EL() has 4 columns: Time [x distance], y,ydash
## [y distance and gradient], and "l" which is string length
## coordinate.

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

