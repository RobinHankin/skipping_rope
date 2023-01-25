library("deSolve")

## Here, "Time" means "distance along the x axis"

## Object returned by function EL() defined below has 4 columns: Time
## [x distance], y,ydash [y distance and gradient], and "l" which is
## string length coordinate.


times <- seq(1, 16, by = 0.05)
yini  <- c(y=1,ydash=5.05,l=0)

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

  
  return(ode(yini, times, LVmod, pars))
}

## Now find what happens if we have a string of length L=10:

L <- 10 # string length

jj <- EL(1,1)
jj[,2:3] <- -jj[,2:3] # gravity is downward!
jj[,4] <-  jj[,4] - L  # so l=0 corresponds to the end of the string

x <- jj[,1]
y <- jj[,2]
ydash <- jj[,3]
l <- jj[,4]

plot(x,y,asp=1,type='l')
points(times[1],-yini[1],pch=16)


## Now find the properties of the end of the string, that is, at x_end
## where \int_{x=x_0}^{x_end}\sqrt{1+y'(x)^2}dx=L


findx <- function(L){
  f <- function(xv){approxfun(x,l)(xv)}
  uniroot(f,c(x[1],x[length(x)]))$root
}

findy <- function(L){
  f <- function(yv){approxfun(y,l)(yv)}
  uniroot(f,c(y[1],y[length(y)]))$root
}

findg <- function(L){   # 'g' for gradient
  f <- function(ydv){approxfun(ydash,l)(ydv)}
  uniroot(f,c(ydash[1],ydash[length(ydash)]))$root
}

x_end <- findx(L)
y_end <- findy(L)
g_end <- findg(L)  # gradient

if(FALSE){
print("gradient is:")
print(g_end)

print("gradient should be:")
print(0.5/y_end)
}
print("difference:")
print(g_end-0.5/y_end)


dx <- 0.7
arrows(
    x0=x_end, y0=y_end,
    x1=x_end + dx,
    y1=y_end + dx*g_end,col='red',length=0)

arrows(
    x0=x_end, y0=y_end,
    x1=x_end + dx,
    y1=y_end + dx/y_end/2,col='blue',length=0)


segments(
    x0=0,x1=1,
    y0=-yini[1])

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


