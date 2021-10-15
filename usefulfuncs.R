library("deSolve")

`vdpol` <- function (t, Z, mu) {
    omega <- mu[1]
    Omega <- mu[2]
    y <- Z[1]
    yd <- Z[2]
    ydd <- Z[3]
    x <- t
    thing <-  (Omega*x + Omega*y*yd               - omega^2*y )
    thingd <- (Omega   + Omega*yd^2 + Omega*y*ydd - omega^2*yd)

    num <- (
        +Omega*y
        +ydd^(-2)*yd*thing/(1+yd^2)
        +ydd^(-2)*thingd
    )

    den <- 2*ydd^(-3)*thing

    list(c(
        yd=Z[2],ydd=-Z[3],
        yddd = num/den,
        sd = sqrt(1+yd^2)
    ))
}

`free_cord` <- function (t, Z, mu) {
    
    y    <- Z[1] 
    yd   <- Z[2]
    ydd  <- Z[3]
    x <- t

    F      <- mu[[1]](x,y)
    Fx     <- mu[[2]](x,y)
    Fy     <- mu[[3]](x,y)

    G      <- mu[[4]](x,y)
    Gx     <- mu[[5]](x,y)
    Gy     <- mu[[6]](x,y)


    return(list(c(
        yd   = Z[2],
        ydd  = Z[3],
        yddd = yd*ydd^2/(1+yd^2) + ydd*(2*ydd*F + yd*Fx + yd^2*Fy - Gx -yd*Gy)/(yd*F-G)
    )))
}

M <- function(out){cbind(out[,1],out[,2])}
rev <- function(x){cbind(-x[,1],x[,2])}
    
circ <- function(x0,y0,r,...){
    th <- seq(from=0,to=2*pi,len=100)
    points(x0+cos(th)*r,y0+sin(th)*r,...)
}


`badness` <- function(start=c(y1, y2, y3)){
    yini <- c(start,s=0)  # recall that 's' is arclength
    right <- ode(yini,func=vdpol,times=seq(0,x_root,by=0.01), parms=c(1,+Omega))
    yini[2] <- -yini[2]   # change the slope at x=0 as we are going backwards
    left <- ode(yini,func=vdpol,times=seq(0,x_root,by=0.01), parms=c(1,-Omega))

    right <- data.frame(right)
    left <-  data.frame(left)

    mismatch_left  <- left[nrow(left),2]^2
    mismatch_right <- right[nrow(right),2]^2

    mismatch_arc <- (2*s-(left[nrow(left),5]+right[nrow(right),5]))^2

    mismatches <- c(mismatch_left, mismatch_right, mismatch_arc)
    print(mismatches)
    return(sum(mismatches^2))
}

