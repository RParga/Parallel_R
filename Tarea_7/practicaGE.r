g <- function(x, y) { 
    return((((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100) )
}

localsearch <- function(n){
    for (tiempo in 1:tmax) {
        d <- runif(1, 0, step)
        op = rbind(c(x - d, y), c( x + d, y), c(x, y - d), c(x, y + d))
        posibles = -g(op[,1], op[,2])
        npos = which( posibles == max(posibles) )
        nuevo = max(posibles) 
        if (nuevo > bestval) { # minimizamos
            bestpos <- op[npos,]
            bestval <- nuevo
        }
        trayectoria <<- c(trayectoria, -bestval)
    }
    return (c(bestpos, bestval))
}

low <- -6
high <- 5
tmax <- 100
x <- runif(1, low, high)
y <- runif(1, low, high)
step <- 0.3
bestpos <- c(x, y)
bestval <- -g(x, y)
trayectoria <<- c(-bestval)

localsearch(1)

png("prueba7.png", width=1000, height=300)
plot(0:tmax, trayectoria, type="l")
abline(h=-bestval, col="red")
graphics.off()
