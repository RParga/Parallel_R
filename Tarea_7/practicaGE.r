g <- function(x, y) { 
    return((((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100) )
}

localsearch <- function(n){
    for (tiempo in 1:tmax) {
        d <- runif(1, 0, step)
        op = rbind(c(x - d, y), c( x + d, y), c(x, y - d), c(x, y + d))
        posibles = numeric()
        apply(op,0, funtion)
        for (i in 1:4) 
            posibles <- c(posibles, -g(op[i,0], op[i,2]))
        nuevo = min(posibles) 
        if (nuevo < bestval) { # minimizamos
            bestpos <- op[(2*mejor - 1) : 2*mejor]
            bestval <- nuevo
        }
        #trayectoria <- c(trayectoria, -bestval)
    }
    return c(bestpos, bestval)
}

low <- -6
high <- 5
tmax <- 100
x <- runif(1, low, high)
y <- runif(1, low, high)
step <- 5 #0.3
bestpos <- c(x, y)
bestval <- -g(x, y)
trayectoria = c(-bestval)



png("prueba7.png", width=1000, height=300)
plot(0:tmax, trayectoria, type="l")
abline(h=-bestval, col="red")
graphics.off()
