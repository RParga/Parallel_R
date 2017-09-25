library(parallel)

library(lattice) # lo mismo aplica con este paquete
library(reshape2) # recuerda instalar paquetes antes de intentar su uso
g <- function(x, y) { 
    return((((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y) ) )
}

localsearch <- function(nr){
    x <- runif(1, low, high)
    y <- runif(1, low, high)
    bestpos <- c(x, y)
    bestval <- -g(x, y)
    tray = c(nr, 0, x,y,bestval)
    for (tiempo in 1:tmax) {
        d <- runif(1, 0, step)
        op = rbind(c(max(bestpos[1] - d, low), bestpos[2]), c( min(bestpos[1] + d, high), bestpos[2]), c(bestpos[1], max(bestpos[2] - d, low)), c(bestpos[1], min(bestpos[2] + d,high)))
        posibles = -g(op[,1], op[,2])
        npos = which( posibles == min(posibles) )
        nuevo = min(posibles) 
        if (nuevo < bestval) { # minimizamos
            bestpos <- op[npos,]
            bestval <- nuevo
        }
        tray <- c(tray, c(nr,tiempo, bestpos, bestval))
    }
    return (tray)
}


rep = 10
low <- -6
high <- 5
tmax <- 100
step <- 0.3
#trayectoria <<- data.frame(rep, sec, xr, yr , zr)
v <- seq(low, high, abs(high-low)/50)
w <-  v
#z <- outer(v, w, g)
z= outer(v,w,function(q,r){-g(q,r)})
dimnames(z) <- list(v, w)
d <- melt(z)
names(d) <- c("v", "w", "z")
nombr = c("rep","paso","x","y","z")
cluster = makeCluster(detectCores(logical=FALSE))
clusterExport(cluster, "low")
clusterExport(cluster, "high")
clusterExport(cluster, "tmax")
clusterExport(cluster, "step")
clusterExport(cluster, "g")
rs = data.frame(t(matrix(parSapply(cluster, 1:rep, localsearch), nrow=length(nombr))))
#rs = data.frame(t(matrix(parSapply(cluster, 1:rep, localsearch))))
colnames(rs) = nombr

lp = rs[ rs$paso== tmax,]
#bs = rs[bp,]
bpp = rs[rs$z==min(rs$z),]$rep
bp = rs[rs$rep== bpp,]
png("p7_Tarea_1.png", width=1000, height=1000)
levelplot(z~ v * w , data = d)
trellis.focus("panel", 1, 1, highlight=FALSE)

#parSapply(cluster,1:rep, function(re){llines(x=rs[rs$rep == re,]$x, y = rs[rs$rep == re,]$y, pch=16, col=2)})

for(re in 1:rep)
{
    llines(x=rs[rs$rep == re,]$x, y = rs[rs$rep == re,]$y, pch=16, col=2)
}
llines(x=bp$x, y = bp$y, pch=16, col=4)
lpoints(x=lp$x, y = lp$y, pch=16, col=3)
trellis.unfocus()
graphics.off()

