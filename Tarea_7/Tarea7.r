library(parallel)
library(lattice) # lo mismo aplica con este paquete
library(reshape2) # recuerda instalar paquetes antes de intentar su uso
g <- function(x, y) { 
    return((((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y) ) )
}
bval = -g(5,5)

localsearch <- function(nr){
    x <- runif(1, low, high)
    y <- runif(1, low, high)
    bestpos <- c(x, y)
    bestval <- -g(x, y)
    tb=0
    tray = c(nr, 0, x,y,bestval,tb)
    for (tiempo in 1:tmax) {
        d <- runif(1, 0, step)
        #op = rbind(c(max(bestpos[1] - d, low), bestpos[2])
        #         , c( min(bestpos[1] + d, high), bestpos[2])
        #         , c(bestpos[1], max(bestpos[2] - d, low))
        #         , c(bestpos[1], min(bestpos[2] + d,high)))
        op = rbind(c(max(bestpos[1] - d, low), bestpos[2]),
                   c(min(bestpos[1] + d, high), bestpos[2]),
                   c(bestpos[1], max(bestpos[2] - d, low)),
                   c(bestpos[1], min(bestpos[2] + d, high))
                  ,c(min(bestpos[1] + d, high), min(bestpos[2] + d, high))
                  ,c(max(bestpos[1] - d, low), min(bestpos[2] + d, high))
                  ,c(max(bestpos[1] - d, low), max(bestpos[2] - d, low))
                  ,c(min(bestpos[1] + d, high), max(bestpos[2] - d, low))
                   )
        posibles = -g(op[,1], op[,2])
        npos = which( posibles == min(posibles) )[1]
        nuevo = min(posibles) 
        if (nuevo < bestval) { # minimizamos
            bestpos <- op[npos,]
            bestval <- nuevo
            tb = tiempo
        }
        tray <- c(tray, c(nr,tiempo, bestpos, bestval,tb))
    }
    return (tray)
}


rep = 100
low <- -6
high <- 5
tmax <- 100
step <- 0.3
#trayectoria <<- data.frame(rep, sec, xr, yr , zr)
v <- seq(low, high, abs(high-low)/500)
w <-  v
#z <- outer(v, w, g)
z= outer(v,w,function(q,r){g(q,r)})
dimnames(z) <- list(v, w)
d <- melt(z)
names(d) <- c("v", "w", "z")
nombr = c("rep","paso","x","y","z", "tb")
cluster = makeCluster(detectCores(logical=FALSE))
clusterExport(cluster, "low")
clusterExport(cluster, "high")
clusterExport(cluster, "tmax")
clusterExport(cluster, "step")
clusterExport(cluster, "g")
rs = data.frame(t(matrix(parSapply(cluster, 1:rep, localsearch), nrow=length(nombr))))
#rs = data.frame(t(matrix(parSapply(cluster, 1:rep, localsearch))))
colnames(rs) = nombr

lp = rs[rs$paso== tmax,]

#bs = rs[bp,]
bpp = min(rs[rs$z==min(rs$z),]$rep)
bp = rs[rs$rep== bpp,]
titl = paste("Gráfica de superficie Míninmo en g(",bp[bp$paso==tmax,]$x,", ", bp[bp$paso==tmax,]$y,") = ", round(bp[bp$paso==tmax,]$z,2)," Separación: ", round((bval- bp[bp$paso==tmax,]$z)/bval*100,2), "% Iteraciones ", bp[bp$paso==tmax,]$tb ,sep="")
png("p7_Tarea_1.png", width=1000, height=1000)
levelplot(z~ v * w , data = d,  xlab = list(label="Coordenada X", cex=2), ylab = list(label="Coordenada Y", cex=2), main=list(label=titl, cex=2), col.regions=rev(cm.colors(100)), scales=list(x=list(cex=2),y=list(cex=2)), colorkey=list(labels=list(cex=2)) )
trellis.focus("panel", 1, 1, highlight=FALSE)

if(rep>=10)
{
    mues = sample(1:rep, log(rep,2))
} else
{
    mues = 1:rep
}
for(re in mues)
{
    llines(x=rs[rs$rep == re,]$x, y = rs[rs$rep == re,]$y, pch=16, col=2)
    lpoints(x=rs[rs$rep == re & rs$paso == 0,]$x, y = rs[rs$rep == re & rs$paso == 0,]$y, pch=18, col=2, cex=2)
}

lpoints(x=bp[bp$paso==0,]$x, y = bp[bp$paso==0,]$y, pch=18, col=4, cex=2)
llines(x=bp$x, y = bp$y, pch=16, col=4)
lpoints(x=lp$x, y = lp$y, pch=16, col=3, cex=2)
trellis.unfocus()
graphics.off()

