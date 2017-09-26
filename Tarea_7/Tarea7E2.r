library(parallel)
library(lattice) # lo mismo aplica con este paquete
library(reshape2) # recuerda instalar paquetes antes de intentar su uso
g <- function(x, y) { 
    return((((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y) ) )
}

localsearch <- function(nr){
    #x <- runif(2, low, high)
    #y <- runif(1, low, high)
    ti <- Temp
    cpos <- runif(2, low, high)
    bestpos <- cpos
    bestval <- -g(cpos[1], cpos[2])
    tray = c(nr, 0, cpos,bestval, 0)
    tb= 0
    for (tiempo in 1:tmax) {
        d <- runif(1, 0, step)
        op = rbind(c(max(cpos[1] - d, low), cpos[2]),
                   c( min(cpos[1] + d, high), cpos[2]),
                   c(cpos[1], max(cpos[2] - d, low)),
                   c(cpos[1], min(cpos[2] + d,high))
                   ,c(min(cpos[1] + d, high), min(cpos[2] + d, high))
                   ,c(max(cpos[1] - d, low), min(cpos[2] + d, high))
                   ,c(max(cpos[1] - d, low),max(cpos[2] - d, low))
                   ,c(min(cpos[1] + d, high),max(cpos[2] - d, low))
                   )
        posibles = -g(op[,1], op[,2])
        #npos = which( posibles == min(posibles) )
        npos = sample(1:nrow(op), 1)
        nuevo = posibles[npos]
        delta = bestval - nuevo
        if (delta > 0)
        { # minimizamos
            bestpos <- op[npos,]
            cpos <- bestpos
            bestval <- nuevo
            tb = tiempo
        }
        else
        {            
            if(runif(1) < exp(delta/ti))
            {
                ti = ti * epsi
                cpos <- op[npos,]
            }
        }        
        tray <- c(tray, c(nr , tiempo , cpos,  bestval,tb) )
    }
    return (tray)
}


rep = 10
low <- -6
high <- 5
tmax <- 200
step <- 0.3
Temp = 10000
epsi = 1 - 0.1
#print(Temp)
#trayectoria <<- data.frame(rep, sec, xr, yr , zr)
v <- seq(low, high, abs(high-low)/50)
w <-  v
#z <- outer(v, w, g)
z= outer(v,w,function(q,r){-g(q,r)})
dimnames(z) <- list(v, w)
d <- melt(z)
names(d) <- c("v", "w", "z")
nombr = c("rep","paso","x","y","z","tb")
cluster = makeCluster(detectCores(logical=FALSE))
clusterExport(cluster, "low")
clusterExport(cluster, "high")
clusterExport(cluster, "tmax")
clusterExport(cluster, "step")
clusterExport(cluster, "g")
clusterExport(cluster, "Temp")
clusterExport(cluster, "epsi")
rs = data.frame(t(matrix(parSapply(cluster, 1:rep, localsearch), nrow=length(nombr))))
#rs = parSapply(cluster, 1:rep, localsearch)
colnames(rs) = nombr

lp = rs[ rs$paso== 0,]
#bs = rs[bp,]
bpp = rs[rs$z==min(rs$z),]$rep
bp = rs[rs$rep== bpp,]
png("p7_Tarea_1_E2.png", width=1000, height=1000)
levelplot(z~ v * w , data = d)
trellis.focus("panel", 1, 1, highlight=FALSE)

#parSapply(cluster,1:rep, function(re){llines(x=rs[rs$rep == re,]$x, y = rs[rs$rep == re,]$y, pch=16, col=2)})

lpoints(x=lp$x, y = lp$y, pch=17, col=8)
for(re in 1:rep)
{
    ax = rs[rs$rep == re,]
    mb = max(ax$tb)
    llines(x=ax$x, y = ax$y, pch=16, col=2)
    lpoints(x=ax[ax$paso==mb,]$x, y = ax[ax$paso==mb,]$y, pch=16, col=3)
}
llines(x=bp$x, y = bp$y, col=4)
trellis.unfocus()
graphics.off()
stopCluster(cluster)
