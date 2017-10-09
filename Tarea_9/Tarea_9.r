n <- 50
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
png("p9i.png")
library(lattice)
xyplot(y ~ x, group=g, data=p, auto.key=list(space="right"),
       xlab="X", ylab="Y", main="Part\u{00ed}culas generadas",
       par.settings = list(superpose.symbol = list(pch = 15, cex = 1.5,
                                                   col = colores)))
graphics.off()
eps <- 0.001
fuerza <- function(i) {
    xi <- p[i,]$x  # valor x
    yi <- p[i,]$y  # valor y
    ci <- p[i,]$c  # valor carga
    fx <- 0
    fy <- 0
    for (j in 1:n) { #Para cada carga
        cj <- p[j,]$c #Se toma el valor de la carga
        dir <- (-1)^(1 + 1 * (ci * cj < 0)) #Se calcula la direccion de atraccion o repulsion
        dx <- xi - p[j,]$x # Se saca la diferencia de distancia en x entre las 2 cargas
        dy <- yi - p[j,]$y # Se saca la diferencia de distancia en y entre las 2 cargas
        factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps) #Se hace el calculo del "factor" de avance hacia la direccion de la particula deacuerdo a la distancia que hay entre las cargas
        fx <- fx - dx * factor #la particula se mueve hacia el resultado
        fy <- fy - dy * factor #
    }
    return(c(fx, fy))
}
#suppressMessages(library(doParallel))
suppressMessages(library(parallel))
#registerDoParallel(makeCluster(detectCores() - 1))
cluster = makeCluster(detectCores(logical=FALSE))
system("rm -f p9_t*.png") # borramos anteriores en el caso que lo hayamos corrido
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
}
png(paste("p9_t", tl, ".png", sep=""))
plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
     main="Estado inicial", xlab="X", ylab="Y")
graphics.off()
for (iter in 1:tmax) {
    clusterExport(cluster, "p")    
    clusterExport(cluster, "eps")
    clusterExport(cluster, "n")
    f <- data.frame(t(matrix(parSapply(cluster, 1:n, fuerza), nrow=2)))
    colnames(f) = c("X","Y")
    #f1 <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
    delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
    p$x <- apply(matrix(p$x+delta*f$X,nrow=1),2, function(x){ return(ifelse(x>1,1,ifelse(x<0,0,x))) } )
    #p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[i,]$X, 1), 0)
    p$y <- apply(matrix(p$y+delta*f$Y,nrow=1),2, function(x){ return(ifelse(x>1,1,ifelse(x<0,0,x))) } )
    #p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[i,]$Y, 1), 0)
    tl <- paste(iter, "", sep="")
    while (nchar(tl) < digitos) {
        tl <- paste("0", tl, sep="")
    }
    png(paste("p9_t", tl, ".png", sep=""))
    plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
         main=paste("Paso", iter), xlab="X", ylab="Y")
    graphics.off()
}
stopCluster(cluster)

#library(magick)

#frames=lapply(1:t,function(w) image_read(paste("Practica9_",w,".png",sep="")))
#animation <- image_animate(image_join(frames),fps=4)
#print(animation)
#w3<-paste("P9_", replicas,".gif",sep=""  )
#image_write(animation, w3 )
#sapply(1:t,function(x) file.remove(paste("Practica7_",x,".png",sep="")))

system("convert -delay 50 -size 300x300 p9_t*.png -loop 0 p9.gif") # creamos animacion con ImageMagick
system("rm -f p9_t*.png") # borramos anteriores en el caso que lo hayamos corrido
