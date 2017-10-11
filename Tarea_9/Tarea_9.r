library(ggplot2)
library(lattice)
suppressMessages(library(parallel))
#suppressMessages(library(doParallel))

n <- 50
tmax <- 100
mass = FALSE
impr = TRUE
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m=rnorm(n))
vv <- data.frame(m=numeric(),vx = numeric(), vy=numeric())
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

mmax <- max(p$m)
mmin <- min(p$m)
p$m <- round(10 * (p$m - mmin) / (mmax - mmin)) # masas son entre 0 y 50
p$m[p$m==0]= 1 # hacemos 1 los 0
#p$g <- round(5 * p$m) # coloreamos segun la carga a 11 niveles de -5 a 5


paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
if(impr){
    png("p9i.png")
    xyplot(y ~ x, group=g, data=p, auto.key=list(space="right"),
           xlab="X", ylab="Y", main="Part\u{00ed}culas generadas",
           par.settings = list(superpose.symbol = list(pch = 15, cex = 1.5,
                                                       col = colores)))
    graphics.off()
}
eps <- 0.001
fuerza <- function(i) {
    xi <- p[i,]$x  # valor x
    yi <- p[i,]$y  # valor y
    ci <- p[i,]$c  # valor carga
    mi <- p[i,]$m  # valor masa
    mx= max(p$m)
    fx <- 0
    fy <- 0
    for (j in 1:n) { #Para cada carga
        cj <- p[j,]$c #Se toma el valor de la carga
        dir <- (-1)^(1 + 1 * (ci * cj < 0)) #Se calcula la direccion de atraccion o repulsion
        dx <- xi - p[j,]$x # Se saca la diferencia de distancia en x entre las 2 cargas
        dy <- yi - p[j,]$y # Se saca la diferencia de distancia en y entre las 2 cargas
        if(mass){
            factor <- (mx/mi)*(dir * abs(ci - cj) / ((sqrt(dx^2 + dy^2) + eps))) #Se hace el calculo del "factor" de avance hacia la direccion de la particula deacuerdo a la distancia que hay entre las cargas
        }
        else {
            factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
        }
        fx <- fx - dx * factor #la particula se mueve hacia el resultado
        fy <- fy - dy * factor #
    }
    return(c(fx, fy))
}
#registerDoParallel(makeCluster(detectCores() - 1))
cluster = makeCluster(detectCores(logical=FALSE))
if(impr){
    system("rm -f p9_t*.png") # borramos anteriores en el caso que lo hayamos corrido
    digitos <- floor(log(tmax, 10)) + 1
    tl <- "0"
    while (nchar(tl) < digitos) {
        tl <- paste("0", tl, sep="")
    }
    #png(paste("p9_t", tl, ".png", sep=""))
    #plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
#         main="Estado inicial", xlab="X", ylab="Y")
    #graphics.off()
    siz = 15
    ggplot(p, aes(x=x, y=y,col=colores[p$g+6]))+
            geom_point(aes(size = m))+
            labs(size='masa',col='cargas')+
            scale_color_manual(labels=seq(5,-5,-1),values=colores)+
            guides(col= guide_legend(override.aes = list(size=3, stroke=1.5))) +
            scale_size_continuous(breaks=seq(0,10,1),labels=seq(0,10,1))+
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.x = element_text(size=siz), 
                  axis.title.y = element_text(size=siz,angle=0,vjust = 0.5),
                  plot.title = element_text(size=siz+2,hjust = 0.5,face='bold'),
                  legend.text=element_text(size=siz),
                  legend.title = element_text(size=siz,face='bold'),
                  legend.key.size = unit(1.5, 'lines'))+
            ggtitle("Estado inicial") + ylim(-0.1, 1.1) + xlim(-0.1, 1.1)
        ggsave(paste("p9_t", tl, ".png", sep=""))
}
for (iter in 1:tmax) {
    clusterExport(cluster, "p")    
    clusterExport(cluster, "eps")
    clusterExport(cluster, "n")
    clusterExport(cluster, "mass")
    f <- data.frame(t(matrix(parSapply(cluster, 1:n, fuerza), nrow=2)))
    colnames(f) = c("X","Y")
    vv = rbind(vv,cbind(p$m,f))
    delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
    p$x <- apply(matrix(p$x+delta*f$X,nrow=1),2, function(x){ return(ifelse(x>1,1,ifelse(x<0,0,x))) } )
    p$y <- apply(matrix(p$y+delta*f$Y,nrow=1),2, function(x){ return(ifelse(x>1,1,ifelse(x<0,0,x))) } )
    if(impr){
        tl <- paste(iter, "", sep="")
        while (nchar(tl) < digitos) {
            tl <- paste("0", tl, sep="")
        }
        siz = 15
        ggplot(p, aes(x=x, y=y,col=colores[p$g+6]))+
            geom_point(aes(size = m))+
            labs(size='masa',col='cargas')+
            scale_color_manual(labels=seq(5,-5,-1),values=colores)+
            guides(col= guide_legend(override.aes = list(size=3, stroke=1.5))) +
            scale_size_continuous(breaks=seq(0,10,1),labels=seq(0,10,1))+
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.x = element_text(size=siz), 
                  axis.title.y = element_text(size=siz,angle=0,vjust = 0.5),
                  plot.title = element_text(size=siz+2,hjust = 0.5,face='bold'),
                  legend.text=element_text(size=siz),
                  legend.title = element_text(size=siz,face='bold'),
                  legend.key.size = unit(1.5, 'lines'))+
            ggtitle(paste("Paso", iter))+ ylim(-0.1, 1.1) + xlim(-0.1, 1.1)
        ggsave(paste("p9_t", tl, ".png", sep=""))
        #graphics.off()
        print(paste("iterr",tl))
    }
}
stopCluster(cluster)


if(impr){
    system("convert -delay 30 -size 2100x2100 p9_t*.png -loop 0 p9.gif") # creamos animacion con ImageMagick
    #system("rm -f p9_t*.png") # borramos anteriores en el caso que lo hayamos corrido
}
colnames(vv)= c("m","vx","vy")
vv$vp = (vv$vx**2+vv$vy**2)**0.5
#write.csv(vv, "datos.csv")
if(mass){
    write.csv(vv, "datoswm.csv")
    png(paste("p9_comparacion_wm.png", sep=""))
}else{
    write.csv(vv, "datoswom.csv")
    png(paste("p9_comparacion_wom.png", sep=""))
}
boxplot(vv$vp~vv$m)
graphics.off()
