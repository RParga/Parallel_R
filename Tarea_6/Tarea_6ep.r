suppressMessages(library(parallel))

l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
v <- l / 30
r <- 0.1
tmax <- 100
impr = FALSE
digitos <- floor(log(tmax, 10)) + 1

cluster = makeCluster(detectCores(logical=FALSE))
exper = data.frame(rep = numeric(), time=numeric(), pi = numeric(), mi = numeric()  )

contagiosFunc <- function(i){
    a1 <- agentes[i, ]
    cont = FALSE
    if (a1$estado == 0) { # desde los sanos
        for (j in 1:n) {
            a2 <- agentes[j, ]
            if (a2$estado == 1) { # hacia los infectados
                dx <- a1$x - a2$x
                dy <- a1$y - a2$y
                d <- sqrt(dx^2 + dy^2)
                if (d < r) { # umbral
                    p <- (r - d) / r
                    if (runif(1) < p) {
                        cont = TRUE
                    }
                }
            }            
        }
    }
    return(cont)

}

moveFunc <- function(i) { # posibles contagios
    a1 = agentes[i,]
                                        #print(contagios[i])
    if (contagios[i]) {
                                        #print("ns")
                                        #print(a1$estado)
        a1$estado <- 1
    } else if (a1$estado == 1) { # ya estaba infectado
        if (runif(1) < pr) {
            a1$estado <- 2 # recupera
        }
    }
    a1$x <- a1$x + a1$dx
    a1$y <- a1$y + a1$dy
    if (a1$x > l) {
        a1$x <- a1$x - l
    }
    if (a1$y > l) {
        a1$y <- a1$y - l
    }
    if (a1$x < 0) {
        a1$x <- a1$x + l
    }
    if (a1$y < 0) {
        a1$y <- a1$y + l
    }
    agentes[i, ] <- a1   
}


for(r in 1:replicas){
#inic = proc.time()
#agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())
data = c(0, 1, 3)
pc = pi
pv = 0
probs = c(1 - (pc +pv), pc, pv)
inic = proc.time()
agentes <- data.frame(x = runif(n,0,l), y = runif(n,0,l), dx = runif(n, -v, v), dy = runif(n, -v, v), estado  = sample(data, n, replace=TRUE, probs)  )

epidemia <- integer()
inmunes <- integer()
saludables <- integer()

cluster = makeCluster(detectCores(logical=FALSE))
for (tiempo in 1:tmax) {
    infectados <- dim(agentes[agentes$estado == 1,])[1]
    epidemia <- c(epidemia, infectados)
    recuperados <- nrow(agentes[agentes$estado == 2,])
    inmunes <- c(inmunes, recuperados)
    sanos <- nrow(agentes[agentes$estado == 0,])
    saludables <- c(saludables, sanos)

    if (infectados == 0) {
        break
    }
                                        #contagios <- rep(FALSE, n)
    
    clusterExport(cluster, "l")
    clusterExport(cluster, "v")
    clusterExport(cluster, "r")
    clusterExport(cluster, "n")
    clusterExport(cluster, "pi")
    clusterExport(cluster, "pr")
    clusterExport(cluster, "agentes")
                                        #clusterExport(cluster, "contagios")
    contagios = parSapply(cluster, 1:n, contagiosFunc)
                                        #print(contagios)
    clusterExport(cluster, "contagios")
    
    contagios = parSapply(cluster, 1:n, moveFunc)
    
    aS <- agentes[agentes$estado == 0,]
    aI <- agentes[agentes$estado == 1,]
    aR <- agentes[agentes$estado == 2,]
    if(impr){
        tl <- paste(tiempo, "", sep="")
        while (nchar(tl) < digitos) {
            tl <- paste("0", tl, sep="")
        }
        salida <- paste("p6_t", tl, ".png", sep="")
        tiempo <- paste("Paso", tiempo)
        png(salida)
        plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
        if (dim(aS)[1] > 0) {
            points(aS$x, aS$y, pch=15, col="chartreuse3", bg="chartreuse3")
        }
        if (dim(aI)[1] > 0) {
            points(aI$x, aI$y, pch=16, col="firebrick2", bg="firebrick2")
        }
        if (dim(aR)[1] > 0) {
            points(aR$x, aR$y, pch=17, col="goldenrod", bg="goldenrod")
        }
        graphics.off()
    }
}
    exper = rbind(exper, c(r,proc.time()- inic, pi, max(epidemia)))
    png(paste("p6epar", r, ".png",sep=""), width=1800, height=900)
    plot(1:length(epidemia), 100 * epidemia / n, pch=16 , col="firebrick2", ylim=c(0,100), xlab="Tiempo", ylab="Porcentaje")
    points(1:length(inmunes), 100 *inmunes / n, pch=17, col="goldenrod")
    points(1:length(saludables), 100 *saludables / n, pch=15, col="chartreuse3")

    graphics.off()
}
write.csv("datosep.csv")
print(exper)

