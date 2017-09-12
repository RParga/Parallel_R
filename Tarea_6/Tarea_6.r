suppressMessages(library(parallel))
l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
v <- l / 30
r <- 0.1
tmax <- 100

cluster = makeCluster(detectCores(logical=FALSE) - 1)

data = c("S", "I")
p = 0.2
probs = c(1 - p, p)

agentes <- data.frame(x = runif(n,0,l), y = runif(n,0,l), dx = runif(n, -v, v), dy = runif(n, -v, v), estado  = sample(data, n, replace=TRUE, probs)  )
epidemia <- integer()
digitos <- floor(log(tmax, 10)) + 1

clusterExport(cluster, "l")
clusterExport(cluster, "v")
clusterExport(cluster, "pi")
#assign = parSapply(cluster, 1:n, asignacion)

for (tiempo in 1:tmax) {
    infectados <- dim(agentes[agentes$estado == "I",])[1]
    epidemia <- c(epidemia, infectados)
    if (infectados == 0) {
        break
    }
    contagios <- rep(FALSE, n)
    for (i in 1:n) { # posibles contagios
        a1 <- agentes[i, ]
        if (a1$estado == "I") { # desde los infectados
            for (j in 1:n) {
                if (!contagios[j]) { # aun sin contagio
                    a2 <- agentes[j, ]
                    if (a2$estado == "S") { # hacia los susceptibles
                        dx <- as.numeric(a1$x) - as.numeric(a2$x)
                        dy <- as.numeric(a1$y) - as.numeric(a2$y)
                        d <- sqrt(as.numeric(dx)^2 + as.numeric(dy)^2)
                        print(a1)
                        print(i)
                        print(a2)
                        print(j)
                        if (d < r) { # umbral
                            p <- (r - d) / r
                            if (runif(1) < p) {
                                contagios[j] <- TRUE
                            }
                        }
                    }
                }
            }
        }
    }
    for (i in 1:n) { # movimientos y actualizaciones
        a <- agentes[i, ]
        if (contagios[i]) {
            a$estado <- "I"
        } else if (a$estado == "I") { # ya estaba infectado
            if (runif(1) < pr) {
                a$estado <- "R" # recupera
            }
        }
        a$x <- as.numeric(a$x) + as.numeric(a$dx)
        a$y <- as.numeric(a$y) + as.numeric(a$dy)
        if (a$x > l) {
            a$x <- a$x - l
        }
        if (a$y > l) {
            a$y <- a$y - l
        }
        if (a$x < 0) {
            a$x <- a$x + l
        }
        if (a$y < 0) {
            a$y <- a$y + l
        }
        agentes[i, ] <- a
    }
    aS <- agentes[agentes$estado == "S",]
    aI <- agentes[agentes$estado == "I",]
    aR <- agentes[agentes$estado == "R",]
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
png("p6e.png", width=600, height=300)
plot(1:length(epidemia), 100 * epidemia / n, xlab="Tiempo", ylab="Porcentahe de infectados")
graphics.off()
