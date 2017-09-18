suppressMessages(library(parallel))

l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
v <- l / 30
r <- 0.1
tmax <- 100
replicas = 30

impr = FALSE
cluster = makeCluster(detectCores(logical=FALSE))
exper = data.frame(rep = numeric(), time=numeric(), pi = numeric(), mi = numeric()  )

procesaContagio  <- function(i)
{
    ap = agentes[i,] #actual point
    estado = ap$estado
    if(estado == 0) {
        cercanos = agentes[which(agentes$estado == 1 & agentes$x>ap$x-r & agentes$x<ap$x+r & agentes$y>ap$y-r & agentes$y<ap$y+r),]            
        if(nrow(cercanos) > 0){
            d = sqrt(((rep(ap$x, nrow(cercanos))-cercanos$x)**2)+((rep(ap$y,nrow(cercanos))-cercanos$y)**2))
            md = which(d<r)
            if(any(md > 0)){
                dist = d[md]
                li = cbind(cercanos[md,] , dist )            
                if( any(li$estado==1) ){                        
                    aux = li[which(li$estado == 1),]
                    probc <- ( runif(nrow(aux)) < ((rep(r, nrow(aux)) - aux$dist) / rep(r, nrow(aux))))
                    estado = ifelse(any(probc),1,0)                    
                    
                }
            }
        }
    }
    if(estado == 1) {        
        if (runif(1) < pr) {
            estado = 2 # recupera
        }
    }
    return(estado)
}
for(rep in 1:replicas){
    data = c(0, 1, 3)
    pc = pi
    pv = 0
    probs = c(1 - (pc +pv), pc, pv)
    inic = proc.time()
    agentes <- data.frame(x = runif(n,0,l), y = runif(n,0,l), dx = runif(n, -v, v), dy = runif(n, -v, v), estado  = sample(data, n, replace=TRUE, probs)  )
    epidemia <- integer()
    inmunes <- integer()
    saludables <- integer()
    digitos <- floor(log(tmax, 10)) + 1

    clusterExport(cluster, "l")
    clusterExport(cluster, "v")
    clusterExport(cluster, "r")
    clusterExport(cluster, "n")
    clusterExport(cluster, "pi")
    clusterExport(cluster, "pr")



    for (tiempo in 1:tmax) {
                                        #print(paste("Tiempo",tiempo))
        infectados <- nrow(agentes[agentes$estado == 1,])
        epidemia <- c(epidemia, infectados)
        recuperados <- nrow(agentes[agentes$estado == 2,])
        inmunes <- c(inmunes, recuperados)
        sanos <- nrow(agentes[agentes$estado == 0,])
        saludables <- c(saludables, sanos)
        
        if (infectados == 0) {
            break
        }    
        clusterExport(cluster, "agentes")
        contagios = parSapply(cluster, 1:n, procesaContagio)
        agentes$estado = contagios
        agentes$x <- agentes$x + agentes$dx 
        agentes$y <- agentes$y + agentes$dy
        if( any(agentes$x > l) ){
            agentes[which(agentes$x > l),]$x <- agentes[which(agentes$x > l),]$x - l
        }
        if( any(agentes$x < 0) ){
            agentes[which(agentes$x < 0),]$x <- agentes[which(agentes$x < 0),]$x + l
        }
        if( any(agentes$y > l) ){
            agentes[which(agentes$y > l),]$y <- agentes[which(agentes$y > l),]$y - l
        }
        if( any(agentes$y < 0) ){
            agentes[which(agentes$y < 0),]$y <- agentes[which(agentes$y < 0),]$y + l
        }
        aS <- agentes[agentes$estado == 0,]
        aI <- agentes[agentes$estado == 1,]
        aR <- agentes[agentes$estado == 2,]
        if(impr){
            tl <- paste(tiempo, "", sep="")
            while (nchar(tl) < digitos) {
                tl <- paste("0", tl, sep="")
            }
            salida <- paste("p6_tp", tl, ".png", sep="")
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
    exper = rbind(exper, c(rep,(proc.time()- inic)[3], pi, max(epidemia)*100/n))
    png(paste("images/p6ldbp", rep, ".png",sep=""), width=1800, height=900)
    plot(1:length(epidemia), 100 * epidemia / n, pch=16 , col="firebrick2", ylim=c(0,100), xlab="Tiempo", ylab="Porcentaje")
    points(1:length(inmunes), 100 *inmunes / n, pch=17, col="goldenrod")
    points(1:length(saludables), 100 *saludables / n, pch=15, col="chartreuse3")
    graphics.off()
}
colnames(exper) = c("rep", "time", "pi", "mi")
write.csv(exper,"datosldbp.csv")
print(exper)
