#suppressMessages(library(parallel))

l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
v <- l / 30
r <- 0.1
tmax <- 100

impr = TRUE
replicas = 3
exper = data.frame(rep = numeric(), time=numeric()  )
data = c(0, 1, 2)
pc = pi
pv = 0
probs = c(1 - (pc +pv), pc, pv)
procesaContagio  <- function(y)
{
                                        #contagio por linea de barrido
    ordatos = agentes[order(agentes$x),] #ordenar los datos por el eje x
    cercanos = data.frame(i=numeric(),x=numeric(),y=numeric(), estado=numeric()) # los más cercanos a la linea de barrido que tal ue su distancia a la linea de barrido es menor al umbral
    for(i in 1:n){ #recorrido de los puntos ordenados
        ap = cbind(i,ordatos[i,]) #actual point
        if(nrow(cercanos) > 0){ # si hay puntos cercanos a la linea de barrido
                                        #actualizamos los cercanos al nuevo putno de la linea
            cercanos= cercanos[which(cercanos$x>(ap$x-r)),] 
                                        #calculo de la distancia de los cercanos al punto actual en la linea de barrido
            d = sqrt((rep(ap$x, nrow(cercanos))-cercanos$x)**2)+((rep(ap$y,nrow(cercanos))-cercanos$y)**2)
            md = which(d<r)
                                        #si hay alguien en el rango del umbral
            if(any(md > 0)){
                dist = d[md]
                li = cbind(cercanos[md,] , dist )            
                if(ap$estado == 1){
                    if( any(li$estado==0) ){
                        aux = li[which(li$estado == 0),]
                        dl = (rep(r, nrow(aux)) - aux$dist) / rep(r, nrow(aux))
                        probl = runif(nrow(aux))
                                        #probc <- ( runif(nrow(aux)) < (rep(r, nrow(aux)) - aux$dist) / rep(r, nrow(aux)) )
                        probc= probl < dl
                        ordatos[aux$i,]$estado = ifelse(probc,1,0)                       
                    }
                    if (runif(1) < pr) {
                        ordatos[i,]$estado = 2 # recupera
                    }
                }
                if(ap$estado == 0) {
                    if( any(li$estado==1) ){                        
                        aux = li[which(li$estado == 0),]
                        probc <- ( runif(nrow(aux)) < (rep(r, nrow(aux)) - aux$dist) / rep(r, nrow(aux)) )
                        ordatos[i,]$estado = ifelse(any(probc==TRUE),1,0)
                    }
                }
            }
            
            
        }
        cercanos = rbind(cercanos,ap) #agregar punto actual a cercanos.
    }
    agentes$estado <<- ordatos[order(as.numeric(row.names(ordatos))),]$estado
    agentes$x <<- agentes$x + agentes$dx 
    agentes$y <<- agentes$y + agentes$dy
    if( any(agentes$x > l) ){
        agentes[which(agentes$x > l),]$x <<- agentes[which(agentes$x > l),]$x - l
    }
    if( any(agentes$x < 0) ){
        agentes[which(agentes$x < 0),]$x <<- agentes[which(agentes$x < 0),]$x + l
    }
    if( any(agentes$y > l) ){
        agentes[which(agentes$y > l),]$y <<- agentes[which(agentes$y > l),]$y - l
    }
    if( any(agentes$y < 0) ){
        agentes[which(agentes$y < 0),]$y <<- agentes[which(agentes$y < 0),]$y + l
    }
}

for(r in 1:replicas){
    inic = proc.time()
    agentes <<- data.frame(x = runif(n,0,l), y = runif(n,0,l), dx = runif(n, -v, v), dy = runif(n, -v, v), estado  = sample(data, n, replace=TRUE, probs)  )
    epidemia <- integer()
    inmunes <- integer()
    saludables <- integer()
    digitos <- floor(log(tmax, 10)) + 1   

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
        if (tiempo==1) {
            print(agentes[1:n,])
        }
        procesaContagio(tiempo)        
        if (tiempo==1) {
            print(agentes[1:n,])
        }
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
    exper = rbind(exper, c(r,(proc.time()- inic)[3]))
    png(paste("p6ldb", r, ".png",sep=""), width=1800, height=900)
    plot(1:length(epidemia), 100 * epidemia / n, pch=16 , col="firebrick2", ylim=c(0,100), xlab="Tiempo", ylab="Porcentaje")
    points(1:length(inmunes), 100 *inmunes / n, pch=17, col="goldenrod")
    points(1:length(saludables), 100 *saludables / n, pch=15, col="chartreuse3")
    graphics.off()
}
write.csv(exper, "datosldb.csv")
print(exper)
