library(testit)
library(parallel)

replicas = 30
reptime = data.frame(replicas=numeric(),time=numeric(),gap=numeric())

knapsack <- function(cap, peso, valor) {
    n <- length(peso)
    pt <- sum(peso) 
    assert(n == length(valor))
    vt <- sum(valor) 
    if (pt < cap) { 
        return(vt)
    } else {
        filas <- cap + 1 
        cols <- n + 1 
        tabla <- matrix(rep(-Inf, filas * cols),
                       nrow = filas, ncol = cols) 
        for (fila in 1:filas) {
            tabla[fila, 1] <- 0 
        }
        rownames(tabla) <- 0:cap 
        colnames(tabla) <- c(0, valor) 
        for (objeto in 1:n) { 
            for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
                anterior <- acum - peso[objeto]
                if (anterior > 0) { # si conocemos una combinacion con ese peso
                    tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
                }
            }
        }
        return(max(tabla))
    }
}
 
factible <- function(seleccion, pesos, capacidad) {
    return(sum(seleccion * pesos) <= capacidad)
}
 
objetivo <- function(seleccion, valores) {
    return(sum(seleccion * valores))
}
 
normalizar <- function(data) {
    menor <- min(data)
    mayor <- max(data)
    rango <- mayor - menor
    data <- data - menor # > 0
    return(data / rango) # entre 0 y 1
}
 
generador.pesos <- function(cuantos, min, max) {
    return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}
 
generador.valores <- function(pesos, min, max) {
    n <- length(pesos)
    valores <- double()
    for (i in 1:n) {
        media <- pesos[n]
        desv <- runif(1)
        valores <- c(valores, rnorm(1, media, desv))
    }
    valores <- normalizar(valores) * (max - min) + max
    return(valores)
}
 
poblacion.inicial <- function(n, tam) {
    pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
    for (i in 1:tam) {
        pobl[i,] <- round(runif(n))
    }
    return(as.data.frame(pobl))
}
 
mutacion <- function(sol, n) {
    pos <- sample(1:n, 1)
    mut <- sol
    mut[pos] <- (!sol[pos]) * 1
    return(mut)
}
 
reproduccion <- function(x, y, n) {
    pos <- sample(2:(n-1), 1)
    xy <- c(x[1:pos], y[(pos+1):n])
    yx <- c(y[1:pos], x[(pos+1):n])
    return(c(xy, yx))
}
 
cluster = makeCluster(detectCores(logical=FALSE))
for(repli in 1:replicas){
    n <- 50
    pesos <- generador.pesos(n, 15, 80)
    valores <- generador.valores(pesos, 10, 500)
    capacidad <- round(sum(pesos) * 0.65)
    optimo <- knapsack(capacidad, pesos, valores)
    init <- 200
    p <- poblacion.inicial(n, init)
    tim = proc.time()[3]
    tam <- dim(p)[1]
    assert(tam == init)
    pm <- 0.05
    rep <- 50
    tmax <- 50
    mejores <- double()
    for (iter in 1:tmax) {
        p$obj <- NULL
        p$fact <- NULL
        clusterExport(cluster, "pm")    
        clusterExport(cluster, "mutacion")
        clusterExport(cluster, "p")
        clusterExport(cluster, "n")
        p= as.data.frame(t(parSapply(cluster, 1:tam, function(i){
            if(runif(1)<pm){
                return(mutacion(unlist(p[i,]),n))
            }else{
                return(p[i,])
            }
        })))
    #for (i in 1:tam) { # cada objeto puede mutarse con probabilidad pm
    #    if (runif(1) < pm) {
    #        p <- rbind(p, mutacion(p[i,], n))
    #    }
    #}

        tam <- dim(p)[1]
        obj <- double()
        fact <- integer()
        for (i in 1:tam) {
            obj <- c(obj, objetivo(unlist(p[i,]), valores))
            fact <- c(fact, factible(unlist(p[i,]), pesos, capacidad))
        }
        probs = (obj/sum(obj))
        clusterExport(cluster, "p")
        clusterExport(cluster, "reproduccion")
        padres = data.frame(p1=numeric(),p2=numeric())#, piv=numeric())
        padres = rbind(padres, t(sapply(1:rep, function(x){return(sample(1:tam,2,replace=FALSE,prob=probs))})))#,sample(2:(n-1),1)))})))
        hijos = as.data.frame(matrix(unlist(parRapply(cluster,padres,function(x){(reproduccion(p[x[1],],p[x[2],],n))})),ncol=n,byrow=TRUE))
        p=rbind(p,hijos)
    
    #for (i in 1:rep) { # una cantidad fija de reproducciones
    #    padres <- sample(1:tam, 2, replace=FALSE)
    #    hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
    #    p <- rbind(p, hijos[1:n]) # primer hijo
    #    p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
    #}
        tam <- dim(p)[1]
        obj <- double()
        fact <- integer()
        for (i in 1:tam) {
            obj <- c(obj, objetivo(unlist(p[i,]), valores))
            fact <- c(fact, factible(unlist(p[i,]), pesos, capacidad))
        }
        p <- cbind(p, obj)
        p <- cbind(p, fact)
        mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
        p <- p[mantener,]
        tam <- dim(p)[1]
        assert(tam == init)
        factibles <- p[p$fact == TRUE,]
        mejor <- max(factibles$obj)
        mejores <- c(mejores, mejor)
    }
    tim = proc.time()[3] - tim
    print(tim)
    png(paste("t10_",repli,".png",sep=""), width=600, height=300)
    plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
    points(1:tmax, mejores, pch=15)
    abline(h=optimo, col="green", lwd=3)
    graphics.off()
    print(paste(mejor, (optimo - mejor) / optimo))
    reptime = rbind(reptime,c(repli,tim, (optimo - mejor) / optimo))
}
colnames(reptime)= c("replica","time","gap")
write.csv(reptime,"results.csv")
print(reptime)
