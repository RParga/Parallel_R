suppressMessages(library(ggplot2))
suppressMessages(library(parallel))


#n <- 40
replicas = 200
limite = 0 # grietas de que largo minimo queremos graficar
#zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
#k <- 12
ns = c(40,90,140,190,250)
datos = data.frame()    

#a una celda pos dada, le asigna el valor de region que le corresponde, deacuerdo a la semilla más cercana, para eso calcula todas las distancias y escoge la más cercana.
celda <-  function(pos) {
    fila <- floor((pos - 1) / n) + 1
    columna <- ((pos - 1) %% n) + 1
    if (zona[fila, columna] > 0) { # es una semilla
        return(zona[fila, columna])
    } else {
        cercano <- NULL # sin valor por el momento
        menor <- n * sqrt(2) # mayor posible para comenzar la busqueda
        for (semilla in 1:k) {
            dx <- columna - x[semilla]
            dy <- fila - y[semilla]
            dist <- sqrt(dx^2 + dy^2)
            if (dist < menor) {
                cercano <- semilla
                menor <- dist
            }
        }
        return(cercano)
    }
}

paso <- function(pos)
{
    fila <- floor((pos - 1) / n) + 1
    columna <- ((pos - 1) %% n) + 1
    cercano = zona[fila,columna]
    if(cercano==0)
    {
        vecindad <-  zona[max(fila - 1, 1) : min(fila + 1, n),
                        max(columna - 1, 1): min(columna + 1, n)]
    	sel = vecindad[vecindad>0]
	#sel =  vecindad[!duplicated(vecindad)]
    	if(length(sel)>0)
        {
            cercano=min(sel)
        }
        
        #menor <- n * sqrt(2) # mayor posible para comenzar la busqueda
        #for (semilla in 1:k)
        #{
        #    dx <- columna - x[semilla]
        #    dy <- fila - y[semilla]
        #    dist <- sqrt(dx^2 + dy^2)
        #    if (dist < menor) {
        #        cercano <- semilla
        #        menor <- dist
        #    }
        #}
    }
    return(cercano)
}

#indica en que posicion de las orillas comienza la grieta.
inicio <- function() {
    direccion <- sample(1:4, 1)
    xg <- NULL
    yg <- NULL
    if (direccion == 1) { # vertical
        xg <- 1
        yg <- sample(1:n, 1)
    } else if (direccion == 2) { # horiz izr -> der
        xg <- sample(1:n, 1)
        yg <- 1
    } else if (direccion == 3) { # horiz der -> izq
        xg <- n
        yg <- sample(1:n, 1)
    } else { # vertical al reves
        xg <- sample(1:n, 1)
        yg <- n
    }
    return(c(xg, yg))
}


rotate <- function(x) t(apply(x, 2, rev))

propaga <- function(replica) {
    # probabilidad de propagacion interna
    prob <- 1
    dificil <- 0.99
    grieta <- voronoi # marcamos la grieta en una copia
    i <- inicio() # posicion inicial al azar
    xg <- i[1]
    yg <- i[2]
    largo <- 0
    while (TRUE) { # hasta que la propagacion termine
        grieta[yg, xg] <- 0 # usamos el cero para marcar la grieta
        largo <-  largo + 1
        frontera <- numeric()
        interior <- numeric()
        for (v in 1:vc) {
            vecino <- vp[v,]
            xs <- xg + vecino$dx # columna del vecino potencial
            ys <- yg + vecino$dy # fila del vecino potencial
            if (xs > 0 & xs <= n & ys > 0 & ys <= n) { # no sale de la zona
                if (grieta[ys, xs] > 0) { # aun no hay grieta ahi
                    if (voronoi[yg, xg] == voronoi[ys, xs]) {
                        interior <- c(interior, v)
                    } else { # frontera
                        frontera <- c(frontera, v)
                    }
                }
            }
        }
        elegido <- 0
        if (length(frontera) > 0) { # siempre tomamos frontera cuando haya
            if (length(frontera) > 1) {
                elegido <- sample(frontera, 1)
            } else {
                elegido <- frontera # sample sirve con un solo elemento
            }
            prob <- 1 # estamos nuevamente en la frontera
        } else if (length(interior) > 0) { # no hubo frontera para propagar
            if (runif(1) < prob) { # intentamos en el interior
                if (length(interior) > 1) {
                    elegido <- sample(interior, 1)
                } else {
                    elegido <- interior
                }
                prob <- dificil * prob # mas dificil a la siguiente
            }
        }
        if (elegido > 0) { # si se va a propagar
            vecino <- vp[elegido,]
            xg <- xg + vecino$dx
            yg <- yg + vecino$dy
        } else {
            break # ya no se propaga
        }
    }
    if (limite>0 && largo >= limite) {
        png(paste("p4g_", replica, ".png", sep=""))
        par(mar = c(0,0,0,0))
        image(rotate(grieta), col=rainbow(k+1), xaxt='n', yaxt='n')
        graphics.off()
    }
    return(largo)
}

#suppressMessages(library(doParallel))
for(n  in ns)
{
    ks= c(round(n/10), round(n/2), n, 2*n, round(n*n/2) , round((n**2-n)) )
    dat = data.frame()    
    for(k in ks)
    {        
        zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
        x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
        y <- rep(0, k) # igual como las coordenadas y de las semillas
                                        #Este for "planta" las k semillas en posiciones libres. 
        for (semilla in 1:k) {
            while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
                fila <- sample(1:n, 1)
                columna <- sample(1:n, 1)
                if (zona[fila, columna] == 0) {
                    zona[fila, columna] = semilla
                    x[semilla] <- columna
                    y[semilla] <- fila
                    break
                }
            }
        }
        cluster = makeCluster(detectCores(logical=FALSE))
        clusterExport(cluster, "n")
        clusterExport(cluster, "celda")
        clusterExport(cluster, "zona")
        clusterExport(cluster, "k")
        clusterExport(cluster, "x")
        clusterExport(cluster, "y")
        #registerDoParallel()
        #celdas <- foreach(p = 1:(n * n), .combine=c) %dopar% celda(p)
        celdas = parSapply(cluster, 1:(n*n), celda)
        #stopImplicitCluster()
        voronoi <- matrix(celdas, nrow = n, ncol = n, byrow=TRUE)
        if(limite>0)
        {
            png("p4s.png")
            par(mar = c(0,0,0,0))
            image(rotate(zona), col=rainbow(k+1), xaxt='n', yaxt='n')
            graphics.off()
            png("p4c.png")
            par(mar = c(0,0,0,0))
            image(rotate(voronoi), col=rainbow(k+1), xaxt='n', yaxt='n')
            graphics.off()
        }
        #me da las 8 posiciones de los posibles vecinos
        vp <- data.frame(numeric(), numeric()) # posiciones de posibles vecinos
        for (dx in -1:1) {
            for (dy in -1:1) {
                if (dx != 0 | dy != 0) { # descartar la posicion misma
                    vp <- rbind(vp, c(dx, dy))
                }
            }
        }
        names(vp) <- c("dx", "dy")
        vc <- dim(vp)[1]
    

        #for (r in 1:10)
        #{ # para pruebas sin paralelismo
        #    propaga(r)
        #}
        #suppressMessages(library(doParallel))
        #registerDoParallel(makeCluster(detectCores() - 1))
        #largos <- foreach(r = 1:200, .combine=c) %dopar% propaga(r)

        #cluster = makeCluster(detectCores(logical=FALSE))
        clusterExport(cluster, "replicas")
        clusterExport(cluster, "propaga")
        clusterExport(cluster, "inicio")
        clusterExport(cluster, "voronoi")
        clusterExport(cluster, "vc")
        clusterExport(cluster, "vp")
        clusterExport(cluster, "limite")

        largos = parSapply(cluster, 1:replicas, propaga)
        stopCluster(cluster)
        dat = cbind(largos, rep(n,replicas), rep(k,replicas))
        
        if(length(datos)==0)
        {
            datos = dat
        }
        else
        {
            datos = rbind(datos, dat)
        }
    }
    
}
datos = data.frame(datos)
colnames(datos) = c("Largos","Dimension", "Semillas")
write.csv(datos, file="datos.csv")

datos40 = datos[which( datos$Dimension == 40),]
png(paste("D40",".png", sep=""), width = 960, height = 960, units = "px", pointsize = 20)
ggplot(data = datos40, aes(x=factor(Semillas), y=Largos)) + labs( x="Número de regiones", y="Largos" ) + geom_violin() + facet_wrap(~Dimension, scales="free") + geom_boxplot(width=0.05) + theme(text = element_text(size=20)) + ylim(0,500)
graphics.off()
datos90 = datos[which( datos$Dimension == 90),]
png(paste("D90",".png", sep=""), width = 960, height = 960, units = "px", pointsize = 20)
ggplot(data = datos90, aes(x=factor(Semillas), y=Largos)) + labs( x="Número de regiones", y="Largos" ) + geom_violin() + facet_wrap(~Dimension, scales="free") + geom_boxplot(width=0.05) + theme(text = element_text(size=20)) + ylim(0,600)
graphics.off()
datos140 = datos[which( datos$Dimension == 140),]
png(paste("D140",".png", sep=""), width = 960, height = 960, units = "px", pointsize = 20)
ggplot(data = datos140, aes(x=factor(Semillas), y=Largos)) + labs( x="Número de regiones", y="Largos" ) + geom_violin() + facet_wrap(~Dimension, scales="free") + geom_boxplot(width=0.05) + theme(text = element_text(size=20)) + ylim(0,850)
graphics.off()
png(paste("D190",".png", sep=""), width = 960, height = 960, units = "px", pointsize = 20)
datos190 = datos[which( datos$Dimension == 190),]
ggplot(data = datos190, aes(x=factor(Semillas), y=Largos)) + labs( x="Número de regiones", y="Largos" ) + geom_violin() + facet_wrap(~Dimension, scales="free") + geom_boxplot(width=0.05) + theme(text = element_text(size=20)) + ylim(0,900)
graphics.off()
datos250 = datos[which(datos$Dimension == 250),]
png(paste("D250",".png", sep=""), width = 960, height = 960, units = "px", pointsize = 20)
ggplot(data = datos250, aes(x=factor(Semillas), y=Largos)) + labs( x="Número de regiones", y="Largos" ) + geom_violin() + facet_wrap(~Dimension, scales="free") + geom_boxplot(width=0.05) + theme(text = element_text(size=20))+ ylim(0,1200)
graphics.off()
print("End Tarea4")
