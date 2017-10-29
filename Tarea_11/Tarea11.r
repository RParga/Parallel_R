library(ggplot2) # recordar instalar si hace falta
library(parallel)
pick.one <- function(x){
    if (length(x) == 1) {
        return(x)
    } else {
        return(sample(x, 1))
    }
}
 
poli <- function(maxdeg, varcount, termcount) {
    f <- data.frame(variable=integer(), coef=integer(), degree=integer())
    for (t in 1:termcount) {
        var <- pick.one(1:varcount)
        deg <- pick.one(1:maxdeg)
        f <-  rbind(f, c(var, runif(1), deg))
    }
    names(f) <- c("variable", "coef", "degree")
    return(f)
}
 
eval <- function(pol, vars, terms) {
    value <- 0.0
    for (t in 1:terms) {
        term <- pol[t,]
        value <-  value + term$coef * vars[term$variable]^term$degree
    }
    return(value)
}
 
domin.by <- function(target, challenger, total) {
    if (sum(challenger < target) > 0) {
        return(FALSE) # hay empeora
    } # si no empeora, vemos si hay mejora
    return(sum(challenger > target) > 0)
}
cluster = makeCluster(detectCores(logical=FALSE) )
vc <- 4 #número de variables
md <- 3 #máximo grado
tc <- 5 #número de terminos
#k <- 2 #cantidad de funciones objetivo
<<<<<<< HEAD
replicas <- 3
ns <- 200#c(50 , 100, 200, 400, 800)
ks = 7#c(2,3,4,5,6)
impr = TRUE
=======
replicas <- 30
ns <- c(50 , 100, 200, 400, 800)
ks = c(2,3,4,5,6)
impr = FALSE
>>>>>>> 561f1b7fd4440ec96610014628ab7259d9bf0cb5
paral=TRUE
results = data.frame(replicas=numeric(), ks=numeric(), ns=numeric(), time=numeric(), pnd=numeric())
for(k in ks){
    for(n in ns){
            for(rep in 1:replicas){
            tim= proc.time()[3]
            obj <- list()
            for (i in 1:k) { # se crean las k funciones de manera aleatoria
                obj[[i]] <- poli( md, vc, tc) 
            }
            minim <- (runif(k) > 0.5) #Se establece para cada k si se max o se min
            sign <- (1 + -2 * minim) #Se establece el signo de cada función
            #n <- 200 # cuantas soluciones aleatorias
            sol <- matrix(runif(vc * n), nrow=n, ncol=vc) #se crean valores aleatorios para cada variable de las n soluciones
            val <- matrix(rep(NA, k * n), nrow=n, ncol=k) #se crean la matriz de valores de las evaluaciones de los elementos de cada solucion
            for (i in 1:n) { # evaluamos las soluciones
                for (j in 1:k) { # para todos los objetivos
                    val[i, j] <- eval(obj[[j]], sol[i,], tc)
                }
            }
            
            no.dom <- logical()
            dominadores <- integer()
            
            
            if(paral){
                clusterExport(cluster, "domin.by")
                clusterExport(cluster, "sign")
                clusterExport(cluster, "val")
                clusterExport(cluster, "k")
                clusterExport(cluster, "i")
                clusterExport(cluster, "n")
                d = (parSapply(cluster, 1:n, function(i){
                        dp <- logical()
                        for (j in 1:n) {
                            dp <- c(dp, domin.by(sign * val[i,], sign * val[j,], k))
                        }
                        cuantos <- sum(dp)
                        return(c(cuantos,cuantos==0))
                }))                
                dominadores = d[1,]
                no.dom = as.logical(d[2,])
                rm(d)
            }else{
                for (i in 1:n) { #para casa solución
                    d <- logical() #vector de los dominados
                    for (j in 1:n) { #se compara contra TODAS las demas soluciones
                        d <- c(d, domin.by(sign * val[i,], sign * val[j,], k)) #se establece si para cada k hay una mejora o no 
                    }
                    cuantos <- sum(d)
                    dominadores <- c(dominadores, cuantos)
                    no.dom <- c(no.dom, cuantos == 0) # nadie le domina
                }
            }
            #print("CDF")
            #print(d)
            #cuantos <- (colSums(d))
            #print(cuantos)
            #dominadores <- c(dominadores, cuantos) #vector de los soluciones dominadas
            #print(dominadores)
            #no.dom <- c(no.dom, cuantos == 0) # se agrega si nadie le domina
            #print(no.dom)
            frente <- subset(val, no.dom) # solamente las no dominadas
            #print(frente)
        
            #if(impr){
            #    png("p11_frente.png")
            #    plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
            #         ylab=paste(yl,"mejor con bolita naranja"),
            #         main="Ejemplo bidimensional")
            #    points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
            #    graphics.off()
            #    print("Frente")
            #    print(dim(frente))
            #    data <- data.frame(pos=rep(0, n), dom=dominadores)
            #    print(data)
            #    png(paste("p_", rep,"_11_hist.png",sep=""))
            #    hist(data$dom)
            #    graphics.off()
            #    png(paste("p_", rep,"_11_violin.png",sep=""))
            #    gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
            #    gr + geom_boxplot(width=0.2, fill="blue", color="white", lwd=2) +
            #    xlab("") +
            #        ylab("Frecuencia") +
            #    ggtitle("Cantidad de soluciones dominantes")
            #    graphics.off()
            #}
            results = rbind(results, c( NE=k, NS=n, replica=rep, T=proc.time()[3]-tim, P=dim(frente)[1]*100/n ))
        }
    }
}
tipo="Secuencial"
if(paral){
    tipo="Parallelo"
}
colnames(results)=c("replica", "NE", "NS", "T", "P")
print(results)
write.csv(results, paste(tipo,"_data.csv",sep=""))
