binario <- function(d, l) {
    b <-  rep(FALSE, l)
    while (l > 0 | d > 0) {
        b[l] <- (d %% 2 == 1)
        l <- l - 1
        d <- bitwShiftR(d, 1)
    }
    return(b)
}
 
decimal <- function(bits, l) {
    valor <- 0
    for (pos in 1:l) {
        valor <- valor + 2^(l - pos) * bits[pos]
    }
    return(valor)
}
 
modelos <- read.csv("digitos2.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- 0.995
modelos[modelos=='g'] <- 0.92
modelos[modelos=='b'] <- 0.002

replicas=30
pll=TRUE

tipo="Secuencial"
if(pll){
    tipo="Paralelo"
}
r <- 5
c <- 3
dim <- r * c
trains=c(5000,10000,15000)
#train=5000
tests= c(600)
tasa <- 0.15
tranqui <- 0.99
library('parallel')
cluster = makeCluster(detectCores(logical=FALSE))
topes=c(9,12,15,18,21)
#tope <- 21
result = data.frame(R=numeric(),M=numeric(),E=numeric(), T=numeric(), P=numeric())

for(test in tests){
    for(tope in topes){
        for(train in trains){
            digitos <- 0:tope
            k <- length(digitos)
            n <- floor(log(k-1, 2)) + 1
            for(rep in 1:replicas){
                tasa <- 0.15
                neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones, uno por cada posicion de la presentacion binaria log(digito,2)
                tim=proc.time()[3]
                for (t in 1:train) { # entrenamiento
                    d <- sample(0:tope, 1) # escoge una posicion al azar entre el número y el tope
                    pixeles <- runif(dim) < modelos[d + 1,] #crea una imagen aleatoria de la posicion deacuerdo al modelo que le corresponde
                    correcto <- binario(d, n) #obtiene el resultado "real" en representacion binaria 
                    for (i in 1:n) { #para cada neurona, la cual "percibe" cada posicion binaria
                        w <- neuronas[i,] # obtiene su vector de pesos
                        deseada <- correcto[i] # obtiene el valor correspondiente a la posicion si es true o false para el numero selccionado d
                        resultado <- sum(w * pixeles) >= 0 # si el pixel multiplicado por el peso es mayor o igual a cero asigna True en caso contrario false
                                        #print(paste(deseada,resultado, d))
                        if (deseada != resultado) { # si la suma de pixeles y los pesos no es igual al valor deseado
                            ajuste <- tasa * (deseada - resultado) # calcula el ajuste necesario
                            tasa <- tranqui * tasa #disminuye la tasa de cambio para futuros cambios
                            neuronas[i,] <- w + ajuste * pixeles #realiza el ajuste
                        }
                    }
                }
                
                
                if(pll){
                    clusterExport(cluster, "tope")
                    clusterExport(cluster, "binario")
                    clusterExport(cluster, "modelos")
                    clusterExport(cluster, "dim")
                    clusterExport(cluster, "neuronas")
                    clusterExport(cluster, "n")
                    clusterExport(cluster, "k")
                    clusterExport(cluster, "decimal")
                    
                    res = parSapply(cluster, 1:test, function(x){
                        d <- sample(0:tope, 1)
                        pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
                        correcto <- binario(d, n)
                        salida <- rep(FALSE, n)
                        for (i in 1:n) {
                            w <- neuronas[i,]
                            deseada <- correcto[i]
                            resultado <- sum(w * pixeles) >= 0
                            salida[i] <- resultado
                        }
                        r <- min(decimal(salida, n), k) # todos los no-existentes van al final
                                        #contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
                        return(t(c(d,r)))
                    })
                    resm = matrix(res,ncol=2, byrow=TRUE)
                }else{
                    contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
                    rownames(contadores) <- 0:tope
                    colnames(contadores) <- c(0:tope, NA)
                    resm <- matrix(rep(0,2*test),ncol=2) #matrix(res,ncol=2, byrow=TRUE)
                    for (t in 1:test) { # prueba
                        d <- sample(0:tope, 1)
                        pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
                        correcto <- binario(d, n)
                        salida <- rep(FALSE, n)
                        for (i in 1:n) {
                            w <- neuronas[i,]
                            deseada <- correcto[i]
                            resultado <- sum(w * pixeles) >= 0
                            salida[i] <- resultado
                        }
                        r <- min(decimal(salida, n), k) # todos los no-existentes van al final
                                        #contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
                        resm[t,1] <- d
                        resm[t,2] <- r
                    }
                }
                tim=proc.time()[3]-tim
                #print(resm)
                porc=sum(resm[,1]-resm[,2] == 0)/test*100
                pr = c(R=rep,M=tope,E=train,T=tim, P=porc)
                print(pr)
                result = rbind(result,pr)
            }
        }
    }    
}
write.csv(result, paste("datos_",tipo,"_T12_E2.csv",sep=""))
