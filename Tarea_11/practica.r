library(ggplot2) # recordar instalar si hace falta
pick.one <- function(x) {
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
    } # si no hay empeora, vemos si hay mejora
    return(sum(challenger > target) > 0)
}
 
vc <- 4 #número de variables
md <- 3 #maximo grado
tc <- 5 #numero de terminos
k <- 2 # cuantas funciones objetivo
obj <- list()
for (i in 1:k) { # se crean las k funciones de manera aleatoria
    obj[[i]] <- poli( md, vc, tc) 
}
minim <- (runif(k) > 0.5) #Se establece para cada k si se max o se min
sign <- (1 + -2 * minim) #Se establece el signo de cada función
n <- 200 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc) #se crean valores aleatorios para cada variable de las n soluciones
val <- matrix(rep(NA, k * n), nrow=n, ncol=k) #se crean la matriz de valores de las evaluaciones de los elementos de cada solucion
for (i in 1:n) { # evaluamos las soluciones
    for (j in 1:k) { # para todos los objetivos
        val[i, j] <- eval(obj[[j]], sol[i,], tc)
    }
}
#cambiar para k elementos
mejor1 <- which.max(sign[1] * val[,1]) #se obtiene el mejor valor de funcion 1
mejor2 <- which.max(sign[2] * val[,2]) # se obtiene el mejor valor de funcion 2
cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
png("p11_init.png")
plot(val[,1], val[,2], xlab=xl, ylab=yl, main="Ejemplo bidimensional")
graphics.off()
png("p11_mejores.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(val[mejor1, 1], val[mejor1, 2], col="blue", pch=15, cex=1.5)
points(val[mejor2, 1], val[mejor2, 2], col="orange", pch=16, cex=1.5)
graphics.off()
#frente de pareto
#conjunto de los no dominados
no.dom <- logical()
dominadores <- integer()
for (i in 1:n) { #para casa solución
    d <- logical() #vector de los dominados
    for (j in 1:n) { #se compara contra TODAS las demas soluciones
        d <- c(d, domin.by(sign * val[i,], sign * val[j,], k)) #se establece si para cada k hay una mejora o no 
    }
    cuantos <- sum(d)
    dominadores <- c(dominadores, cuantos) #vector de los soluciones dominadas 
    no.dom <- c(no.dom, cuantos == 0) # se agrega si nadie le domina
}
frente <- subset(val, no.dom) # solamente las no dominadas
png("p11_frente.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
graphics.off()
data <- data.frame(pos=rep(0, n), dom=dominadores)
print(data)
png("p11_hist.png")
hist(data$dom)
graphics.off()
png("p11_violin.png")
gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
gr + geom_boxplot(width=0.2, fill="blue", color="white", lwd=2) +
    xlab("") +
    ylab("Frecuencia") +
    ggtitle("Cantidad de soluciones dominantes")
graphics.off()
