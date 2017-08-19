library(parallel)
dim <- 10 
num <-  dim^2
actual <- matrix(round(runif(num)), nrow=dim, ncol=dim)
suppressMessages(library("sna"))
postscript("p2_t0.ps")
plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
dev.off()
 
paso <- function(pos) {
    fila <- floor((pos - 1) / dim) + 1
    columna <- ((pos - 1) %% dim) + 1
    vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                        max(columna - 1, 1): min(columna + 1, dim)]
    return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}
 
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")
 
for (iteracion in 1:9) {
    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:num, paso)
    if (sum(siguiente) == 0) { # todos murieron
        print("Ya no queda nadie vivo.")
        break;
    }
    actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    salida = paste("p2_t", iteracion, ".ps", sep="")
    tiempo = paste("Paso", iteracion)
    postscript(salida)
    plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
    dev.off()
}
stopCluster(cluster)
