suppressMessages(library(distr))
suppressMessages(library(parallel))

desde <- -0.5
hasta <- 0.5
#pedazo <- 5000
listpedazo = c(1250, 2500, 5000, 10000, 25000, 50000, 125000)
replicas <- 30
#tamcuantos = c(5,25,125) #c(125,250,500,1000,2000,4000,8000)
cuantos = 50
valE = pi



f <- function(x,y) { return(xs^2 + ys^2) }
#g <- function(x) { return((2 / pi) * f(x)) }



#generador  <- r(AbscontDistribution(d = f)) # creamos un generador

parte <- function(z) {
    xs <- runif(pedazo)
    ys <- runif(pedazo)
    return(sum(xs^2+ys^2 <= hasta^2+desde^2))
}

cluster = makeCluster(detectCores(logical=FALSE) - 1)
#registerDoParallel(makeCluster(detectCores() - 1))
clusterExport(cluster, "desde")
clusterExport(cluster, "hasta")
#clusterExport(cluster, "generador")
datos = data.frame()
for(pedazo in listpedazo)
{
    for(i in 1:replicas)
    {
        
        clusterExport(cluster, "pedazo")
        tim = proc.time()
        piaprox = parSapply(cluster, 1:cuantos, parte)
        val <- sum(piaprox) / (cuantos * pedazo) * 8
        #print(piaprox)
        tim = proc.time() - tim
        if(length(datos) == 0)
        {
            datos = c(pedazo*cuantos, i, (abs(valE-val)/valE*100), tim[3]*1000, val)
        }
        else
        {
            datos = rbind(datos, c(pedazo*cuantos,i, (abs(valE-val)/valE*100), tim[3]*1000, val) )
        }
    }
}
stopCluster(cluster)
colnames(datos) = c('Tam', 'Rep', 'Gap', 'Time', 'Val')
write.csv(datos, "datosPi.csv")
datos = as.data.frame(datos)
#print(datos)

options(scipen=5)
png(paste("bxplt_Gap_TamE1.png",sep=""), width = 1500, height = 1125, units = "px", pointsize = 20)
par(mar=c(6,6,4,2),cex.axis=1.5, cex.lab=1.8, cex.main=1.5)
boxplot(datos$Gap~as.factor(datos$Tam),xlab="Tamaño Muestra", ylab="Porcentaje de separación al valor exacto de la integral")
graphics.off()
png(paste("bxplt_Time_TamE1.png",sep=""), width = 1500, height = 1125, units = "px", pointsize = 20)
par(mar=c(6,6,4,2),cex.axis=1.5, cex.lab=1.8, cex.main=1.5)
boxplot(datos$Time~as.factor(datos$Tam),xlab="Tamaño Muestra", ylab="Tiempo de ejecución en milisegundos")
graphics.off()
