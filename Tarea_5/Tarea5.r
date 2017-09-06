suppressMessages(library(distr))
suppressMessages(library(parallel))

desde <- 3
hasta <- 7
pedazo <- 50000
listpedazo = c(1250, 2500, 5000, 10000, 25000, 50000, 125000)
replicas <- 30
#tamcuantos = c(5,25,125) #c(125,250,500,1000,2000,4000,8000)
cuantos = 50
valE = 0.04883411112604931084064237

f <- function(x) { return(1 / (exp(x) + exp(-x))) }
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador

parte <- function(x) {
    valores <- generador(pedazo)
    return(sum(valores >= desde & valores <= hasta))
}


cluster = makeCluster(detectCores(logical=FALSE) - 1)
#registerDoParallel(makeCluster(detectCores() - 1))
clusterExport(cluster, "desde")
clusterExport(cluster, "hasta")
clusterExport(cluster, "generador")
datos = data.frame()
for(pedazo in listpedazo)
{
    for(i in 1:replicas)
    {        
        clusterExport(cluster, "pedazo")
        tim = proc.time()
        montecarlo = parSapply(cluster, 1:cuantos, parte)
        integral <- sum(montecarlo) / (cuantos * pedazo)
        val = (pi / 2) * integral
        tim = proc.time() - tim
        if(length(datos) == 0)
        {
            datos = c(pedazo*cuantos, i, (abs(valE-val)/valE*100), tim[3]*1000)
        }
        else
        {
            datos = rbind(datos, c(pedazo*cuantos,i, (abs(valE-val)/valE*100), tim[3]*1000) )
        }
    }
}
stopCluster(cluster)
colnames(datos) = c('Tam', 'Rep', 'Gap', 'Time')
#print(datos)
write.csv(datos, "datos.csv")
datos = as.data.frame(datos)

options(scipen=5)
png(paste("bxplt_Gap_Tam.png",sep=""), width = 1500, height = 1125, units = "px", pointsize = 20)
par(mar=c(6,6,4,2),cex.axis=1.5, cex.lab=1.8, cex.main=1.5)
boxplot(datos$Gap ~ as.factor(datos$Tam),xlab="Tamaño Muestra", ylab="Porcentaje de separación al valor exacto de la integral")
graphics.off()
png(paste("bxplt_Time_Tam.png",sep=""), width = 1500, height = 1125, units = "px", pointsize = 20)
par(mar=c(6,6,4,2),cex.axis=1.5, cex.lab=1.8, cex.main=1.5)
boxplot(datos$Time ~ as.factor(datos$Tam),xlab="Tamaño Muestra", ylab="Tiempo de ejecución en milisegundos")
graphics.off()
