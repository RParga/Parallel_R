duration = 200
repetir = 100
library(parallel)
datos = data.frame()

args = commandArgs(trailingOnly=TRUE)
if (length(args)<0)
{
    stop("At least one argument must be supplied (input file).\n", call.=FALSE)
} else if (length(args)>0)
{
    duration = strtoi(args[1])
    if (length(args)>1)
    {
        repetir = strtoi(args[2])
    }

}

experimento = function(replica)
{
    pos = rep(0, dimension)
    origen = 0
    for(t in 1:duration)
    {
	posicion_cambio = sample(0:(dimension),1)
	cambio = 1
	if(runif(1)<0.5)
	{
	    cambio=-1
	}
	pos[posicion_cambio] = pos[posicion_cambio] + cambio
	if(all(pos==0))
	{
	    origen = origen + 1
	}
    }
    return(origen)
}

cluster = makeCluster(detectCores()-1)
clusterExport(cluster, "duration")
clusterExport(cluster, "experimento")
ptm = proc.time() 
for(dimension in 1:8)
{
    clusterExport(cluster, "dimension")
    r = rep (0,repetir)
    resultado = parSapply(cluster, r, experimento)
    datos = rbind(datos, resultado)
    
}
stopCluster(cluster)
proc.time() - ptm
png("PasoPorOrigen.png")
boxplot(data.matrix(datos), use.cols=FALSE,
xlab="Dimensi\u{F3}n", ylab="N\u{FA}mero de veces que pasa por el origen", main="Paso por el origen vs dimensiones")
    
graphics.off()