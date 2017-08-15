library(parallel)
datos = data.frame()
duration = 200
repetir = 100

dur = 0
dime = 1

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

caminata = function(repet)
{
    pos = rep(0, dime)
    for(t in 1:dur)
    {
	posicion_cambio = sample(0:dime,1)
	cambio = 1
	if(runif(1)<0.5)
	{
	    cambio=-1
	}
	pos[posicion_cambio] = pos[posicion_cambio] + cambio
    }
    return(pos)

}

experimento = function(dimension)
{
    cl = detectCores()-1
    cluster = makeCluster(cl)
    dur = round(duration/cl)
    clusterExport(cluster, "caminata")
    clusterExport(cluster, "dur")
    for(dime in 1:8)
    {
        resultado = rep(0,dime)
	for(rep in 1:repetir)
	{
	    clusterExport(cluster, "dime")
	    resultadop = parSapply(cluster, 1:cl, caminata)
	    resultado = resultado + resultadop
	}
	datos = rbind(datos, resultado)
	print(datos)
    }
    stopCluster(cluster)
    return(datos)
}
for(di in 1:8)
{
    datosf = data.frame()
    result = experimento(di)
    print(result)
    datosf = cbind(datosf, result)
}
    print(datosf)
