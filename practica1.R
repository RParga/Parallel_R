
library(parallel)
source("fndistancias.R")
datos = data.frame()

experimento = function(dimension=3, duration=200, repeticiones=100, fndist=ed_origen)
{
    pos = rep(0, dimension)
    mayor = 0
    for(t in 1:duration)
    {
	posicion_cambio = sample(0:(dimension-1),1)
	cambio = 1
	if(runif(1)<0.5)
	{
	    cambio=-1
	}
	pos[posicion_cambio] = pos[posicion_cambio] + cambio
	d = fndist(pos)
	if(d>mayor)
	{
	    mayor = d
	}
    }
    return(mayor)
}