library(parallel)
dim = 20 
num =  dim^2
pvl = seq(0.05,1,0.05)
repet = 50
mxd = dim *5
#suppressMessages(library("sna"))
result = data.frame()
resulti = data.frame()

rotate <- function(x) t(apply(x, 2, rev))
paso <- function(pos)
{
    fila <- floor((pos - 1) / dim) + 1
    columna <- ((pos - 1) %% dim) + 1
    vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                        max(columna - 1, 1): min(columna + 1, dim)]
    return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

for(pv in pvl)
{
    rep = 1
    res = data.frame()
	resi = 0
    while(rep <= repet)
    {
		actual <- matrix(round( 0.49999+(pv)-runif(num)), nrow=dim, ncol=dim)
    	i=0;
    	for (iteracion in 1:mxd)
    	{
			i=i+1
			clusterExport(cluster, "actual")
    	    siguiente <- parSapply(cluster, 1:num, paso)
    	    if (all(siguiente == 0))
	    	{ # todos murieron
                #print("Ya no queda nadie vivo.")
	        	i=iteracion
            	break;
    	    }
	    	actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    	}
		if(i<mxd)
		{
		    res = rbind(res, i)
	    	rep = rep+1
    	}
		else
		{
			resi = resi + 1
		}
    }

    if(length(result) !=0)
    {
		result = cbind(result,res)
		resulti = cbind(resulti, resi)
    }
    else
    {
		result = res #data.frame(res)
		resulti = resi
    }
}
colnames(result) = pvl
colnames(resulti) = pvl

write.csv(result, file="resultadosTarea2.csv")
write.csv(resulti, file="resultadosInfinitosTarea2.csv")
stopCluster(cluster)
#postscript('Tarea2.ps')
png('Tarea2.png')
boxplot(result)
#dev.off()
graphics.off()
#postscript('Tarea2.ps')
png('Tarea2Infi.png')
barplot(resulti)
#dev.off()
graphics.off()
