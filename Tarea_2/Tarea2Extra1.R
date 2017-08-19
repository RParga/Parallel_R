library('parallel')
library('plyr')
suppressMessages(library("sna"))
dim <- 30
num <-  dim^2
seeds = 15
mxd = 50

paso <- function(pos)
{
    fila <- floor((pos - 1) / dim) + 1
    columna <- ((pos - 1) %% dim) + 1
    rs = actual[fila,columna]    
    if(rs==0)
    {
        vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                        max(columna - 1, 1): min(columna + 1, dim)]
    	sel = vecindad[vecindad>0]
    	if(length(sel)>0)
    	{
	    selv = sum(seq(1/(length(sel)+1),1,1/(length(sel)+1)) >runif(1))
	    if(selv>0 && selv<=length(sel))
	    {
		rs = sel[selv]
	    }
	    else
	    {
	        rs = 0
	    }
    	}
    }
    return(rs)
}

vs = data.frame()
actual <- matrix(rep(0,num), nrow=dim, ncol=dim)
seedl = sample(1:num, seeds)
sv = 0
for(s in seedl)
{
    sv = sv + 1
    fil = floor((s - 1) / dim) + 1
    col = ((s - 1) %% dim) + 1
    actual[fil,col] = sv
    vs = rbind(vs,c(sv,c(fil,col)))
}
#print(actual)

png("pb2_t0.png")
plot.sociomatrix(actual, diaglab=FALSE, main="Inicio",col=rainbow(seeds))
graphics.off()
 
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")
 
for (iteracion in 1:mxd) {
    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:num, paso)    
    actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    #print(actual)
    salida = paste("pb2_t", iteracion, ".png", sep="")
    tiempo = paste("Paso", iteracion)
    png(salida)
    plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
    graphics.off()
    if (all(siguiente>0)) { # todos murieron
        print("Ya no queda nadie vivo.")
        break;
    }
}
vcr = count(as.vector(actual))
vc = vcr$freq
names(vc) = vcr$x
print(vc)
print(vs)
barplot(vc, ylim=c(0,200) )
stopCluster(cluster)