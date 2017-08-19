library('parallel')
library('plyr')
#suppressMessages(library("sna"))
dim <- 100
seeds = 20
num <-  dim^2
mxd = (num/seeds)	
freqrep = 3
probns = 1
rl = rainbow(seeds+round(dim/freqrep))
rl = sample(rl)
print(rl)
paso <- function(pos)
{
    fila <- floor((pos - 1) / dim) + 1
    columna <- ((pos - 1) %% dim) + 1
    rs = actual[fila,columna]    
    if(rs==0)
    {
        vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                        max(columna - 1, 1): min(columna + 1, dim)]
    	#sel = vecindad[vecindad>0]
	sel =  vecindad[!duplicated(vecindad)]
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

rotate = function(x) t(apply(x, 2, rev))


vs = data.frame()
actual <- matrix(rep(0,num), nrow=dim, ncol=dim)
seedl = sample(1:num, seeds)
sv = 0
for(s in seedl)
{
    sv = sv + 1
#    fil = floor((s - 1) / dim) + 1
#    col = ((s - 1) %% dim) + 1
    actual[s] = sv
#    vs = rbind(vs,c(sv,c(fil,col)))
}
#print(actual)

png("pb2_t0.png")
#plot.sociomatrix(actual, diaglab=FALSE, main="Inicio",col=rainbow(seeds))
image(rotate(actual), col=c("#FFFFFFFF",rl[1:seeds]), xaxt='n', yaxt='n')
graphics.off()
 
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")
clusterExport(cluster, "duplicated")
 
for (iteracion in 1:mxd) {

    if(iteracion%%freqrep==0 && runif(1) < probns)
    {
	zl = which(actual==0)
	ns = sample(zl,1)
	#print(zl)
	seeds = seeds +1
	#rl = rbind(rl, rainbow(seeds)[1])
	actual[ns] = seeds
    }


    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:num, paso)    
    actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    #print(actual)
    salida = paste("pb2_t", iteracion, ".png", sep="")
    tiempo = paste("Paso", iteracion)
    if (all(siguiente>0)) { # todos murieron
        print("Ya no queda nadie vivo.")
	png(salida)
	#plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
	image(rotate(actual), col=rl[1:seeds], xaxt='n', yaxt='n')
	graphics.off()
        break;
    }
    png(salida)
    #plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
    image(rotate(actual), col=c("#FFFFFFFF",rl[1:seeds]), xaxt='n', yaxt='n')
    graphics.off()
}
#edg = actual[!duplicated(actual[1,])]
#edg =c(edg, actual[!duplicated(actual[dim,])])
#edg =c(edg, actual[!duplicated(actual[,1])])
#edg =c(edg, actual[!duplicated(actual[,dim])])
#edg = edg[!duplicated(edg)]

vcr = count(as.vector(actual))
vc = vcr$freq
names(vc) = vcr$x


#edgl = vc[!edg]
#names(edgl) = vcr$x[!edg]

png("DistribucionTamanios.png" )
barplot(vc, ylim=c(0,((num/seeds)*seeds**0.5)), col=rl, main="Distribuciones de Tamaños" )
graphics.off()
#png("DistribucionTamaniosNO.png" )
#barplot(edg, ylim=c(0,((num/seeds)*seeds**0.5)), main="Distribuciones de Tamaños SO" )
#graphics.off()

stopCluster(cluster)