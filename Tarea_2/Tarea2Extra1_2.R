library('parallel')
library('plyr')
#suppressMessages(library("sna"))
dim <- 100
num <-  dim^2
seedsl = seq(100, 300, 50)
impr = FALSE
result = list()
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

rotate <- function(x) t(apply(x, 2, rev))

j = 1
for (seeds in seedsl)
{
	#seeds = 100
	mxd = (num/seeds)*seeds**2	


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
	if(impr)
	{
		png("pb2_t000.png")
		#plot.sociomatrix(actual, diaglab=FALSE, main="Inicio",col=rainbow(seeds))
		image(rotate(actual), col=c("#FFFFFFFF",rainbow(seeds)), xaxt='n', yaxt='n')
		graphics.off()
	}
	 
	cluster <- makeCluster(detectCores() - 1)
	clusterExport(cluster, "dim")
	clusterExport(cluster, "paso")
	clusterExport(cluster, "duplicated")
	 
	for (iteracion in 1:mxd) 
	{
		clusterExport(cluster, "actual")
		siguiente <- parSapply(cluster, 1:num, paso)    
		actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
		#print(actual)
		salida = paste("pb2_t", sprintf("%03d",iteracion), ".png", sep="")
		tiempo = paste("Paso", iteracion)
		if (all(siguiente>0)) 
		{ # todos murieron
		    print("Ya no queda nadie vivo.")
			if(impr)
			{
				png(salida)
				#plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
				image(rotate(actual), col=rainbow(seeds), xaxt='n', yaxt='n')
				graphics.off()
			}
		    break;
		}
		if(impr)
		{
			png(salida)
			#plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
			image(rotate(actual), col=c("#FFFFFFFF",rainbow(seeds)), xaxt='n', yaxt='n')
			graphics.off()
		}
	}
	
	edg = count(as.numeric(actual[1,]))$x
	edg = c(edg, count(as.numeric(actual[dim,]))$x)
	edg = c(edg, count(as.numeric(actual[,2]))$x)
	edg = c(edg, count(as.numeric(actual[,dim]))$x)
	edg = edg[!duplicated(edg)]
	

	vcr = count(as.numeric(actual))
	vc = vcr$freq
	
	nvc = setdiff(vcr$x, edg)

	#lvc = vcr[vcr$x %in% as.numeric(nvc)]$freq
	lvc = subset(vcr, vcr$x %in% nvc)$freq
	write.csv(lvc, file= "aver.csv")
	result[[j]] = as.numeric(lvc)
	j = j+1
	#names(vc) = vcr$x
	#write.csv(actual, file=)
	#write.csv(vcr)
	#if(length(result)==0)
	#{
	#	result = lvc
	#}
	#else
	#{
	#	result = add.col(result, lvc)
	#}
	#edgl = vc[!edg]
	#names(edgl) = vcr$x[!edg]

	#png("DistribucionTamaniosTE1.png" )
	#barplot(vc, ylim=c(0,((num/seeds)*seeds**0.5)), col=rainbow(seeds), main="Distribuciones de Tamaños" )
	#graphics.off()
	#png("DistribucionTamaniosNO.png" )
	#barplot(edg, ylim=c(0,((num/seeds)*seeds**0.5)), main="Distribuciones de Tamaños SO" )
	#graphics.off()
}

stopCluster(cluster)
#names(result) = seedsl
#print(result)

#no = sapply(result, length)
#smx = seq_len(max(no))
rm = ldply(result, rbind)
#print(rm)

write.csv(rm, file="DistribucionTamaños.csv")
png("DistribucionTamaniosBPTE1.png" )
boxplot(data.matrix(rm), use.cols=FALSE, main="Distribuciones de Tamaños" )
graphics.off()

