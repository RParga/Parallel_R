suppressMessages(library(doParallel))
suppressMessages(library(ggplot2))

primo <- function(n) {
    if (n == 1 || n == 2) {
        return(TRUE)
    }
    if (n %% 2 == 0) {
        return(FALSE)
    }
    for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
        if ((n %% i) == 0) {
            return(FALSE)
        }
    }
    return(TRUE)
}
 
desde <- 5
hasta <-  20000
original <- desde:hasta
invertido <- hasta:desde
pares = seq(2, 2*hasta,2)
impares = seq(1, 2*hasta+1,2)
replicas <- 30
type = FALSE
cores = detectCores() #- 1
#datos = data.frame( Tipo= character(),Nucleos= numeric(0), Time= double(0))
datos = data.frame()
for(core in 1:cores)
{
	pt <-  numeric()
	ot <-  numeric()
	it <-  numeric()
	at <-  numeric()
	if(type)
	{
		registerDoParallel(makeCluster(cores))
	}
	else
	{
		cluster <- makeCluster(detectCores())
		clusterExport(cluster, "primo")
	}
	#print(paste('nucleos: ', core))
	for (r in 1:replicas)
	{
	    if(core == 1)
	    {
		    pt <- system.time(sapply(pares, primo))[3] # de menor a mayor
		    mt <- system.time(sapply(impares, primo))[3] # de menor a mayor
		    ot <- system.time(sapply(original, primo))[3] # de menor a mayor
		    it <- system.time(sapply(invertido, primo))[3] # de mayor a menor
		    at <- system.time(sapply(sample(original), primo))[3] # orden aleatorio
	    }
	    else
	    {

		    if(type)
		    {
			pt <- system.time(foreach(n = pares, .combine=c) %dopar% primo(n))[3] # de menor a mayor
			mt <- system.time(foreach(n = impares, .combine=c) %dopar% primo(n))[3] # de menor a mayor
			ot <- system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3] # de menor a mayor
		    	it <- system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3] # de mayor a menor
			at <- system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3] # orden aleatorio
		    }
		    else
		    {
			pt = system.time(parSapply(cluster, pares, primo))[3]
			mt = system.time(parSapply(cluster, impares, primo))[3]
			ot = system.time(parSapply(cluster, original, primo))[3]
			it = system.time(parSapply(cluster, invertido, primo))[3]
			at = system.time(parSapply(cluster, sample(original), primo))[3]
		    }
	    }
	    if(length(datos)==0)
	    {
	    	datos = c('Ordenado',  core, ot)
	    }
	    else
	    {
		datos = rbind(datos,c('Ordenado',  core, ot))
	    }	    
	    datos = rbind(datos,c('Reverso',   core, it))
	    datos = rbind(datos,c('Aleatorio', core, at))
	    datos = rbind(datos,c('Pares', core, pt))
	    datos = rbind(datos,c('Impares', core, mt))
	    print(paste('ot: ', ot, 'it: ', it,'at: ', at, 'pt: ', pt, 'mt: ', mt))
	}
	#if(type)
	#{	
	#}
	#else
	if(!type)
	{
		stopCluster(cluster)
	}
	
}
stopImplicitCluster()
colnames(datos)= c('Tipo', 'Nucleos', 'Time')
print(typeof(datos))
datos = as.data.frame(datos)
write.csv(datos, file="datos.csv")
#ggplot(data = datos[which(datos$Time<0.6),], aes(x=factor(Nucleos), y=Time)) + geom_boxplot(aes(fill=Tipo))
png("boxplotComp.png")
ggplot(data = datos, aes(x=factor(Nucleos), y=as.numeric(paste(Time)))) + labs( x="Núcleos", y="Tiempo" ) + geom_boxplot(aes(fill=Tipo))
graphics.off()


