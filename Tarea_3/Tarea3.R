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
 
desde <- 10000
hasta <-  50000
original <- desde:hasta
invertido <- hasta:desde
replicas <- 50
suppressMessages(library(doParallel))
suppressMessages(library(ggplot2))
cores = detectCores() #- 1
#datos = data.frame( Tipo= character(),Nucleos= numeric(0), Time= double(0))
datos = data.frame()
for(core in 1:cores)
{
	registerDoParallel(makeCluster(detectCores() - 1))
	#print(paste('nucleos: ', core))

	ot <-  numeric()
	it <-  numeric()
	at <-  numeric()
	for (r in 1:replicas)
	{
	    ot <- system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3] # de menor a mayor
	    it <- system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3] # de mayor a menor
	    at <- system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3] # orden aleatorio
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
	    print(paste('ot: ', ot, 'it: ', it,'at: ', at))
	}
	
	#summary(ot)
	#summary(it)
	#summary(at)
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


