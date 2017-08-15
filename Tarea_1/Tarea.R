
library(parallel)
datos = data.frame()
temp=c()
dimensions = 1:8
repetir = seq(50,200,50)
durations = seq(100,300,50)

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
clusterExport(cluster, "experimento")
ptm = proc.time()

for(repet in repetir){
    for (dimension in dimensions) {
    	clusterExport(cluster, "dimension")
        for(duration in durations)
	{
 	    clusterExport(cluster, "duration")	    
	    r = rep (0,repet)
    	    resultado = parSapply(cluster, r, experimento)
	    for(i in 1:repet)
	    {
                datos = rbind(datos, c(repet,dimension,duration,resultado[i]))
            	temp = c(temp,paste(repet,"_",dimension,"_",duration))
	    }
	}
    }
}
stopCluster(cluster)


# renombre de las columnas
names(datos) =c("iteration","dimension","duration","crxs")

datos$iteration = as.factor(datos$iteration)
datos$dimension = as.factor(datos$dimension)
datos$duration = as.factor(datos$duration)
datos$all = as.factor(temp)

# modelo lineal
linmod = lm(datos$crxs~datos$iteration+datos$dimension+datos$duration)
residuales = resid(linmod)
png("residuales.png")
ghist = hist(residuales)
graphics.off()
den <- density(resid(linmod))



muestra=datos[sample(nrow(datos),5000),]
linmodM=lm(muestra$crxs~muestra$iteration+muestra$dimension+muestra$duration)
residualesM=resid(linmodM)
png("residualesMuestra.png")
histogramaM=hist(residualesM)
graphics.off()
denM = density(resid(linmodM))

ss<-shapiro.test(residualesM)
print(ss)

#Los datos no provienen de una distribución normal, aplicar pruebas no parametricas

#Prueba de kruskal y Wallis para determiniar si hay diferencia significativa entre las medianas de las configuraciones
#Una configuracion es una terna (repeticiones,dimension,duracion)
kw<-kruskal.test(datos$crxs~datos$all,data=datos)
print(kw)
#Existe diferencia entre las configuraciones

#Ahora revisemos cada factor por separado

#Repeticiones
kw<-kruskal.test(datos$crxs~datos$iteration,data=datos)
print(kw)
#No hay diferencia signifcativa entre los niveles del factor repeticiones; es decir, el número 
#de cruces no depende del numero derepeticiones

#Duracion
kw<-kruskal.test(datos$crxs~datos$duration,data=datos)
print(kw)
#No hay diferencia signifcativa entre los niveles del factor duracion; es decir, el número 
#de cruces no depende de la duración de la caminata

#Diagrama de bigotes para duracion
png("T1_boxplot_duracion.png")
boxplot(datos$crxs~datos$duration,xlab="Duracion",ylab="Cruces con el origen")
graphics.off()

#Dimension
kw<-kruskal.test(datos$crxs~datos$dimension,data=datos)
print(kw)
#Si hay diferencia signifcativa entre los niveles del factor dimensión; de acuerdo a lo anterior, el número
#de cruces solo depende de la duración

#Diagrama de bigotes para dimension
png("T1_boxplot_dimension.png")
boxplot(datos$crxs~datos$dimension,xlab="Dimension",ylab="Cruces con el origen")
graphics.off()

#Revisemos si todos los niveles son significativos

  library(FSA)

PT = dunnTest(datos$crxs~datos$dimension, method="bh") 
PT = PT$res
#Resultado de prueba Dunn, comparacion entre cada par de dimensiones
print(PT)

#Pares no significtaivos al 99.9% de confianza
print(PT[PT$P.adj>=0.00001,]) #estos son estadisticamente equivalentes