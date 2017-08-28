fname = "datos50000_550000R40C8"
alfa=0.0005
args = commandArgs(trailingOnly=TRUE)
if (length(args)<0)
{
    stop("At least two argument must be supplied (input file).\n", call.=FALSE)
} else if (length(args)>0)
{
    fname = strtoi(args[1])
    
    if (length(args)>1)
    {
        alfa = strtoi(args[2])
    }

}

datosR = read.csv(file = paste("images/",fname, ".csv", sep=""))
datosN = datosR[which(datosR$Nucleos>5),]
datosi = datosN[which(datosN$Tipo=="Impares"),]
datosp = datosN[which(datosN$Tipo=="Pares"),]
datosm = datosN[which(datosN$Tipo=="Medium"),]
datosmp = datosN[which(datosN$Tipo=="MaxPrimo"),]
datosas = datosN[which(datosN$Tipo=="Ordenado"),]
datosds = datosN[which(datosN$Tipo=="Reverso"),]
datosrd = datosN[which(datosN$Tipo=="Aleatorio"),]

linmodi = lm(Time~Nucleos, data=datosi)
linmodp = lm(Time~Nucleos, data=datosp)
linmodm = lm(Time~Nucleos, data=datosm)
linmodmp = lm(Time~Nucleos, data=datosmp)
linmodas = lm(Time~Nucleos, data=datosas)
linmodds = lm(Time~Nucleos, data=datosds)
linmodrd = lm(Time~Nucleos, data=datosrd)

residualesi = resid(linmodi)
residualesp = resid(linmodp)
residualesm = resid(linmodm)
residualesmp = resid(linmodmp)
residualesas = resid(linmodas)
residualesds = resid(linmodds)
residualesrd = resid(linmodrd)

png(file=paste("residuales_impar_", fname,".png", sep=""))
hist(residualesi)
graphics.off()
png(file=paste("qqresiduales_impar_", fname,".png", sep=""))
qqnorm(residualesi)
graphics.off()
png(file=paste("residuales_par_", fname,".png", sep=""))
hist(residualesp)
graphics.off()
png(file=paste("qqresiduales_par_", fname,".png", sep=""))
qqnorm(residualesp)
graphics.off()
png(file=paste("residuales_medio_", fname,".png", sep=""))
hist(residualesm)
graphics.off()
png(file=paste("qqresiduales_medio_", fname,".png", sep=""))
qqnorm(residualesm)
graphics.off()
png(file=paste("residuales_maxprimo_", fname,".png", sep=""))
hist(residualesmp)
graphics.off()
png(file=paste("qqresiduales_maxprimo_", fname,".png", sep=""))
qqnorm(residualesmp)
graphics.off()
png(file=paste("residuales_asc_", fname,".png", sep=""))
hist(residualesas)
graphics.off()
png(file=paste("qqresiduales_asc_", fname,".png", sep=""))
qqnorm(residualesas)
graphics.off()
png(file=paste("residuales_des", fname,".png", sep=""))
hist(residualesds)
graphics.off()
png(file=paste("qqresiduales_des", fname,".png", sep=""))
qqnorm(residualesds)
graphics.off()
png(file=paste("residuales_rand", fname,".png", sep=""))
hist(residualesrd)
graphics.off()
png(file=paste("qqresiduales_rand", fname,".png", sep=""))
qqnorm(residualesrd)
graphics.off()
                                        # prueba shapiro
fileConn<-file(paste("output", fname,".txt"))
writeLines(capture.output(shapiro.test(residualesp)))
writeLines(capture.output(shapiro.test(residualesi)))
writeLines(capture.output(shapiro.test(residualesm)))
writeLines(capture.output(shapiro.test(residualesmp)))
writeLines(capture.output(shapiro.test(residualesas)))
writeLines(capture.output(shapiro.test(residualesds)))
writeLines(capture.output(shapiro.test(residualesrd)))
if(st$p.value<alfa) # si el p.value es menor a alfa, los residuales **NO** siguen una distribucion normal
{
                                        #Pruebas no parametricas
    print("Residuos NO normales, pruebas no parametricas")
    kruskal.test(formula=Time~Nucleos, data=datosp)
    kruskal.test(formula=Time~Nucleos, data=datosi)
    kruskal.test(formula=Time~Nucleos, data=datosm)
    kruskal.test(formula=Time~Nucleos, data=datosmp)
    kruskal.test(formula=Time~Nucleos, data=datosas)
    kruskal.test(formula=Time~Nucleos, data=datosds)
    kruskal.test(formula=Time~Nucleos, data=datosrd)
    writelines(capture.output(print(kw)))
    if(kw$p.value<alfa)
    {
                                        #Existe diferencia entre las varianzas, no son todas iguales, revisar cada factor para encontrar la diferencia
        print("Existe diferencia entre las varianzas, no son todas iguales, revisar cada factor para encontrar la diferencia")
        boxplot(formula=Time~Nucleos, data=datosp)   
        boxplot(formula=Time~Nucleos, data=datosi)   
        boxplot(formula=Time~Nucleos, data=datosm)   
        boxplot(formula=Time~Nucleos, data=datosmp)   
        boxplot(formula=Time~Nucleos, data=datosas)   
        boxplot(formula=Time~Nucleos, data=datosds)   
        boxplot(formula=Time~Nucleos, data=datosrd)        
    } else{
                                        #NO existe diferencia entre las varianzas, son todas iguales
        print("NO existe diferencia entre las varianzas, son todas iguales")
    }
    
	
	
}
else
{
                                        #aplicar anova
    print("Residuos normales, pruebas parametricas, ANOVA")
    anov=aov(linmod)
    writelines(capture.output(print(anov)), file= paste("anova_",fname,".txt"))
	
	
  
}

close(fileConn)
