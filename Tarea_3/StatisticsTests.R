fname = "datos50000_550000R40C8"
equation = "datos$Time~datos$Nucleos"
alfa=0.0005
args = commandArgs(trailingOnly=TRUE)
if (length(args)<0)
{
    stop("At least two argument must be supplied (input file).\n", call.=FALSE)
} else if (length(args)>0)
{
    fname = strtoi(args[1])
    equation = strtoi(args[2])
    
    if (length(args)>2)
    {
        alfa = strtoi(args[2])
    }

}

datosR = read.csv(file = paste("images/",fname, ".csv", sep=""))
datosN = datosR[which(datosR$Nucleos>2),]
datosi = datosN[which(datosN$Tipo=="Impares"),]
datosp = datosN[which(datosN$Tipo=="Pares"),]

datos = datosp
linmod = lm(equation, data=datos)
residuales = resid(linmod)
png(file=paste("residuales_", fname,".png", sep=""))
hist(residuales)
graphics.off()
png(file=paste("qqresiduales_", fname,".png", sep=""))
qqnorm(residuales)
graphics.off()
                                        # prueba shapiro
fileConn<-file(paste("output", fname,".txt"))
st = shapiro.test(residuales)
writeLines(capture.output(print(st)))
if(ss$p.value<alfa) # si el p.value es menor a alfa, los residuales **NO** siguen una distribucion normal
{
                                        #Pruebas no parametricas
    print("Residuos NO normales, pruebas no parametricas")
    kw<-kruskal.test(formula=equation, data=datos)
    writelines(capture.output(print(kw)))
    if(kw$p.value<alfa)
    {
                                        #Existe diferencia entre las varianzas, no son todas iguales, revisar cada factor para encontrar la diferencia
        print("Existe diferencia entre las varianzas, no son todas iguales, revisar cada factor para encontrar la diferencia")
        boxplot(equiaton)
        
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
