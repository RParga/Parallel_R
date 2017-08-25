fname = ""
alfa=0.0005
args = commandArgs(trailingOnly=TRUE)
if (length(args)<0)
{
    stop("At least two argument must be supplied (input file).\n", call.=FALSE)
}
else if (length(args)>0)
{
    fname = strtoi(args[1])
    equation = strtoi(args[2])
    
    if (length(args)>2)
    {
        alfa = strtoi(args[2])
    }

}

datos = read.csv(file = paste(fname, ".csv", sep=""))
linmod = lm(equation, data=datos)
residuales = resid(linmod)
png(file=paste("residuales_", fname,".png", sep=""))
hist(residuales)
graphics.off()
png(file=paste("qqresiduales_", fname,".png", sep=""))
qqnorm(residuales)
graphics.off()
# prueba shapiro
st = shaphiro.test(residuales)
writelines(capture.output(print(ss)), file= paste("shapiro_",fname,".txt"))
if(ss$p.value<alfa) # si el p.value es menor a alfa, los residuales **NO** siguen una distribucion normal
{
	#Pruebas no parametricas
	kw<-kruskal.test(equation, data=datos)
	writelines(capture.output(print(kw)), file= paste("kruskallWallis_",fname,".txt"))
	
}
else
{
	#aplicar anova
	anov=aov(linmod)
	writelines(capture.output(print(anov)), file= paste("anova_",fname,".txt"))
	
	
  
}