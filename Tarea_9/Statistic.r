alfa=0.0005
fname= "datos"
datos1 = read.csv(file = paste(fname,"wm", ".csv", sep=""))
datos2 = read.csv(file = paste(fname,"wom", ".csv", sep=""))

datos= as.data.frame(rbind(cbind(rep(1, nrow(datos1)),datos1$vp,datos1$m),cbind(rep(2, nrow(datos2)),datos2$vp,datos2$m)))

colnames(datos)= c("t","vp", "m")

muestra=datos[sample(nrow(datos),5000),]
linM=lm(vp~t, data=muestra)
residualesM<-resid(linM)
histogramaM<-hist(residualesM)

linmod = lm(vp~t, data=datos)
residuales = resid(linmod)
png(file=paste("residuales_", fname,".png", sep=""))
hist(residuales)
graphics.off()
png(file=paste("qqresiduales_", fname,".png", sep=""))
qqnorm(residuales,col=rgb(0,1,0,0.5))
points(qqnorm(residualesM,plot=FALSE),col=rgb(0,0,1,0.5),pch=4)
qqline(residuales,col="red")
legend("bottomright", legend = c("Original", "Muestra"), pch = c(1,4), col = c("green","blue"))
graphics.off()



st = shapiro.test(residualesM)
print(st)


kw<-kruskal.test(formula=datos$vp~datos$t, data=datos)
print(kw)
