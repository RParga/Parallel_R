r1 =read.csv('resultsParallel.csv')
r2 =read.csv('resultsSecuential.csv')
r1 =cbind('Paralelo', r1)
r2 = cbind('Secuencial', r2)
r1$X=NULL
r2$X=NULL
colnames(r1)=c('type','replica','time','gap')
colnames(r2)=c('type','replica','time','gap')
results = rbind(r1,r2)

png(paste("bxplt_Tiempo_Tipo.png",sep=""))
boxplot(results$time ~ results$type, xlab='Tipo', ylab="Tiempo")
graphics.off()
png(paste("bxplt_Gap_Tipo.png",sep=""))
boxplot(results$gap ~ results$type, xlab='Tipo', ylab="Porcentaje de separación")
graphics.off()


linmod = lm(gap~type, data=results)
residuales = resid(linmod)

png(file=paste("residuales_spg.png", sep=""))
hist(residuales)
graphics.off()

png(file=paste("qqresiduales_spg.png", sep=""))
qqnorm(residuales,col=rgb(0,1,0,0.5), ylab='Quantiles', xlab='Quantiles teóricos')
qqline(residuales,col="red")
graphics.off()

st = shapiro.test(residuales)
print(st)
anovg = aov(linmod)
summary(anovg)


linmod = lm(time~type, data=results)
residuales = resid(linmod)

png(file=paste("residuales_spt.png", sep=""))
hist(residuales)
graphics.off()

png(file=paste("qqresiduales_spt.png", sep=""))
qqnorm(residuales,col=rgb(0,1,0,0.5), ylab='Quantiles', xlab='Quantiles teóricos'))
qqline(residuales,col="red")
graphics.off()

st = shapiro.test(residuales)
print(st)
anovg = aov(linmod)
summary(anovg)
