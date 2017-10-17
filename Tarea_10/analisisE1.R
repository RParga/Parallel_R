r1 =read.csv('resultswg.csv')
r2 =read.csv('resultsE1wg.csv')
r1 =cbind('Paralelo', r1)
r2 = cbind('Ruleta Selección', r2)
r1$X=NULL
r2$X=NULL
colnames(r1)=c('type','replica','time','gap','est')
colnames(r2)=c('type','replica','time','gap','est')
results = rbind(r1,r2)

png(paste("bxplt_Gap_TipoE1.png",sep=""))
boxplot(results$time ~ results$type, xlab='Tipo', ylab="Tiempo")
graphics.off()
png(paste("bxplt_Tiempo_TipoE1.png",sep=""))
boxplot(results$gap ~ results$type, xlab='Tipo', ylab="Porcentaje de separación")
graphics.off()
png(paste("bxplt_Estabilidad_TipoE1.png",sep=""))
boxplot(results$est ~ results$type, xlab='Tipo', ylab="Número Iteración")
graphics.off()

