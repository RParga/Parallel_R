datap=read.csv("datos_Paralelo_T12_1.csv", stringsAsFactors=F)
datas=read.csv("datos_Secuencial_T12_1.csv", stringsAsFactors=F)
datap$X=NULL
datas$X=NULL
datas=cbind("Secuencial", datas)
datap=cbind("Paralelo", datap)
colnames(datap)=c('Y','R','M','T','P')
colnames(datas)=c('Y','R','M','T','P')
results = rbind(datap,datas)

png(paste("bxplt_Tiempo_TipoM.png",sep=""),width=960, height=480)
boxplot(results$T ~ results$Y+as.factor(results$M), xlab='Tipo', ylab="Tiempo")
graphics.off()
png(paste("bxplt_P_TipoM.png",sep=""),width=960, height=480)
boxplot(results$P ~ results$Y+as.factor(results$M), xlab='Tipo', ylab="Porcentaje de separación")
graphics.off()

