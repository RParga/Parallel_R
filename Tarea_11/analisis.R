library(ggplot2)
datap = read.csv("Parallelo_data.csv")
datas = read.csv("Secuencial_data.csv")
datap = cbind("Paralelo", datap)
datas = cbind("Secuencial", datas)
datas$X = NULL
datap$X = NULL
colnames(datap) = c("Tipo", "replica","NE", "NS","T","P")
colnames(datas) = c("Tipo", "replica","NE", "NS","T","P")
datos = rbind(datap,datas)
boxplot(datos$T~datos$Tipo+datos$NE)
png(paste("boxplotTiempo_NS.png", sep=""), width = 960, height = 960, units = "px", pointsize = 12)
ggplot(data = datos, aes(x=factor(NE), y=as.numeric(paste(T)))) + labs( x="Cantidad de soluciones", y="Tiempo" ) + geom_boxplot(aes(fill=Tipo))
graphics.off()

png(paste("violinProcentaje_NE",".png", sep=""), width = 1440, height = 960, units = "px", pointsize = 10)
ggplot(data = datos, aes(x=factor(replica), y=P)) + labs( x="Cantidad de funciones objetivo", y="Porcentaje de soluciones no dominadas" ) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.1, fill="blue", color="white", lwd=1) + theme(text = element_text(size=40))
graphics.off()
boxplot(datos$P~datos$replica)

