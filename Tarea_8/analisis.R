suppressMessages(library(ggplot2))
d1 = read.csv("tiempospf.csv")
d1 = d1[!is.na(d1$replica) & d1$k<16000,]
d2 = read.csv("tiempossecf.csv")
d2 = d2[!is.na(d2$replica) & d2$k<16000,]

datos = as.data.frame(rbind(cbind(d1$replica, rep("Paralelo", nrow(d1)), d1$k, d1$time), cbind(d2$replica, rep("Secuencial", nrow(d2)), d2$k, d2$time) ))
colnames(datos) = c("replica","tipo", "k","time")
datos$ko = reorder(datos$k,as.numeric(rownames(datos)))
png(paste("boxplotComplete_1",".png", sep=""), width = 960, height = 960, units = "px", pointsize = 12)
ggplot(data = datos, aes(x=factor(ko), y=as.numeric(paste(time)))) + labs( x="Tamaño cúmulos", y="Tiempo" ) + geom_boxplot(aes(fill=tipo)) + theme(axis.text=element_text(size=16), axis.title=element_text(size=18,face="bold"))
graphics.off()
d1 = read.csv("tiempospf.csv")
d1 = d1[!is.na(d1$replica) & d1$k>16000,]
d2 = read.csv("tiempossecf.csv")
d2 = d2[!is.na(d2$replica) & d2$k>16000,]

datos = as.data.frame(rbind(cbind(d1$replica, rep("Paralelo", nrow(d1)), d1$k, d1$time), cbind(d2$replica, rep("Secuencial", nrow(d2)), d2$k, d2$time) ))
colnames(datos) = c("replica","tipo", "k","time")
datos$ko = reorder(datos$k,as.numeric(rownames(datos)))
png(paste("boxplotComplete_2",".png", sep=""), width = 960, height = 960, units = "px", pointsize = 12)
ggplot(data = datos, aes(x=factor(ko), y=as.numeric(paste(time)))) + labs( x="Tamaño cúmulos", y="Tiempo" ) + geom_boxplot(aes(fill=tipo)) + theme(axis.text=element_text(size=16), axis.title=element_text(size=18,face="bold"))
graphics.off()

d1 = read.csv("tiempospf.csv")
d1 = d1[!is.na(d1$replica),]
d2 = read.csv("tiempossecf.csv")
d2 = d2[!is.na(d2$replica),]
datos = as.data.frame(rbind(cbind(d1$replica, rep("Paralelo", nrow(d1)), d1$k, d1$time), cbind(d2$replica, rep("Secuencial", nrow(d2)), d2$k, d2$time) ))
colnames(datos) = c("replica","tipo", "k","time")
datos$ko = reorder(datos$k,as.numeric(rownames(datos)))

mxl = aggregate(as.numeric(as.character(datos$time)) ~ datos$k, data=datos, max)
colnames(mxl) =c("k","mx")1
datos$mx= 0
for(i in 1:nrow(mxl)){
    #aux = as.numeric(datos[datos$k==mxl[i,]$k,]$time) / mxl[i,]$mx
    #print(aux)
    datos[datos$k==mxl[i,]$k,]$mx = mxl[i,]$mx
}
#datos$ftime= as.numeric((as.character(datos[order(rownames(datos)),]$time)))
datos$Ntime= as.numeric(as.character(datos$time))/datos$mx
png(paste("boxplotComplete_3",".png", sep=""), width = 960, height = 960, units = "px", pointsize = 12)

ggplot(data = datos, aes(x=factor(ko), y=as.numeric(paste(Ntime)))) + labs( x="Tamaño cúmulos", y="Tiempo" ) + geom_boxplot(aes(fill=tipo)) + theme(axis.text=element_text(size=16), axis.title=element_text(size=18,face="bold"))

graphics.off()
wilcox.test(d1$time,d2$time)
