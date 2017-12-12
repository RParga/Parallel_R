#library(ggplot2)
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
par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE)
boxplot(results$T ~ results$Y+as.factor(results$M), xlab='Cantidad de iteraciones de prueba', ylab="Tiempo", names=c('300','300','600','600','1200','1200'), col=c("firebrick","darkturquoise"), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
legend("right", inset=c(-0.2,0), legend=c('Paralelo','Secuencial'), fill=c("firebrick","darkturquoise"), title="Tipo", cex=1.5)
#gplot(data = results, aes(x=factor(M), y=as.numeric(paste(T)))) + labs( x="Cantidad de iteraciones de prueba", y="Tiempo", fill="Tipo"  ) + geom_boxplot(aes(fill=Y)) + theme(text = element_text(size=30))

graphics.off()
png(paste("bxplt_P_TipoM.png",sep=""),width=960, height=480)
par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE)
boxplot(results$P ~ results$Y+as.factor(results$M), xlab='Cantidad de iteraciones de prueba', ylab="Porcentaje de aciertos", names=c('300','300','600','600','1200','1200'), col=c("firebrick","darkturquoise"),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
legend("right", inset=c(-0.2,0), legend=c('Paralelo','Secuencial'), fill=c("firebrick","darkturquoise"), title="Tipo", cex=1.5)
#ggplot(data = results, aes(x=factor(M), y=as.numeric(paste(P)))) + labs( x="Cantidad de iteraciones de prueba", y="Tiempo", fill="Tipo" ) + geom_boxplot(aes(fill=Y)) + theme(text = element_text(size=30))

graphics.off()


datae1=read.csv("datos_Paralelo_T12_E1.csv", stringsAsFactors=F)
datae1$X=NULL
colnames(datae1)=c('R','M','PN','PG','PB','T','P')
result = datae1[datae1$PN==0,]
png(paste("bxplt_P_PSN.png",sep=""),width=960, height=480)
par(mar=c(5.1, 4.1, 4.1, 13.1), xpd=TRUE)
boxplot(P ~ as.factor(PN)+as.factor(PB)+as.factor(PG), data=result, names=c('N','B','M','A','N','B','M','A','N','B','M','A','N','B','M','A'), col=c("olivedrab","olivedrab","olivedrab","olivedrab","firebrick","firebrick","firebrick","firebrick","mediumorchid1","mediumorchid1","mediumorchid1","mediumorchid1","darkturquoise","darkturquoise","darkturquoise","darkturquoise") , xlab='PB', ylab="Porcentaje", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
legend("right", inset=c(-0.25,0), legend=c("PN=N, PG=N","PN=N, PG=B","PN=N, PG=M",'PN=N, PG=A'), fill=c("olivedrab","firebrick","mediumorchid1","darkturquoise"), title="Grupos", cex=1.5)

#boxplot(P ~ as.factor(PN)+as.factor(PB)+as.factor(PG), las=2, data=result, names=c('NNN','NNB','NNM','NNA','NBN','NBB','NBM','NBA','NMN','NMB','NMM','NMA','NAN','NAB','NAM','NAA'), xlab='Tipo', col=c("olivedrab","olivedrab","olivedrab","olivedrab","firebrick","firebrick","firebrick","firebrick","mediumorchid1","mediumorchid1","mediumorchid1","mediumorchid1","darkturquoise","darkturquoise","darkturquoise","darkturquoise"), ylab="Porcentaje")
#ggplot(data = result, aes(x=interaction(factor(PN),factor(PB),factor(PG)), y=as.numeric(paste(P)))) + labs( x="Cantidad de iteraciones de prueba", y="Tiempo", fill="Tipo" ) + geom_boxplot(aes(fill=PG)) + theme(text = element_text(size=30))
graphics.off()
result = datae1[which(round(datae1$PN,digits=3)==0.333),]
png(paste("bxplt_P_PSB.png",sep=""),width=960, height=480)
par(mar=c(5.1, 4.1, 4.1, 13.1), xpd=TRUE)
boxplot(P ~ as.factor(PN)+as.factor(PB)+as.factor(PG), data=result, names=c('N','B','M','A','N','B','M','A','N','B','M','A','N','B','M','A'), col=c("olivedrab","olivedrab","olivedrab","olivedrab","firebrick","firebrick","firebrick","firebrick","mediumorchid1","mediumorchid1","mediumorchid1","mediumorchid1","darkturquoise","darkturquoise","darkturquoise","darkturquoise") , xlab='PB', ylab="Porcentaje", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
legend("right", inset=c(-0.25,0), legend=c("PN=B, PG=N","PN=B, PG=B","PN=B, PG=M",'PN=B, PG=A'), fill=c("olivedrab","firebrick","mediumorchid1","darkturquoise"), title="Grupos", cex=1.5)
#boxplot(P ~ as.factor(PN)+as.factor(PB)+as.factor(PG), las=2, data=result, names=c('BNN','BNB','BNM','BNA','BBN','BBB','BBM','BBA','BMN','BMB','BMM','BMA','BAN','BAB','BAM','BAA'), xlab='Tipo', col=c("olivedrab","olivedrab","olivedrab","olivedrab","firebrick","firebrick","firebrick","firebrick","mediumorchid1","mediumorchid1","mediumorchid1","mediumorchid1","darkturquoise","darkturquoise","darkturquoise","darkturquoise"), ylab="Porcentaje")
graphics.off()
result = datae1[which(round(datae1$PN,digits=3)==0.667),]
png(paste("bxplt_P_PSM.png",sep=""),width=960, height=480)
par(mar=c(5.1, 4.1, 4.1, 13.1), xpd=TRUE)
boxplot(P ~ as.factor(PN)+as.factor(PB)+as.factor(PG), data=result, names=c('N','B','M','A','N','B','M','A','N','B','M','A','N','B','M','A'), col=c("olivedrab","olivedrab","olivedrab","olivedrab","firebrick","firebrick","firebrick","firebrick","mediumorchid1","mediumorchid1","mediumorchid1","mediumorchid1","darkturquoise","darkturquoise","darkturquoise","darkturquoise") , xlab='PB', ylab="Porcentaje", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
legend("right", inset=c(-0.25,0), legend=c("PN=M, PG=N","PN=M, PG=B","PN=M, PG=M",'PN=M, PG=A'), fill=c("olivedrab","firebrick","mediumorchid1","darkturquoise"), title="Grupos", cex=1.5)
#boxplot(P ~ as.factor(PN)+as.factor(PB)+as.factor(PG), las=2, data=result, names=c('MNN','MNB','MNM','MNA','MBN','MBB','MBM','MBA','MMN','MMB','MMM','MMA','MAN','MAB','MAM','MAA'), col=c("olivedrab","olivedrab","olivedrab","olivedrab","firebrick","firebrick","firebrick","firebrick","mediumorchid1","mediumorchid1","mediumorchid1","mediumorchid1","darkturquoise","darkturquoise","darkturquoise","darkturquoise"), xlab='Tipo', ylab="Porcentaje")
graphics.off()
result = datae1[datae1$PN==1,]
png(paste("bxplt_P_PSA.png",sep=""),width=960, height=480)
par(mar=c(5.1, 4.1, 4.1, 13.1), xpd=TRUE)
boxplot(P ~ as.factor(PN)+as.factor(PB)+as.factor(PG), data=result, names=c('N','B','M','A','N','B','M','A','N','B','M','A','N','B','M','A'), col=c("olivedrab","olivedrab","olivedrab","olivedrab","firebrick","firebrick","firebrick","firebrick","mediumorchid1","mediumorchid1","mediumorchid1","mediumorchid1","darkturquoise","darkturquoise","darkturquoise","darkturquoise") , xlab='PB', ylab="Porcentaje", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
legend("right", inset=c(-0.25,0), legend=c("PN=A, PG=N","PN=A, PG=B","PN=A, PG=M",'PN=A, PG=A'), fill=c("olivedrab","firebrick","mediumorchid1","darkturquoise"), title="Grupos", cex=1.5)
graphics.off()
#c("olivedrab","olivedrab","olivedrab","olivedrab","firebrick","firebrick","firebrick","firebrick","mediumorchid1","mediumorchid1","mediumorchid1","mediumorchid1","darkturquoise","darkturquoise","darkturquoise","darkturquoise")

datae2=read.csv("datos_Paralelo_T12_E2.csv", stringsAsFactors=F)
datae2$X=NULL
colnames(datae2)=c('R','M','E','T','P')
png(paste("bxplt_P_CHE5000.png",sep=""),width=960, height=480)
par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE)
boxplot(datae2$P~datae2$M+datae2$E, xlab='Cantidad de Caracteres', ylab='Porcentaje de aciertos',col=c(rep("firebrick",5),rep("mediumorchid1",5),rep("darkturquoise",5)), names=rep(c(10,13,16,19,22),3),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
legend("right", inset=c(-0.2,0), legend=c("5,000","10,000","15,000"), fill=c("firebrick","mediumorchid1","darkturquoise"), title="Tamaño", cex=1.5)
graphics.off()
