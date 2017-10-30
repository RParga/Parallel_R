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
boxplot(results$T ~ results$Y+as.factor(results$M), xlab='Tipo', ylab="Tiempo", las=2, names=c('P-300','S-300','P-600','S-600','P-1200','S-1200'))
graphics.off()
png(paste("bxplt_P_TipoM.png",sep=""),width=960, height=480)
boxplot(results$P ~ results$Y+as.factor(results$M), xlab='Tipo', ylab="Porcentaje de aciertos", las=2, names=c('P-300','S-300','P-600','S-600','P-1200','S-1200'))
graphics.off()


datae1=read.csv("datos_Paralelo_T12_E1.csv", stringsAsFactors=F)
datae1$X=NULL
colnames(datae1)=c('R','M','PN','PG','PB','T','P')
result = datae1[datae1$PN==0,]
png(paste("bxplt_P_PSN.png",sep=""),width=960, height=480)
boxplot(P ~ as.factor(PN)+as.factor(PB)+as.factor(PG), las=2, data=result, names=c('NNN','NNB','NNM','NNA','NBN','NBB','NBM','NBA','NMN','NMB','NMM','NMA','NAN','NAB','NAM','NAA'), xlab='Tipo', ylab="Porcentaje")
graphics.off()
result = datae1[which(round(datae1$PN,digits=3)==0.333),]
png(paste("bxplt_P_PSB.png",sep=""),width=960, height=480)
boxplot(P ~ as.factor(PN)+as.factor(PB)+as.factor(PG), las=2, data=result, names=c('BNN','BNB','BNM','BNA','BBN','BBB','BBM','BBA','BMN','BMB','BMM','BMA','BAN','BAB','BAM','BAA'), xlab='Tipo', ylab="Porcentaje")
graphics.off()
result = datae1[which(round(datae1$PN,digits=3)==0.667),]
png(paste("bxplt_P_PSM.png",sep=""),width=960, height=480)
boxplot(P ~ as.factor(PN)+as.factor(PB)+as.factor(PG), las=2, data=result, names=c('MNN','MNB','MNM','MNA','MBN','MBB','MBM','MBA','MMN','MMB','MMM','MMA','MAN','MAB','MAM','MAA'), xlab='Tipo', ylab="Porcentaje")
graphics.off()
result = datae1[datae1$PN==1,]
png(paste("bxplt_P_PSA.png",sep=""),width=960, height=480)
boxplot(P ~ as.factor(PN)+as.factor(PB)+as.factor(PG), las=2, data=result, names=c('ANN','ANB','ANM','ANA','ABN','ABB','ABM','ABA','AMN','AMB','AMM','AMA','AAN','AAB','AAM','AAA'), xlab='Tipo', ylab="Porcentaje")
graphics.off()


datae2=read.csv("datos_Paralelo_T12_E2.csv", stringsAsFactors=F)
datae2$X=NULL
colnames(datae2)=c('R','M','E','T','P')
png(paste("bxplt_P_CHE5000.png",sep=""),width=960, height=480)
boxplot(datae2$P~datae2$M+datae2$E, xlab='Cantidad de Caracteres', ylab='Porcentaje de aciertos',las=2)
graphics.off()
