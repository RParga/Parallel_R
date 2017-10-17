library(XML)
elisa = "http://elisa.dyndns-web.com/teaching/comp/par/resultados.html"
elisa.table = readHTMLTable(elisa, header=T, which=1, stringAsFactors=FALSE)
d = dim(elisa.table)
elisam=  as.matrix(elisa.table[,2:d[2]], ncol = dim(elisa.table)[2])
nombr = elisa.table[,1]
nombr = nombr[2:d[1]]
nombr = c(as.character(nombr),"Total")
results = numeric()
lt = d[2] +1
elisam[is.na(elisam)]= 0

for(i in 1:(d[2]-1)){
    if(all(elisam[2:d[1],i]==0||elisam[2:d[1],i]=='') && i<lt ){
        lt=i-1
    }
    av = as.numeric(elisam[2:d[1],i])
    av[is.na(av)]= 0
    av=c(av, mean(av))
    results = cbind(results, av)
}
results = data.frame(results)
colnames(results)=c(1:(d[2]-1) )

png(file="CajaBigotesXTarea.png", width = 960, height = 960, units = "px", pointsize = 20)
boxplot(results[,1:lt], main="Variaciones por tarea", xlab="Tarea", ylab="Puntos")
points(colMeans(results), pch=4)
abline(h=6.6,xpd=FALSE, col="red")
graphics.off()
#abline(h = 6.6, col = "red") 

png(file="CajaBigotesXPersona.png", width = 1200, height = 960, units = "px", pointsize = 20)
par(xpd = T, mar = par()$mar + c(0,0,0,7))
boxplot(t(results[,1:lt]), main="Variaciones por Persona", xlab="Persona", ylab="Puntos")
points(colMeans(t(results[,1:lt])), pch=4)
abline(h=6.6,xpd=FALSE, col="red")
legend(d[1]+4.5, 10, paste(1:d[1],nombr), cex = 0.8, )
graphics.off()


png(file="barplotXPersona.png", width = 1200, height = 960, units = "px", pointsize = 20)
par(xpd = T, mar = par()$mar + c(0,0,0,7))
barplot(rowSums(results[,1:lt]), main="Acumulado por persona", xlab="Persona", ylab="Puntos", ylim=c(0,150), names=1:d[1] )
abline(h=80,xpd=FALSE, col="green")
abline(h=50,xpd=FALSE, col="red")
abline(h=100,xpd=FALSE, col="blue")
legend(d[1]+4.5, 140, paste(1:d[1],nombr), cex = 0.8 )
graphics.off()


#boxplot(t(results[,1:lt]))
#abline(h = 6.6, col = "red") 
for(i in 1:(d[1]-1)){
    png(file=paste("plot_", nombr[i],".png", sep=""))
    plot(x=1:(lt), y=results[(d[1]),1:(lt)], ylim=c(0,10), xlim=c(1,d[2]), type='o', xlab="Tareas", ylab="puntos", col="green")
    lines(x=1:(lt), y=results[i,1:(lt)], ylim=c(0,10), xlim=c(0,d[2]), type='o', xlab="Tareas", ylab="puntos", col="blue", pch=4)
    abline(h=6.6,xpd=FALSE, col="red")
    legend("bottomright", legend = c("Promedio", as.character(nombr[i])), pch = c(1,4), col = c("green","blue"))
    graphics.off()
}
