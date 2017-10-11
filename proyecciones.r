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

for(i in 1:d[2]){
    if(all(elisam[2:d[1],i]==0) && i<lt ){
        lt=i-1
    }
    av = as.numeric(elisam[2:d[1],i])
    av[is.na(av)]= 0
    av=c(av, mean(av))
    results = cbind(results, av)
}
results = data.frame(results)
colnames(results)=c(1:(d[2]-1) )

png(file="CajaBigotesXTarea.png")
boxplot(results, main="Variaciones por tarea", xlab="Tarea", ylab="Puntos")
points(colMeans(results), pch=4)
graphics.off()
#abline(h = 6.6, col = "red") 

png(file="CajaBigotesXPersona.png")
par(xpd = T, mar = par()$mar + c(0,0,0,7))
boxplot(t(results[,1:lt]), main="Variaciones por Persona", xlab="Persona", ylab="Puntos")
points(colMeans(t(results[,1:lt])), pch=4)
legend(20.5, 10, paste(1:d[1],nombr), cex = 0.8, )
graphics.off()

png(file="histogramaXPersona.png")
plot(rowSums(results), main="Acumulado por persona", xlab="Persona", ylab="Puntos", xlim=c(1,19))
graphics.off()


#boxplot(t(results[,1:lt]))
#abline(h = 6.6, col = "red") 
for(i in 1:(d[1]-1)){
    png(file=paste("plot_", nombr[i],".png", sep=""))
    plot(x=1:(lt), y=results[(d[1]),1:(lt)], ylim=c(0,10), xlim=c(1,d[2]), type='o', xlab="Tareas", ylab="puntos", col="green")
    lines(x=1:(lt), y=results[i,1:(lt)], ylim=c(0,10), xlim=c(0,d[2]), type='o', xlab="Tareas", ylab="puntos", col="blue", pch=4)
    legend("bottomright", legend = c("Promedio", as.character(nombr[i])), pch = c(1,4), col = c("green","blue"))
    graphics.off()
}
