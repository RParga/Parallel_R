puntos = 10
n = 1.5
umbral = 0.5
datos = data.frame(x=runif(puntos,0,n),y=runif(puntos,0,n), estado=rep(0,n))
ordatos = datos[order(datos$y),]
print(ordatos)
cercanos= data.frame(i=numeric(),x=numeric(),y=numeric(), estado=numeric())
for(i in 1:n){
    ap = cbind(i,ordatos[i,])
    if(length(cercanos) > 0){
        cercanos= cercanos[which(cercanos$y<ap$y-umbral)]
        d = ((rep(ap$x, length(cercanos))-cercanos$x)**2)+((rep(ap$y,length(cercanos))-cercanos$y)**2)        
        ordatos[cercanos[which(d<umbral)]$i] = 1 #calcular prob enf
    }
    rbind(cercanos,ap)
}
print(ordatos)
