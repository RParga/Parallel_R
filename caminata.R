caminata = function(dur, fndist, dim=2)
{   # fndist es uan funcion usada como parametro	 
    pos = rep(0, dim)
    mayor = 0
    for (t in 1:dur) {
        cambiar = sample(1:dim, 1)
        cambio = 1
        if (runif(1) < 0.5) {
            cambio = -1
        }
        pos[cambiar] = pos[cambiar] + cambio
        d = fndist(pos)
        if (d > mayor) {
            mayor = d
        }
    }
    return(c(d,mayor))
}