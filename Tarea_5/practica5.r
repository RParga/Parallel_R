inicio <- -6
final <- -inicio
paso <- 0.25
x <- seq(inicio, final, paso)
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
png("fx.png")
plot(x, f(x), type="l")
graphics.off()
png("p5f.png") # dibujamos f(x) para ver como es
#plot(x,  (2/pi) * (1/(exp(x)+exp(-x))))
#lines(x,  (2/pi) * (1/(exp(x)+exp(-x))), type="l")
plot(x,  (2/pi) * f(x))
lines(x,  (2/pi) * f(x), type="l")
graphics.off()
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador
muestra <- generador(50000) # sacamos una muestra
png("p5m.png") # validamos con un dibujo
hist(muestra, freq=F, breaks=50,
     main="Histograma de g(x) comparado con g(x)",
     xlim=c(inicio, final), ylim=c(0, 0.4))
lines(x, g(x), col="red") # dibujamos g(x) encima del histograma
graphics.off()
desde <- 3
hasta <- 7
pedazo <- 50000
cuantos <- 500
valE = 0.04883411112604931084064237
parte <- function(x) {
    valores <- generador(pedazo)
    return(sum(valores >= desde & valores <= hasta))
}
suppressMessages(library(parallel))
cluster = makeCluster(detectCores(logical=FALSE) - 1)
#registerDoParallel(makeCluster(detectCores() - 1))
clusterExport(cluster, "pedazo")
clusterExport(cluster, "desde")
clusterExport(cluster, "hasta")
clusterExport(cluster, "generador")
    #montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
tim = proc.time()
montecarlo = parSapply(cluster, 1:cuantos, parte)
    #stopImplicitCluster()
integral <- sum(montecarlo) / (cuantos * pedazo)
    #print(montecarlo)
    #print(sum(montecarlo))
val = (pi / 2) * integral
tim = proc.time() - tim
print(tim)
print(val)
print(valE-val)
stopCluster(cluster)

