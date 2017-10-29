modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- 0.995 # pixeles negros en plantillas
modelos[modelos=='g'] <- 0.92 # pixeles grises en plantillas
modelos[modelos=='b'] <- 0.002 # pixeles blancos en plantillas
 
r <- 5 # alto
c <- 3 # ancho
dim <- r * c #dimension
 
n <- 49 # cantiadd de 
w <- ceiling(sqrt(n)) # pesos
h <- ceiling(n / w) #
 
png("p12g.png", width=1600, height=2000)
par(mfrow=c(w, h), mar = c(0,0,7,0))
suppressMessages(library("sna"))
 
for (j in 1:n) {
    d <- sample(0:9, 1) #obtiene un número al azar entre 0 y 9
    pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
    imagen <- matrix(pixeles, nrow=r, ncol=c, byrow=TRUE)
    plot.sociomatrix(imagen, drawlab=FALSE, diaglab=FALSE, 
                     main=paste(d, ""), cex.main=5)
}
graphics.off()
