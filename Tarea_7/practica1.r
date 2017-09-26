f <- function(x) {
    return(cos(14.5*x - 0.3) + x * (x + 0.2) + 1.01)
}
x <- seq(-3, 3, 0.05)
png("p7_1d.png", width=500, height=400)
plot(x, f(x), type="l")
graphics.off()
g <- function(x, y) {
    return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y))
}
png("p7_2d.png", width=700, height=700)
x <- seq(-6, 5, abs(-6-5)/50)
y <-  x
z <- outer(x, y, g)
persp(x, y, z, shade=0.2, col='deepskyblue', theta=40, phi=30)
graphics.off()
png("p7_flat_1.png", width=500, height=500)
image(z)
graphics.off()

x <- seq(-6, 5, abs(-6-5)/500)
y <-  x
z <- outer(x, y, g)
dimnames(z) <- list(x, y)
library(reshape2) # recuerda instalar paquetes antes de intentar su uso
d <- melt(z)
names(d) <- c("x", "y", "z")
library(lattice) # lo mismo aplica con este paquete
titl = paste("")
png("p7_flat_2.png", width=1000, height=1000)
levelplot(z ~ x * y, data = d,xlab = list(label="Coordenada X", cex=2), ylab = list(label="Coordenada Y", cex=2), main=list(label=titl, cex=2), col.regions=rev(cm.colors(100)), scales=list(x=list(cex=2),y=list(cex=2)), colorkey=list(labels=list(cex=2)) )
graphics.off()
