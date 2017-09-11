runs <- 100000
#runif samples from a uniform distribution
xs <- runif(runs,min=-0.5,max=0.5)
ys <- runif(runs,min=-0.5,max=0.5)
in.circle <- xs^2 + ys^2 <= 0.5^2
mc.pi <- (sum(in.circle)/runs)*4
png("pi.png")
plot(xs,ys,pch='.',col=ifelse(in.circle,"red","grey")
     ,xlab="",ylab="",asp=1,
     main=paste("Aproximación de Pi =",mc.pi))
graphics.off()
