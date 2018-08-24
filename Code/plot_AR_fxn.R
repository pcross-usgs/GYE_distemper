plot_AR <- function(mod){

mod.pars<- mod$BUGSoutput$sims.list
#get mean and mode of AR(1)  and AR(2) coefficents
bear.AR1mean<- apply(mod.pars$betaLL.b1,2,"mean")
bear.AR1mode<- apply(mod.pars$betaLL.b1,2,"quantile",probs=0.5)
bear.AR2mean<- apply(mod.pars$betaLL.b2,2,"mean")
bear.AR2mode<- apply(mod.pars$betaLL.b2,2,"quantile",probs=0.5)

wolf.AR1mean<- apply(mod.pars$betaLL.w1,2,"mean")
wolf.AR1mode<- apply(mod.pars$betaLL.w1,2,"quantile",probs=0.5)
wolf.AR2mean<- apply(mod.pars$betaLL.w2,2,"mean")
wolf.AR2mode<- apply(mod.pars$betaLL.w2,2,"quantile",probs=0.5)

#par(mfrow=c(1,1),mar=c(2.4,2.2,0.5,0.5), mgp=c(1.2,0.3,0), cex.axis=0.7, cex.lab=0.7, tck=-0.03, col.axis='grey10', col.lab='grey10')

plot(1,1, xlim=c(-3,3), ylim=c(-3,3), yaxt='n', col='white', 
		 xlab='AR(1)', ylab='AR(2)', main='' )
#add AR triangle
segments(-2,-1,0,1, col='grey10', lwd=1)
segments(0,1,2,-1, col='grey10', lwd=1)
segments(-2,-1,2,-1, col='grey10', lwd=1)
axis(2, c('-1.0','0.0','1.0'), at=c(-1.0,0.0,1.0) )
#legend(1.5,1.0, c('wolf','bear'), pch=16, cex=1.1, col=c('blue','black'), bty='n')

#add AR in bear
points(mod.pars$betaLL.b1, mod.pars$betaLL.b2, col='red', cex=0.3)
points(bear.AR1mean, bear.AR2mean, col='black', pch=16, cex=1.5)
#add AR in wolf
points(mod.pars$betaLL.w1, mod.pars$betaLL.w2, col='blue', cex=0.3)
points(wolf.AR1mean, wolf.AR2mean, col='black', pch=16, cex=1.5)

#add semi-circle with periodicity contours
x<- seq(-2,2,0.01)
curve(-0.25*x^2, from=-2, to=2, col="grey10", lwd=0.6, add=T)
cycleperiod<- c(3, 5:80) #periodicity
w= 2*pi/cycleperiod
for(i in 1){curve(-0.25 * x^2 * (tan(w[i])^2 + 1), -sqrt(1 / (0.25 * (tan(w[i])^2 + 1))), 0, add= T, lwd=0.6, col='grey30')}
for(i in 2:70){curve(-0.25 * x^2 * (tan(w[i])^2 + 1), 0, sqrt(1 / (0.25 * (tan(w[i])^2 + 1))), add= T, lwd=0.6, col='grey30')}
segments(0,0,0,-1, col='grey40', lwd=0.8) #make the w=4 line.
text(x= c(-1.34, -0.65, 0.05, 0.47, 0.73, 1.04), y= c(-0.516, -0.516, -0.516, -0.516, -0.516, -0.516), labels= c(2:6,8), col='grey40', cex=1)

legend('topright', c('Bears','Wolves'), col=c('grey50', 'blue'), lty=1, lwd=1.5, bty='n')
}
