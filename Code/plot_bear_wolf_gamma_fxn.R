plot_bear_wolf_gamma <- function(mod, xlabel, ylabel){
	
	# input must be a bugs model with annual bear and wolf probabilities for 44 years
	# names Prob.yr.b and Prob.yr.w
	# xlabel is the text to put in the xlab, insert "" to keep it empty
	#ylabel is the text to put in the ylab, insert "" to keep it empty
	
	mod.pars <- mod$BUGSoutput$sims.list
	mean.b <- mod$BUGSoutput$mean$gamma.b
	mean.w <- mod$BUGSoutput$mean$gamma.w
	
	nyear <- length(mean.b)
	b.ci <- apply(mod.pars$gamma.b,2,"quantile",probs=c(0.025,0.25,0.75,0.975))
	w.ci <- apply(mod.pars$gamma.w,2,"quantile",probs=c(0.025,0.25,0.75,0.975))
	
	plot(mean.w[26:(nyear-1)], mean.b[26:(nyear-1)], 
			 cex.lab = 1.25, cex.axis = 1.25, ylim = c(-12, -1), xlim = c(-12,-1),
			 xlab = xlabel, ylab = ylabel)
	for(i in 26:(nyear-1)){
		lines(c(mean.w[i], mean.w[i]), c(b.ci[1,i], b.ci[4,i]))
		lines(c(w.ci[1,i], w.ci[4,i]), c(mean.b[i], mean.b[i]))
		lines(c(mean.w[i], mean.w[i]), c(b.ci[2,i], b.ci[3,i]), lwd = 3)
		lines(c(w.ci[2,i], w.ci[3,i]), c(mean.b[i], mean.b[i]), lwd = 3)
	}
	points(mean.w[26:(nyear-1)], mean.b[26:(nyear-1)], pch = 21, cex = 1.25, 
				 col = "black", bg = "grey")
}# end