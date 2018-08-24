plot_haz_3plot <- function(mod, P1, P2, ylabs, leg, figtext){
	#Input: output from an R2jags model
	#mod = model output
	#p1 = parameter 1
	#p2 = parameter 2
	#ylabs = label for the y axis
	#leg = T/F for including the legend
	# fig text = title text
	#OUTPUT: plot of the hazards over time.
	
	if(missing(ylabs)){ylabs = "Probability of CDV"}
	
	nyear <- dim(mod$BUGSoutput$sims.list[[P1]])[2]
	
	#calculate CIs
	ci95b <- apply(mod$BUGSoutput$sims.list[[P1]], 2, FUN = quantile,
								 probs = c(0.025, 0.975))
	ci95w <- apply(mod$BUGSoutput$sims.list[[P2]], 2, FUN = quantile,
								 probs = c(0.025, 0.975))
	
	ci50b <- apply(mod$BUGSoutput$sims.list[[P1]], 2, FUN = quantile,
								 probs = c(0.25, 0.75))
	ci50w <- apply(mod$BUGSoutput$sims.list[[P2]], 2, FUN = quantile,
								 probs = c(0.25, 0.75))
	par(mar = c(2,5,1,2))
	plot(1:nyear, unlist(mod$BUGSoutput$mean[P2]), type="p", pch = 21,
			 col  = "grey", ylim = c(0, 1), 
			 bty = 'n', 
			 ylab = ylabs,
			 cex.lab = 1.5,
			 xlab = "", axes = F)
	
	axis(1, 1:nyear, labels = F, tck = -0.01)
	axis(1, seq(5, 45, 5), (1970:2015) [seq(6,46,5)], 
			 lwd.ticks = 1, tck=-0.03, cex.axis = 1.5)
	axis(2, seq(0,1,0.2), cex.axis = 1.5)
	for(i in 1:nyear){lines(c(i,i), c(ci95w[1,i], ci95w[2,i]), col = "gray")}
	for(i in 1:nyear){lines(c(i,i), c(ci50w[1,i], ci50w[2,i]), lwd = 3, col = "grey")}
	for(i in 1:nyear){lines(c(i+0.3, i+0.3), c(ci95b[1,i], ci95b[2,i]), col = "black")}
	for(i in 1:nyear){lines(c(i+0.3, i+0.3), c(ci50b[1,i], ci50b[2,i]), lwd = 3, col = "black")}
	
	points(c(seq(1:nyear) + 0.3), unlist(mod$BUGSoutput$mean[P1]), pch = 21,
				 col  = "black", bg = "black")
	
	points(c(seq(1:nyear)), unlist(mod$BUGSoutput$mean[P2]), pch = 21,
				 col  = "grey", bg = "grey")
	if(missing(figtext)==F){text(x = .5, y = 1, pos = 4, cex = 1.5, labels = figtext)}
	if(leg == T){
		legend(x = 1.5, y = .9, c('Wolves', 'Grizzly bears'),
				 lwd = c(3,3), col = c('grey', 'black'), bty = 'n', 
				 cex = 1.2, xjust = 0)
		}
}


plot_haz <- function(mod, P1, P2){
	#Input: output from an R2jags model
	#mod = model output
	#p1 = parameter 1
	#p2 = parameter 2

	#OUTPUT: plot of the hazards over time.
	nyear <- dim(mod$BUGSoutput$sims.list[[P1]])[2]

	#calculate CIs
	ci95b <- apply(mod$BUGSoutput$sims.list[[P1]], 2, FUN = quantile,
							 probs = c(0.025, 0.975))
	ci95w <- apply(mod$BUGSoutput$sims.list[[P2]], 2, FUN = quantile,
							 probs = c(0.025, 0.975))

	ci50b <- apply(mod$BUGSoutput$sims.list[[P1]], 2, FUN = quantile,
	             probs = c(0.25, 0.75))
	ci50w <- apply(mod$BUGSoutput$sims.list[[P2]], 2, FUN = quantile,
	             probs = c(0.25, 0.75))
	par(mar = c(3,5,1,2))
	plot(1:nyear, unlist(mod$BUGSoutput$mean[P2]), type="p", pch = 21,
	     col  = "grey", ylim = c(0, 1), 
			 bty = 'n', 
			 ylab = "Probability of CDV",
			 cex.lab = 1.25,
	     xlab = "", axes = F)

	axis(1, 1:nyear, labels = F, tck = -0.01)
	axis(1, seq(5, 45, 5), (1970:2015) [seq(6,46,5)], 
			 lwd.ticks = 1, tck=-0.03, cex.axis = 1.25)
	axis(2, seq(0,1,0.2), cex.axis = 1.25)
	for(i in 1:nyear){lines(c(i,i), c(ci95w[1,i], ci95w[2,i]), col = "gray")}
	for(i in 1:nyear){lines(c(i,i), c(ci50w[1,i], ci50w[2,i]), lwd = 3, col = "grey")}
	for(i in 1:nyear){lines(c(i+0.3, i+0.3), c(ci95b[1,i], ci95b[2,i]), col = "black")}
	for(i in 1:nyear){lines(c(i+0.3, i+0.3), c(ci50b[1,i], ci50b[2,i]), lwd = 3, col = "black")}

  points(c(seq(1:nyear) + 0.3), unlist(mod$BUGSoutput$mean[P1]), pch = 21,
         col  = "black", bg = "black")
  points(c(seq(1:nyear)), unlist(mod$BUGSoutput$mean[P2]), pch = 21,
         col  = "grey", bg = "grey")
  legend(x = 1.25, y = 1, c('Wolves', 'Grizzly bears'),
  			 lwd = c(3,3), col = c('grey', 'black'), bty = 'n', 
  			 cex = 1.2, xjust = 0)
}

# for 1 hazard
plot_haz1 <- function(mod, P1){
  #Input: output from an jags model
  #mod = model output
  #p1 = parameter 1

  #OUTPUT: plot of the hazards over time.
  nyear <- dim(mod$BUGSoutput$sims.list[[P1]])[2]

  #calculate CIs
  ci <- apply(mod$BUGSoutput$sims.list[[P1]], 2, FUN = quantile,
               probs = c(0.025, 0.975))
  ci50 <- apply(mod$BUGSoutput$sims.list[[P1]], 2, FUN = quantile,
                 probs = c(0.25, 0.75))

  plot(1:nyear, unlist(mod$BUGSoutput$mean[P1]), type = "p", pch = 21,
       col  = "red", bg = "grey", axes = F,
       ylim = c(0,1), ylab = "Probability of CDV",
       xlab ="")
  axis(1, 1:nyear, labels = F)
  axis(1, seq(5, 45, 5), (1970:2015) [seq(6,46,5)], lwd.ticks = 1, tck=-0.03)
  axis(2, seq(0,1,0.2))
  for(i in 1:nyear){lines(c(i,i), c(ci[1,i], ci[2,i]), col = "dark grey")}
  for(i in 1:nyear){lines(c(i,i), c(ci50[1,i], ci50[2,i]), lwd = 3, 
  												col = "black")}
  points(c(seq(1:nyear)), unlist(mod$BUGSoutput$mean[P1]), pch = 21,
         col  = "black", bg = "grey")
}


# Only plot the wolf hazards
plot_haz_wolf <- function(mod, P1){
	#Input: output from an jags model
	#mod = model output
	#p1 = parameter 1
	
	#OUTPUT: plot of the hazards over time.
	nyear <- dim(mod$BUGSoutput$sims.list[[P1]])[2]
	
	#calculate CIs
	ci <- apply(mod$BUGSoutput$sims.list[[P1]], 2, FUN = quantile,
							probs = c(0.025, 0.975))
	ci50 <- apply(mod$BUGSoutput$sims.list[[P1]], 2, FUN = quantile,
								probs = c(0.25, 0.75))
	
	plot(1:nyear, unlist(mod$BUGSoutput$mean[P1]), type = "p", pch = 21,
			 col  = "black", bg = "grey", axes = F,
			 ylim = c(0,1), xlim = c(27, 44), 
			 ylab = "Probability of CDV",
			 xlab ="", cex.lab = 1.5)
	
	axis(1, 27:nyear, labels = F, tck = -0.01)
	axis(1, at = seq(30, 44, 5), labels = seq(2000, 2010, 5),
			 lwd.ticks = 1, tck=-0.03, cex.axis = 1.5)
	axis(2, seq(0, 1, 0.2), cex.axis = 1.5)
	
	for(i in 1:nyear){lines(c(i,i), c(ci[1,i], ci[2,i]), col = "dark grey")}
	for(i in 1:nyear){lines(c(i,i), c(ci50[1,i], ci50[2,i]), lwd = 3, 
													col = "black")}
	points(c(seq(1:nyear)), unlist(mod$BUGSoutput$mean[P1]), pch = 21,
				 col  = "black", bg = "grey")

}
