# script to plot the annual hazards across different thresholds
rm(list = ls())
source("./Code/plot_haz_fxn.R")
load("./Output/cutoff12/mod5b_12.RData")
mod5b_12 <- mod5b
load("./Output/cutoff16/mod5b_16.RData")
mod5b_16 <- mod5b
load("./Output/cutoff24/mod5b_24.RData")
mod5b_24 <- mod5b

png(filename = paste("./Figures/Fig4.png", sep = ""),
		bg = "white", width = 600*1.5, height = 1100*1.5, pointsize = 26, units = "px")
par(mfrow = c(3,1), mar = c(5,5,3,3))
plot_haz_3plot(mod5b_12, "Prob.yr.b", "Prob.yr.w", ylabs = "", leg = F, 
							 figtext = "a) titer threshold = 12")
plot_haz_3plot(mod5b_16, "Prob.yr.b", "Prob.yr.w", ylabs = "Probability of CDV", leg = F,
							 figtext = "b) titer threshold = 16")
plot_haz_3plot(mod5b_24, "Prob.yr.b", "Prob.yr.w", ylabs = "", leg = T,
							 figtext = "c) titer threshold = 24")
dev.off()

pdf(file = "./Figures/Fig4.pdf",	width = 6, height = 8)
par(mfrow = c(3,1), mar = c(5,5,2,2))
plot_haz_3plot(mod5b_12, "Prob.yr.b", "Prob.yr.w", ylabs = "", leg = F, 
							 figtext = "a) titer threshold = 12")
plot_haz_3plot(mod5b_16, "Prob.yr.b", "Prob.yr.w", ylabs = "Probability of CDV", leg = F,
							 figtext = "b) titer threshold = 16")
plot_haz_3plot(mod5b_24, "Prob.yr.b", "Prob.yr.w", ylabs = "", leg = T,
							 figtext = "c) titer threshold = 24")
dev.off()

