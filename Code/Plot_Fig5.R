# Script to Plot Figure 5: distribution of the slope parameter
##################################
rm(list = ls())
library(ggplot2); library(reshape2); 

#Load the models
load("./Output/cutoff12/mod3b_12.RData")
mod3_12 <- mod3b
load("./Output/cutoff12/mod5_12.RData")
mod5.U_12 <- mod5
load("./Output/cutoff12/mod5b_12.RData")
mod5.1_12 <- mod5b
load("./Output/cutoff12/mod5c_12.RData")
mod5.2_12 <- mod5c
load("./Output/cutoff12/mod5d_12.RData")
mod5.3_12 <- mod5d

load("./Output/cutoff16/mod3b_16.RData")
mod3_16 <- mod3b
load("./Output/cutoff16/mod5_16.RData")
mod5.U_16 <- mod5
load("./Output/cutoff16/mod5b_16.RData")
mod5.1_16 <- mod5b
load("./Output/cutoff16/mod5c_16.RData")
mod5.2_16 <- mod5c
load("./Output/cutoff16/mod5d_16.RData")
mod5.3_16 <- mod5d

load("./Output/cutoff24/mod3b_24.RData")
mod3_24 <- mod3b
load("./Output/cutoff24/mod5_24.RData")
mod5.U_24 <- mod5
load("./Output/cutoff24/mod5b_24.RData")
mod5.1_24 <- mod5b
load("./Output/cutoff24/mod5c_24.RData")
mod5.2_24 <- mod5c
load("./Output/cutoff24/mod5d_24.RData")
mod5.3_24 <- mod5d

# define some color options
cols3 <- c('#ffff00','#ffa600','#ff0000')
cols <- c('#ffff00','#90a646','#0000ff')
cols4 <- c('#ffff00','#ffc500','#ff8400','#ff0000')
cols4b <- c('#ffff00','#e0b189','#ab64c7','#0000ff')


#PLot as a fxn of testing error and titer threshold.
n <- mod5.1_12$BUGSoutput$n.sims
x <- data.frame(mod3_12 = mod3_12$BUGSoutput$sims.list$alpha[1:n],
								mod5.1_12= mod5.1_12$BUGSoutput$sims.list$alpha[1:n],
								mod3_16 = mod3_16$BUGSoutput$sims.list$alpha[1:n],
								mod5.1_16= mod5.1_16$BUGSoutput$sims.list$alpha[1:n],
								mod3_24 = mod3_24$BUGSoutput$sims.list$alpha[1:n],
								mod5.1_24= mod5.1_24$BUGSoutput$sims.list$alpha[1:n])

d <- melt(x)
names(d) <- c("model", "slope")

d$threshold <- "12"
d$threshold[d$model %in% c("mod3_12", "mod5.1_12")] <- 12
d$threshold[d$model %in% c("mod3_16", "mod5.1_16")] <- 16
d$threshold[d$model %in% c("mod3_24", "mod5.1_24")] <- 24
d$threshold <- as.factor(d$threshold)

d$test.error <- "testing error included"
d$test.error[d$model %in% c("mod3_12", "mod3_16","mod3_24")] <- "no testing error"
d$test.error <- as.factor(d$test.error)

#prior distribution
#x <- rnorm(n = 1000, mean = 0, sd = 2)
#slope.prior <- as.data.frame(x)
norm.fxn <- function(x) 1/(2*pi*2)^.5 * exp(-x^2/(2*2)) 

# Figure 5a
fig_5a <- ggplot(d, aes(x=slope, fill = threshold)) + 
	geom_density(alpha= 0.6) + 
	xlim(-.5,4.5) + xlab("Slope parameter") + 
	scale_fill_manual(values = cols3) + 
	theme_bw() + 
	facet_wrap(~test.error, nrow = 2) + 
	theme(text = element_text(size = 14),
				legend.position = c(.7,.78),
				panel.grid.minor = element_blank(),
				panel.grid.major = element_blank()) + 
	stat_function(fun = norm.fxn, color = "dark grey", size = 1.5)
fig_5a

##Fig 5b.
#effect of the prior on the slope parameter for titer = 16: 
x <- data.frame(mod5.U_16= mod5.U_16$BUGSoutput$sims.list$alpha,
								mod5.1_16= mod5.1_16$BUGSoutput$sims.list$alpha,
								mod5.2_16= mod5.2_16$BUGSoutput$sims.list$alpha,
								mod5.3_16= mod5.3_16$BUGSoutput$sims.list$alpha)
d <- melt(x)
names(d) <- c("model", "slope")

d$prior <- "5.U"
d$prior[d$model %in% c("mod5.1_16")] <- "5.1"
d$prior[d$model %in% c("mod5.2_16")] <- "5.2"
d$prior[d$model %in% c("mod5.3_16")] <- "5.3"

d$prior <- as.factor(d$prior)

fig_5b <- ggplot(d, aes(x = slope, fill = prior)) + 
	geom_density(alpha= 0.7) + 
	xlim(-.5,4.5) + xlab("Slope parameter") + 
	scale_fill_manual(values = cols4b) + 
	theme_bw() + 
	theme(text = element_text(size = 14),
				legend.position = c(.8,.6),
				panel.grid.minor = element_blank(),
				panel.grid.major = element_blank())
fig_5b
library(cowplot)

# combine figures
fig_5 <- ggdraw() +
	draw_plot(fig_5a, 0.02, .33, .98, .66) +
	draw_plot(fig_5b, 0, 0, 1, 0.33) +
	draw_plot_label(c("A", "B"), c(0.01, 0.01), c(1, 0.36), size = 15)

ggsave("./Figures/Fig5.png", width = 3, height = 7, units = "in", dpi = 600)

pdf(file = paste("./Figures/Fig5.pdf", sep = ""), 
		bg = "white", width = 3, height = 7)
fig_5
dev.off()

