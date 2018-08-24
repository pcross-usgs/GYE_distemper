#Script to plot Figure 6
rm(list = ls())
library(ggplot2)
library(reshape2)
library(cowplot)

#define colors
cols2 <- c('#ffff00','#ffc500','#ff8400','#ff0000')

#Load the models
load("./Output/cutoff12/mod5b_12.RData")
mod5.1_12 <- mod5b
load("./Output/cutoff16/mod5b_16.RData")
mod5.1_16 <- mod5b
load("./Output/cutoff24/mod5b_24.RData")
mod5.1_24 <- mod5b

n <- mod5.1_16$BUGSoutput$n.sims
pos <- data.frame(q.pos.bear.12 = mod5.1_12$BUGSoutput$sims.list$qpos.bears,
									q.pos.bear.16 = mod5.1_16$BUGSoutput$sims.list$qpos.bears,
									q.pos.bear.24 = mod5.1_24$BUGSoutput$sims.list$qpos.bears,
									q.pos.wolf.12 = mod5.1_12$BUGSoutput$sims.list$qpos.wolves,
									q.pos.wolf.16 = mod5.1_16$BUGSoutput$sims.list$qpos.wolves,
									q.pos.wolf.24 = mod5.1_24$BUGSoutput$sims.list$qpos.wolves,
									q.pos.prior = rbeta(n, 25, 0.5))

neg <- data.frame(q.neg.bear.12 = mod5.1_12$BUGSoutput$sims.list$qneg.bears,
									q.neg.wolf.12 = mod5.1_12$BUGSoutput$sims.list$qneg.wolves, 
									q.neg.bear.16 = mod5.1_16$BUGSoutput$sims.list$qneg.bears,
									q.neg.wolf.16 = mod5.1_16$BUGSoutput$sims.list$qneg.wolves, 
									q.neg.bear.24 = mod5.1_24$BUGSoutput$sims.list$qneg.bears,
									q.neg.wolf.24 = mod5.1_24$BUGSoutput$sims.list$qneg.wolves, 
									q.neg.prior = rbeta(n, 25, 0.5))

pos.m <- melt(pos)
neg.m <- melt(neg)
names(pos.m) <- c("model", "q.pos")
names(neg.m) <- c("model", "q.neg")

pos.m$spp <- "prior"
pos.m$spp[pos.m$model %in% c("q.pos.wolf.12", "q.pos.wolf.16", "q.pos.wolf.24")] <- "wolf"
pos.m$spp[pos.m$model %in% c("q.pos.bear.12", "q.pos.bear.16", "q.pos.bear.24")] <- "bear"
pos.m$spp <- as.factor(pos.m$spp)

pos.m$threshold <- "prior"
pos.m$threshold[pos.m$model %in% c("q.pos.wolf.12", "q.pos.bear.12")] <- 12
pos.m$threshold[pos.m$model %in% c("q.pos.wolf.16", "q.pos.bear.16")] <- 16
pos.m$threshold[pos.m$model %in% c("q.pos.wolf.24", "q.pos.bear.24")] <- 24
pos.m$threshold <- as.factor(pos.m$threshold)

neg.m$spp <- "prior"
neg.m$spp[neg.m$model %in% c("q.neg.wolf.12", "q.neg.wolf.16", "q.neg.wolf.24")] <- "wolf"
neg.m$spp[neg.m$model %in% c("q.neg.bear.12", "q.neg.bear.16", "q.neg.bear.24")] <- "bear"
neg.m$spp <- as.factor(neg.m$spp)

neg.m$threshold <- "prior"
neg.m$threshold[neg.m$model %in% c("q.neg.wolf.12", "q.neg.bear.12")] <- 12
neg.m$threshold[neg.m$model %in% c("q.neg.wolf.16", "q.neg.bear.16")] <- 16
neg.m$threshold[neg.m$model %in% c("q.neg.wolf.24", "q.neg.bear.24")] <- 24
neg.m$threshold <- as.factor(neg.m$threshold)

fig_6a <- ggplot(pos.m, aes(x = q.pos, fill = threshold)) + 
	geom_density(alpha=0.6) +
	xlab(bquote("Sensitivity"~(q^"+")~"")) +
	xlim(0.65, .999) + 
	theme_bw() + facet_grid(~spp) + 
	theme(legend.position = c(.13, .68), text = element_text(size = 16),
				panel.grid.minor = element_blank(),
				panel.grid.major = element_blank()) + 
	scale_fill_manual(values = cols2)  
fig_6a


fig_6b <- ggplot(neg.m, aes(x = q.neg, fill = threshold)) +
	geom_density(alpha=0.8) +
	xlab(bquote('Specificity'~(q^'-')~"")) +
	xlim(0.65, .999) + 
	theme_bw() + facet_grid(~spp) + 
	theme(legend.position = c(.13, .68), text = element_text(size = 16),
				panel.grid.minor = element_blank(),
				panel.grid.major = element_blank()) + 
	scale_fill_manual(values = cols2)  
fig_6b

# put them together
fig_6 <- plot_grid(fig_6a, fig_6b, 
									 labels = c("A", "B"), nrow = 2, align = "v")

fig_6
ggsave("./Figures/Fig6.png", width = 8, height = 8, units = "in", dpi = 600)

pdf(file = paste("./Figures/Fig6.pdf", sep = ""), 
		bg = "white", width = 8, height = 8)
fig_6
dev.off()

