# Plot figure s4 (like Fig 5, but with different prior distributions)
rm(list = ls())
library(ggplot2)
library(reshape2)
library(cowplot)

#define colors
cols2 <- c('#ffff00','#ffc500','#ff8400','#ff0000')

#Load the models
load("./Output/cutoff12/mod5d_12.RData")
mod5.3_12 <- mod5d
load("./Output/cutoff16/mod5d_16.RData")
mod5.3_16 <- mod5d
load("./Output/cutoff24/mod5d_24.RData")
mod5.3_24 <- mod5d

n <- mod5.3_16$BUGSoutput$n.sims
pos <- data.frame(q.pos.bear.12 = mod5.3_12$BUGSoutput$sims.list$qpos.bears,
									q.pos.bear.16 = mod5.3_16$BUGSoutput$sims.list$qpos.bears,
									q.pos.bear.24 = mod5.3_24$BUGSoutput$sims.list$qpos.bears,
									q.pos.wolf.12 = mod5.3_12$BUGSoutput$sims.list$qpos.wolves,
									q.pos.wolf.16 = mod5.3_16$BUGSoutput$sims.list$qpos.wolves,
									q.pos.wolf.24 = mod5.3_24$BUGSoutput$sims.list$qpos.wolves,
									q.pos.prior = rbeta(n, 10, 0.5))

neg <- data.frame(q.neg.bear.12 = mod5.3_12$BUGSoutput$sims.list$qneg.bears,
									q.neg.wolf.12 = mod5.3_12$BUGSoutput$sims.list$qneg.wolves, 
									q.neg.bear.16 = mod5.3_16$BUGSoutput$sims.list$qneg.bears,
									q.neg.wolf.16 = mod5.3_16$BUGSoutput$sims.list$qneg.wolves, 
									q.neg.bear.24 = mod5.3_24$BUGSoutput$sims.list$qneg.bears,
									q.neg.wolf.24 = mod5.3_24$BUGSoutput$sims.list$qneg.wolves, 
									q.neg.prior = rbeta(n, 10, 0.5))

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

plot1 <- ggplot(pos.m, aes(x = q.pos, fill = threshold)) + 
	geom_density(alpha=0.6) +
	xlab(bquote("Sensitivity"~(q^"+")~"")) +
	theme_bw() + facet_grid(~spp) + 
	xlim(0.5, .999) + 
	theme(legend.position = c(.13, .68), text = element_text(size = 16),
				panel.grid.minor = element_blank(),
				panel.grid.major = element_blank()) + 
	scale_fill_manual(values = cols2)  
plot1

ggsave("./Figures/FigS4_top.png", width = 8, height = 4, 
			 unit = "in", dpi = 600)

pdf(file = paste("./Figures/FigS4_top.pdf", sep = ""), 
		bg = "white", width = 7, height = 4)
plot1
dev.off()

plot2 <- ggplot(neg.m, aes(x = q.neg, fill = threshold)) +
	geom_density(alpha=0.6) +
	xlab(bquote('Specificity'~(q^'-')~"")) +
	theme_bw() + facet_grid(~spp) + 
	xlim(0.65, .999) + 
	theme(legend.position = c(.13, .68), text = element_text(size = 16),
				panel.grid.minor = element_blank(),
				panel.grid.major = element_blank()) + 
	scale_fill_manual(values = cols2)  
plot2

ggsave("./Figures/FigS4_bottom.png", width = 8, height = 4, 
			 unit = "in", dpi = 600)

pdf(file = paste("./Figures/FigS4_bottom.pdf"), 
		bg = "white", width = 7, height = 4)
plot2
dev.off()

# model 5b vs. 4
load("./Output/cutoff16/mod4_16.RData")
n <- mod5b_16$BUGSoutput$n.sims
neg <- data.frame(q.neg.bear.mod5 = mod5.1_16$BUGSoutput$sims.list$qneg.bears,
									q.neg.bear.mod4 = mod4$BUGSoutput$sims.list$qneg.bears,
									q.neg.prior = rbeta(n, 25, 0.5))

neg.m <- melt(neg)

ggplot(neg.m, aes(x = value, fill = variable)) + geom_density(alpha=0.25)
