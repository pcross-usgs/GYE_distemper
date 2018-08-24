# Plot Figure 2 and some supplemental figures. 

# Load all the model files in the Output folder
library(ggplot2)
library(reshape2)
library(rjags)
library(R2WinBUGS)

#load in results
cutoff <- 16
models <- list.files(paste("./Output/cutoff", cutoff, "/", sep = ""))
lapply(paste("./Output/cutoff", cutoff, "/", models, sep = ""), load, .GlobalEnv)
load(paste("./Data/Wolf_Bear_", cutoff, ".RData", sep = ""))

#load functions
source("./Code/WriteJAGSmodels_paper.R")
source("./Code/plot_haz_fxn.R")
source("./Code/plot_bear_wolf_haz_fxn.R")
source("./Code/plot_bear_wolf_gamma_fxn.R")
source("./Code/plot_interval_fxn.r")

#define colors
cols3 <- c('#ffff00','#ffa600','#ff0000')
cols3_gb <- c('#808080','#3f3f3f','#000000')
cols5 <- c('#ffff00','#ffd300','#ffa600','#ff7100','#ff0000')

#plot Figure S3
png(filename = paste("./Figures/FigS3_", cutoff, ".png", sep = ""),
		bg = "white", width = 800, height = 800, pointsize = 22, units = "px")
par(mfrow = c(2,2))
par(mar = c(4,5,1,1))
plot_bear_wolf_gamma(mod3b, xlabel = "Wolf log hazard", ylabel = "Bear log hazard")
par(mar = c(4,5,1,1))
plot_bear_wolf_gamma(mod5b, xlabel = "Wolf log hazard", ylabel = "")
par(mar = c(4,5,1,1))
plot_bear_wolf(mod3b, xlabel = "Wolf probability", ylabel = "Bear probability")
par(mar = c(4,5,1,1))
plot_bear_wolf(mod5b, xlabel = "Wolf probability", ylabel = "")
dev.off()

# FIGURE 3: 
# Prevalence 
load("./Data/CDV_all.RData") #all data
CDV_all$disease12 <- "N"
CDV_all$disease12[CDV_all$titer >= 12] <- "P"
CDV_all$disease12 <- as.factor(CDV_all$disease12)

CDV_all$disease16 <- "N"
CDV_all$disease16[CDV_all$titer >= 16] <- "P"
CDV_all$disease16 <- as.factor(CDV_all$disease16)

CDV_all$disease24 <- "N"
CDV_all$disease24[CDV_all$titer >= 24] <- "P"
CDV_all$disease24 <- as.factor(CDV_all$disease24)

# reshape and calculate the cis
tmp12 <- as.data.frame(table(CDV_all$disease12, CDV_all$sex, CDV_all$spp))
names(tmp12) <- c("distemper", "sex", "spp", "N")
tmp12 <- dcast(tmp12, spp + sex ~ distemper, sum)
tmp12$prev <- tmp12$P/(tmp12$N + tmp12$P)
tmp12$lci <- 0
tmp12$hci <- 0

for(i in 1:dim(tmp12)[1]){
	cis <- binom.test(tmp12$P[i], tmp12$N[i] + tmp12$P[i])$conf.int
	tmp12$lci[i] <- cis[1]
	tmp12$hci[i] <- cis[2]
}

tmp16 <- as.data.frame(table(CDV_all$disease16, CDV_all$sex, CDV_all$spp))
names(tmp16) <- c("distemper", "sex", "spp", "N")

# reshape and calculate the cis
tmp16 <- dcast(tmp16, spp + sex ~ distemper, sum)
tmp16$prev <- tmp16$P/(tmp16$N + tmp16$P)
tmp16$lci <- 0
tmp16$hci <- 0

for(i in 1:dim(tmp16)[1]){
	cis <- binom.test(tmp16$P[i], tmp16$N[i] + tmp16$P[i])$conf.int
	tmp16$lci[i] <- cis[1]
	tmp16$hci[i] <- cis[2]
}
#cutoff 24
tmp24 <- as.data.frame(table(CDV_all$disease24, CDV_all$sex, CDV_all$spp))
names(tmp24) <- c("distemper", "sex", "spp", "N")

# reshape and calculate the cis
tmp24 <- dcast(tmp24, spp + sex ~ distemper, sum)
tmp24$prev <- tmp24$P/(tmp24$N + tmp24$P)
tmp24$lci <- 0
tmp24$hci <- 0

for(i in 1:dim(tmp24)[1]){
	cis <- binom.test(tmp24$P[i], tmp24$N[i] + tmp24$P[i])$conf.int
	tmp24$lci[i] <- cis[1]
	tmp24$hci[i] <- cis[2]
}

tmp12$threshold <- 12
tmp24$threshold <- 24
tmp16$threshold <- 16
tmp <- rbind(tmp12, tmp16, tmp24)

tmp$threshold <- as.factor(tmp$threshold)

# Plot the seroprevalence
pd <- position_dodge(.3)
plot1 <- ggplot(tmp, aes(x = sex, y = prev, group = threshold, 
												 colour = threshold, shape = threshold, 
												 fill = threshold)) +
	ylab("CDV seroprevalence") + xlab("") + ylim(0,.7) +
	geom_point(size = 3, position = pd, size = 4) +
	geom_errorbar(aes(ymin = lci, ymax = hci), width = 0, position = pd, size = 1) +
	theme_bw() +	facet_grid(~spp) + 
	scale_shape_manual(values = c(21, 22, 23)) +
	scale_fill_manual(values = cols3_gb) +
	scale_color_manual(values = cols3_gb) +
	theme(text = element_text(size = 14),  panel.grid.minor.y = element_blank(),
				panel.grid.major.x = element_blank(), legend.position = c(.15,.85))
plot1
ggsave("./Figures/Fig3.png", width = 4, height = 5, unit = "in", dpi = 600)

pdf(file = "./Figures/Fig3.pdf", bg = "white", width = 4, height = 5)
pd <- position_dodge(.3)
plot1
dev.off()

# FIGURE S2: Titer distribution
plot1 <- ggplot(CDV_all, aes(log(titer))) +
	geom_histogram(bins = 50) +
	geom_vline(xintercept = log(12), colour = "grey", linetype = "dashed") +
	geom_vline(xintercept = log(24), colour = "red") +
	geom_vline(xintercept = log(16)) + facet_grid(~spp) + theme_bw() +
	theme(text = element_text(size = 16),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank())
plot1
ggsave("./Figures/Fig_S2.png", width = 6, height = 4, unit = "in", dpi = 600)

pdf(file = "./Figures/Fig_S2.pdf",
		bg = "white", width = 5, height = 3)
plot1
dev.off()


###################################
# Plot the testing intervals
###################################
source("./Code/plot_interval_fxn.r")
png(filename = paste("./Figures/intervals_bw", cutoff, ".png", sep = ""),
		bg = "white", width = 800, height = 400, units = "px")
par(mfrow = c(1,2))
plot_bear_interval_bw(subset(CDV, CDV$spp == "bear"), 1.5)
plot_wolf_interval_bw(subset(CDV, CDV$spp == "wolf"), 1.5)
dev.off()


####################################################
# Northern Range
####################################################
# Annual hazards plot for NR only
par(mfrow = c(1,1))
png(filename = paste("./Figures/FigS5_NR_", cutoff, ".png", sep = ""),
		bg = "white", width = 600, height = 350,
		pointsize = 16)
plot_haz(mod5b_NR, "Prob.yr.b", "Prob.yr.w")
dev.off()

#Compare the slope estimates
n <- mod5b$BUGSoutput$n.sims
x <- data.frame(all = mod5b$BUGSoutput$sims.list$alpha[1:n] ,
								northern_range = mod5b_NR$BUGSoutput$sims.list$alpha[1:n])
d <- melt(x)
names(d) <- c("Data", "value")
fig_S6 <- ggplot(d, aes(x=value, fill = Data)) +
	geom_density(alpha= 0.7) +
	xlim(-.1,4.5) + xlab("Slope parameter") +
	scale_fill_manual(values = c("grey", "black")) +
	theme_bw() + 
	theme(text = element_text(size = 14), legend.position = c(.8,.8))
fig_S6
ggsave("./Figures/Fig_S6.png", width = 5, height = 3, units = "in", dpi = 600)

