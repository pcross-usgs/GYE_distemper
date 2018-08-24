# Script to run JAGS models
rm(list = ls())
setwd("~/My Documents/Research/Active Papers/YNP CDV/") # may need to be altered for other computers
#load the libraries
library(R2jags)
# load in the data and model code
source("./Code/WriteJAGSmodels_paper.R")
source("./Code/plot_haz_fxn.R")
source("./Code/plot_viana_haz.R")
source("./Code/plot_bear_wolf_haz_fxn.R")

cutoff <- 24
if(cutoff == 12){load("./Data/Wolf_Bear_12.RData")}
if(cutoff == 16){load("./Data/Wolf_Bear_16.RData")}
if(cutoff == 24){load("./Data/Wolf_Bear_24.RData")}

n <- length(infected) #number of intervals
unif.prior <- c(-20, 2)
norm.prior <- c(-6, .25) # .25 is a variance of 4
beta.prior <- c(25, .5)
prec <- 0.25  # precision on some of the priors
niters <- 200000 # number of MCMC iterations.
nburn <- 5000


#*************************************************
# 1. Yr model, SPP combined
##################################################
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
                  'unif.prior')
params <- c("Prob.yr")
mod1 <- jags(jags.data, inits = NULL , params, Yr_mod, n.chains = 3,
             n.iter = niters)

if(cutoff == 12){save(mod1, file = "./Output/mod1_12.RData")}
if(cutoff == 16){save(mod1, file = "./Output/mod1_16.RData")}
if(cutoff == 24){save(mod1, file = "./Output/mod1_24.RData")}

# run with a normal prior
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
                  'norm.prior')
params <- c("Prob.yr")
mod1b <- jags(jags.data, inits = NULL , params, Yr_mod_norm, n.chains = 3,
             n.iter = niters)
if(cutoff == 12){save(mod1b, file = "./Output/mod1b_12.RData")}
if(cutoff == 16){save(mod1b, file = "./Output/mod1b_16.RData")}
if(cutoff == 24){save(mod1b, file = "./Output/mod1b_24.RData")}
##################################################

#*************************************************
# 2. Yr model, SPP separate
##################################################
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
                  'unif.prior', 'wolf', 'bear')
params <- c("Prob.yr.b", "Prob.yr.w", "gamma.w", "gamma.b")
mod2 <- jags(jags.data, inits = NULL , params, Yr_mod_sp, n.chains = 3,
             n.iter = niters)
if(cutoff == 12){save(mod2, file = "./Output/mod2_12.RData")}
if(cutoff == 16){save(mod2, file = "./Output/mod2_16.RData")}
if(cutoff == 24){save(mod2, file = "./Output/mod2_24.RData")}

# run it again with a normal prior on gamma.
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
                  'norm.prior', 'wolf', 'bear')
params <- c("Prob.yr.b", "Prob.yr.w", "gamma.w", "gamma.b")
mod2b <- jags(jags.data, inits = NULL , params, Yr_mod_sp_norm, n.chains = 3,
             n.iter = niters)
if(cutoff == 12){save(mod2b, file = "./Output/mod2b_12.RData")}
if(cutoff == 16){save(mod2b, file = "./Output/mod2b_16.RData")}
if(cutoff == 24){save(mod2b, file = "./Output/mod2b_24.RData")}
##################################################

#***************************************************
# 3. Correlated spp model
####################################################
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
                  'unif.prior', 'wolf', 'bear')
params <- c("Prob.yr.b", "Prob.yr.w", "alpha", "gamma.w", "gamma.b")
mod3 <- jags(jags.data, inits = NULL , params, Yr_mod_corr_sp, n.chains = 3,
						 n.iter = niters, n.burnin = nburn,
						 n.thin = max(1, floor((niters - nburn) / 1000)))
if(cutoff == 12){save(mod3, file = "./Output/mod3_12.RData")}
if(cutoff == 16){save(mod3, file = "./Output/mod3_16.RData")}
if(cutoff == 24){save(mod3, file = "./Output/mod3_24.RData")}

#normal model
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
                  'norm.prior', 'wolf', 'bear', 'prec')
params <- c("Prob.yr.b", "Prob.yr.w", "alpha", "gamma.w", "gamma.b")
mod3b <- jags(jags.data, inits = NULL , params, Yr_mod_corr_sp_norm, n.chains = 3,
							n.iter = niters, n.burnin = nburn,
							n.thin = max(1, floor((niters - nburn) / 1000)))
if(cutoff == 12){save(mod3b, file = "./Output/mod3b_12.RData")}
if(cutoff == 16){save(mod3b, file = "./Output/mod3b_16.RData")}
if(cutoff == 24){save(mod3b, file = "./Output/mod3b_24.RData")}
##################################################

#************************************************
# 4. Test errors, but no correlation
####################################################
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
									'norm.prior', 'wolf', 'bear', 'beta.prior')

params <- c("Prob.yr.b", "Prob.yr.w", "qneg.bears", "qpos.bears",
						"qneg.wolves", "qpos.wolves", "gamma.w", "gamma.b")

mod4 <- jags(jags.data, inits = NULL , params, Yr_mod_test_norm, n.chains = 3,
						 n.iter = niters, n.burnin = nburn,
						 n.thin = max(1, floor((niters - nburn) / 1000)))
if(cutoff == 12){save(mod4, file = "./Output/mod4_12.RData")}
if(cutoff == 16){save(mod4, file = "./Output/mod4_16.RData")}
if(cutoff == 24){save(mod4, file = "./Output/mod4_24.RData")}
##################################################

#************************************************
# 5. Run cloglog model with Test errors
####################################################
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
                  'unif.prior', 'wolf', 'bear', 'beta.prior')

params <- c("Prob.yr.b", "Prob.yr.w", "alpha", "qneg.bears", "qpos.bears",
            "qneg.wolves", "qpos.wolves", "gamma.w", "gamma.b")

mod5 <- jags(jags.data, inits = NULL , params, Yr_mod_corr_sp_test, n.chains = 3,
						 n.iter = niters, n.burnin = nburn,
						 n.thin = max(1, floor((niters - nburn) / 1000)))
if(cutoff == 12){save(mod5, file = "./Output/mod5_12.RData")}
if(cutoff == 16){save(mod5, file = "./Output/mod5_16.RData")}
if(cutoff == 24){save(mod5, file = "./Output/mod5_24.RData")}

# 5b. with normal priors
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
                  'norm.prior', 'wolf', 'bear', 'beta.prior', 'prec')

params <- c("Prob.yr.b", "Prob.yr.w", "alpha", "qneg.bears", "qpos.bears",
            "qneg.wolves", "qpos.wolves", "gamma.w", "gamma.b")

mod5b <- jags(jags.data, inits = NULL , params, Yr_mod_corr_sp_test_norm,
							n.chains = 3, DIC = TRUE,
							n.iter = niters, n.burnin = nburn,
							n.thin = max(1, floor((niters - nburn) / 1000)))
if(cutoff == 12){save(mod5b, file = "./Output/mod5b_12.RData")}
if(cutoff == 16){save(mod5b, file = "./Output/mod5b_16.RData")}
if(cutoff == 24){save(mod5b, file = "./Output/mod5b_24.RData")}
##################################################

#************************************************
# 5c. with more diffuse normal priors
##################################################
norm.prior <- c(-6, .1) # .25 is a variance of 4
beta.prior <- c(25, .5)
prec <- 0.1  # precision on some of the priors
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
									'norm.prior', 'wolf', 'bear', 'beta.prior', 'prec')

params <- c("Prob.yr.b", "Prob.yr.w", "alpha", "qneg.bears", "qpos.bears",
						"qneg.wolves", "qpos.wolves", "gamma.w", "gamma.b")
mod5c <- jags(jags.data, inits = NULL , params, Yr_mod_corr_sp_test_norm, n.chains = 3,
							n.iter = niters, n.burnin = nburn,
							n.thin = max(1, floor((niters - nburn) / 1000)))
if(cutoff == 12){save(mod5c, file = "./Output/mod5c_12.RData")}
if(cutoff == 16){save(mod5c, file = "./Output/mod5c_16.RData")}
if(cutoff == 24){save(mod5c, file = "./Output/mod5c_24.RData")}
##################################################

#************************************************
# 5d. with diffuse test priors
##################################################
norm.prior <- c(-6, .1) # .25 is a variance of 4
beta.prior <- c(10, .5)
prec <- 0.1  # precision on some of the priors
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
									'norm.prior', 'wolf', 'bear', 'beta.prior', 'prec')
mod5d <- jags(jags.data, inits = NULL , params, Yr_mod_corr_sp_test_norm, n.chains = 3,
							n.iter = niters, n.burnin = nburn,
							n.thin = max(1, floor((niters - nburn) / 1000)))
if(cutoff == 12){save(mod5d, file = "./Output/mod5d_12.RData")}
if(cutoff == 16){save(mod5d, file = "./Output/mod5d_16.RData")}
if(cutoff == 24){save(mod5d, file = "./Output/mod5d_24.RData")}
##################################################

#************************************************
# 6b. Like 5b but going from bears to wolves.
##################################################
norm.prior <- c(-6, .25) # .25 is a variance of 4
beta.prior <- c(25, .5)
prec <- .25  # precision on some of the priors
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
									'norm.prior', 'wolf', 'bear', 'beta.prior', 'prec')

params <- c("Prob.yr.b", "Prob.yr.w", "alpha", "qneg.bears", "qpos.bears",
						"qneg.wolves", "qpos.wolves", "gamma.w", "gamma.b")

mod6b <- jags(jags.data, inits = NULL , params, Yr_mod_corr_bw_sp_test_norm, n.chains = 3,
						 n.iter = niters, n.burnin = nburn,
						 n.thin = max(1, floor((niters - nburn) / 1000)))

print(mod6b, digits = 2)
plot(mod6b)
if(cutoff == 12){save(mod6b, file = "./Output/mod6b_12.RData")}
if(cutoff == 16){save(mod6b, file = "./Output/mod6b_16.RData")}
if(cutoff == 24){save(mod6b, file = "./Output/mod6b_24.RData")}

#************************************************
# 7. Model with AR terms only, bears affect wolves
##################################################
norm.prior <- c(-6, .25) # .25 is a variance of 4
beta.prior <- c(25, .5)
prec <- .25  # precision on some of the priors

jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
									'norm.prior', 'wolf', 'bear', 'beta.prior', 'prec')

params <- c("Prob.yr.b", "Prob.yr.w", "alpha", "qneg.bears", "qpos.bears",
						"qneg.wolves", "qpos.wolves", "beta.b0", "beta.w0",
						"betaLL.w1", "betaLL.w2", "betaLL.b1", "betaLL.b2", "gamma.w", "gamma.b")

mod7 <- jags(jags.data, inits = NULL , params, Yr_corr_sp_test_AR_bw_norm, n.chains = 3,
						 n.iter = niters, n.burnin = nburn,
						 n.thin = max(1, floor((niters - nburn) / 1000)))
if(cutoff == 12){save(mod7, file = "./Output/mod7_12.RData")}
if(cutoff == 16){save(mod7, file = "./Output/mod7_16.RData")}
if(cutoff == 24){save(mod7, file = "./Output/mod7_24.RData")}
#************************************************


#************************************************
# 8. Model with AR terms only, wolves affect bears
##################################################
norm.prior <- c(-6, .25) # .25 is a variance of 4
beta.prior <- c(25, .5)
prec <- .25  # precision on some of the priors
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
									'norm.prior', 'wolf', 'bear', 'beta.prior', 'prec')

params <- c("Prob.yr.b", "Prob.yr.w", "alpha", "qneg.bears", "qpos.bears",
						"qneg.wolves", "qpos.wolves", "beta.b0", "beta.w0",
						"betaLL.w1", "betaLL.w2", "betaLL.b1", "betaLL.b2", "gamma.w", "gamma.b")

mod8 <- jags(jags.data, inits = NULL , params, Yr_corr_sp_test_AR_norm, n.chains = 3,
						 n.iter = niters, n.burnin = nburn,
						 n.thin = max(1, floor((niters - nburn) / 1000)))
if(cutoff == 12){save(mod8, file = "./Output/mod8_12.RData")}
if(cutoff == 16){save(mod8, file = "./Output/mod8_16.RData")}
if(cutoff == 24){save(mod8, file = "./Output/mod8_24.RData")}

#************************************************
# 9. Model with AR terms only, wolves affect bears at a lag
##################################################
norm.prior <- c(-6, .25) # .25 is a variance of 4
beta.prior <- c(25, .5)
prec <- .25  # precision on some of the priors
jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
									'norm.prior', 'wolf', 'bear', 'beta.prior', 'prec')

params <- c("Prob.yr.b", "Prob.yr.w", "alpha", "qneg.bears", "qpos.bears",
						"qneg.wolves", "qpos.wolves", "beta.b0", "beta.w0",
						"betaLL.w1", "betaLL.w2", "betaLL.b1", "betaLL.b2")

mod9 <- jags(jags.data, inits = NULL , params, Yr_corr_sp_test_AR_wblag_norm,
							n.chains = 3, n.iter = niters, n.burnin = nburn,
							n.thin = max(1, floor((niters - nburn) / 1000)))
if(cutoff == 12){save(mod9, file = "./Output/mod9_12.RData")}
if(cutoff == 16){save(mod9, file = "./Output/mod9_16.RData")}
if(cutoff == 24){save(mod9, file = "./Output/mod9_24.RData")}

#************************************************
# 11. Model with 2 way transmission
##################################################
norm.prior <- c(-6, .25) # .25 is a variance of 4
prec <- .25  # precision on some of the priors
niters <- 5000 # number of MCMC iterations.
nburn <- 1000

jags.data <- list('n', 'n_year', 'left', 'right', 'infected', 'lookup',
									'norm.prior', 'wolf', 'bear', 'beta.prior', 'prec')

params <- c("Prob.yr.b", "Prob.yr.w", "alpha", "qneg.bears", "qpos.bears",
						"qneg.wolves", "qpos.wolves")

mod11 <- jags(jags.data, inits = NULL , params, Yr_corr_sp_test_2way_norm,
							n.chains = 3, n.iter = niters, n.burnin = nburn,
							n.thin = max(1, floor((niters - nburn) / 1000)))
if(cutoff == 12){save(mod11, file = "./Output/mod11_12.RData")}
if(cutoff == 16){save(mod11, file = "./Output/mod11_16.RData")}
if(cutoff == 24){save(mod11, file = "./Output/mod11_24.RData")}
