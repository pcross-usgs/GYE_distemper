#Script to write the BUGS models.
library(R2WinBUGS)

# Model 1.U Independent Annual hazard, SPP combined
Yr_mod <- function(){
  # PRIORS
  for (i in 1:n_year) {
    gamma[i] ~ dunif(unif.prior[1], unif.prior[2]) #log hazard
    Prob.mo[i] <- 1 - exp( -exp(gamma[i]))
    Prob.yr[i] <- 1 - ((1 - Prob.mo[i])^12)
  }

  # Likelihood
  for (j in 1:n) {
    for (k in left[j]:(right[j] - 1)) {
      #unit cumulative hazard
      UCH[j,k] <- exp(gamma[lookup[k]])
    }

    # prob of being infected
    rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))
    infected[j] ~ dbern(rho[j])
  }
}#end model

# Model 1. same model just a normal prior
Yr_mod_norm <- function(){
  # PRIORS
  for (i in 1:n_year) {
    gamma[i] ~ dnorm(norm.prior[1], norm.prior[2]) #log hazard
    Prob.mo[i] <- 1 - exp( -exp(gamma[i]))
    Prob.yr[i] <- 1 - ((1 - Prob.mo[i])^12)
  }

  # Likelihood
  for (j in 1:n) {
    for (k in left[j]:(right[j] - 1)) {
      #unit cumulative hazard
      UCH[j,k] <- exp(gamma[lookup[k]])
    }

    # prob of being infected
    rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))
    infected[j] ~ dbern(rho[j])
  }
}#end model

# Model 2.U Independent Annual hazard, SPP separate
Yr_mod_sp <- function(){
  # PRIORS
  #bear data available for all intervals
  for (i in 1:n_year) {gamma.b[i] ~ dunif(unif.prior[1], unif.prior[2])}

  # wolf data only available for intervals 26-44
  for(i in c(1:25, n_year)){gamma.w[i] <- -99.99}
  for(i in 26:(n_year-1)){gamma.w[i] ~ dunif(unif.prior[1], unif.prior[2])}

  # Translate to annual probabilities
  for(i in 1:n_year){
    Prob.mo.b[i] <- 1 - exp( -exp(gamma.b[i]))
    Prob.mo.w[i] <- 1- exp( -exp(gamma.w[i]))

    Prob.yr.b[i] <- 1 - ((1 - Prob.mo.b[i])^12)
    Prob.yr.w[i] <- 1 - ((1 - Prob.mo.w[i])^12)
  }

  # Likelihood
  for (j in 1:n) {
    for (k in left[j]:(right[j] - 1)) {
      #unit cumulative hazard
      UCH[j,k] <- exp(gamma.b[lookup[k]] * bear[j] + gamma.w[lookup[k]] * wolf[j])
    }

    # prob of being infected
    rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))
    infected[j] ~ dbern(rho[j])
  }
}#end model

# Model 2. normal priors
Yr_mod_sp_norm <- function(){
  # PRIORS
  #bear data available for all intervals
  for (i in 1:n_year) {gamma.b[i] ~ dnorm(norm.prior[1], norm.prior[2])}

  # wolf data only available for intervals 26-44
  for(i in c(1:25, n_year)){gamma.w[i] <- -99.99}
  for(i in 26:(n_year-1)){gamma.w[i] ~ dnorm(norm.prior[1], norm.prior[2])}

  # Translate to annual probabilities
  for(i in 1:n_year){
    Prob.mo.b[i] <- 1 - exp( -exp(gamma.b[i]))
    Prob.mo.w[i] <- 1- exp( -exp(gamma.w[i]))

    Prob.yr.b[i] <- 1 - ((1 - Prob.mo.b[i])^12)
    Prob.yr.w[i] <- 1 - ((1 - Prob.mo.w[i])^12)
  }

  # Likelihood
  for (j in 1:n) {
    for (k in left[j]:(right[j] - 1)) {
      #unit cumulative hazard
      UCH[j,k] <- exp(gamma.b[lookup[k]] * bear[j] + gamma.w[lookup[k]] * wolf[j])
    }

    # prob of being infected
    rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))
    infected[j] ~ dbern(rho[j])
  }
}#end model

# Model 3.U Correlate bear to wolf hazard
Yr_mod_corr_sp <- function(){
  # PRIORS
  alpha ~ dunif(-4, 4) # correlation in the log hazards

  # no wolf data
  for(i in c(1:25, n_year)){
    gamma.b[i] ~ dunif(unif.prior[1], unif.prior[2])
    gamma.w[i] <- -99.99
  }
  # wolf data
  for(i in 26:(n_year-1)){
    gamma.w[i] ~ dunif(unif.prior[1], unif.prior[2])
    beta.b[i] ~ dunif(unif.prior[1], unif.prior[2])
    gamma.b[i] <- beta.b[i] + alpha * gamma.w[i]
  }

  # Translate to annual probabilities
  for(i in 1:n_year){
    Prob.mo.b[i] <- 1 - exp( -exp(gamma.b[i]))
    Prob.mo.w[i] <- 1 - exp( -exp(gamma.w[i]))

    Prob.yr.b[i] <- 1 - ((1 - Prob.mo.b[i])^12)
    Prob.yr.w[i] <- 1 - ((1 - Prob.mo.w[i])^12)
  }

  # Likelihood
  for (j in 1:n) {
    for (k in left[j]:(right[j] - 1)) {
      #unit cumulative hazard
      UCH[j,k] <- exp(gamma.b[lookup[k]] * bear[j] + gamma.w[lookup[k]] * wolf[j])
    }

    # prob of not being infected
    rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))
    infected[j] ~ dbern(rho[j])
  }
}#end model

#Model 3.
Yr_mod_corr_sp_norm <- function(){
  # PRIORS
  alpha ~ dnorm(0, prec) # correlation in the log hazards

  # no wolf data
  for(i in c(1:25, n_year)){
    gamma.b[i] ~ dnorm(norm.prior[1], norm.prior[2])
    gamma.w[i] <- -99.99
  }
  # wolf data
  for(i in 26:(n_year-1)){
     gamma.w[i] ~ dnorm(norm.prior[1], norm.prior[2])
     beta.b[i] ~ dnorm(norm.prior[1], norm.prior[2])
     gamma.b[i] <- beta.b[i] + alpha * gamma.w[i]
  }

  # Translate to annual probabilities
  for(i in 1:n_year){
    Prob.mo.b[i] <- 1 - exp( -exp(gamma.b[i]))
    Prob.mo.w[i] <- 1 - exp( -exp(gamma.w[i]))

    Prob.yr.b[i] <- 1 - ((1 - Prob.mo.b[i])^12)
    Prob.yr.w[i] <- 1 - ((1 - Prob.mo.w[i])^12)
  }

  # Likelihood
  for (j in 1:n) {
    for (k in left[j]:(right[j] - 1)) {
      #unit cumulative hazard
      UCH[j,k] <- exp(gamma.b[lookup[k]] * bear[j] + gamma.w[lookup[k]] * wolf[j])
    }

    # prob of being infected
    rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))
    infected[j] ~ dbern(rho[j])
  }
}#end model

# Model 4. Test errors, normal prior
Yr_mod_test_norm <- function(){
	
	# qpos and neg
	qneg.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qneg.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	
	# no wolf data
	for(i in c(1:25, n_year)){
		gamma.b[i] ~ dnorm(norm.prior[1], norm.prior[2])
		gamma.w[i] <- -99.99
	}
	# wolf data
	for(i in 26:(n_year-1)){
		gamma.w[i] ~ dnorm(norm.prior[1], norm.prior[2])
		gamma.b[i] ~ dnorm(norm.prior[1], norm.prior[2])
	}
	
	# Translate to annual probabilities
	for(i in 1:n_year){
		Prob.mo.b[i] <- 1 - exp( -exp(gamma.b[i]))
		Prob.mo.w[i] <- 1 - exp( -exp(gamma.w[i]))
		
		Prob.yr.b[i] <- 1 - ((1 - Prob.mo.b[i])^12)
		Prob.yr.w[i] <- 1 - ((1 - Prob.mo.w[i])^12)
	}
	
	# Likelihood
	for (j in 1:n) {
		for (k in left[j]:(right[j] - 1)) {
			#unit cumulative hazard
			UCH[j,k] <- exp(gamma.b[lookup[k]] * bear[j] + gamma.w[lookup[k]] * wolf[j])
		}
		
		# prob of being infected
		rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))
		
		p[j]<- qpos.bears * rho[j] * bear[j] + (1 - qneg.bears) * (1 - rho[j]) * bear[j] +
			qpos.wolves * rho[j] * wolf[j] + (1 - qneg.wolves) * (1 - rho[j]) * wolf[j]
		
		infected[j] ~ dbern(p[j])
	}
}#end model

# Model 5.U Test Error and Correlated bear to wolf hazard
Yr_mod_corr_sp_test <- function(){

  # PRIORS
  alpha ~ dunif(-4, 4) # correlation in the log hazards

  # qpos and neg
  qneg.bears ~ dbeta(beta.prior[1], beta.prior[2])
  qpos.bears ~ dbeta(beta.prior[1], beta.prior[2])
  qneg.wolves ~ dbeta(beta.prior[1], beta.prior[2])
  qpos.wolves ~ dbeta(beta.prior[1], beta.prior[2])

  # no wolf data
  for(i in c(1:25, n_year)){
    gamma.b[i] ~ dunif(unif.prior[1], unif.prior[2])
    gamma.w[i] <- -99.99
  }
  # wolf data
  for(i in 26:(n_year-1)){
    gamma.w[i] ~ dunif(unif.prior[1], unif.prior[2])
    beta.b[i] ~ dunif(unif.prior[1], unif.prior[2])
    gamma.b[i] <- beta.b[i] + alpha * gamma.w[i]
  }

  # Translate to annual probabilities
  for(i in 1:n_year){
    Prob.mo.b[i] <- 1 - exp( -exp(gamma.b[i]))
    Prob.mo.w[i] <- 1 - exp( -exp(gamma.w[i]))

    Prob.yr.b[i] <- 1 - ((1 - Prob.mo.b[i])^12)
    Prob.yr.w[i] <- 1 - ((1 - Prob.mo.w[i])^12)
  }

  # Likelihood
  for (j in 1:n) {
    for (k in left[j]:(right[j] - 1)) {
      #unit cumulative hazard
      UCH[j,k] <- exp(gamma.b[lookup[k]] * bear[j] + gamma.w[lookup[k]] * wolf[j])
    }

    # prob of being infected
    rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))

    p[j]<- qpos.bears * rho[j] * bear[j] + (1 - qneg.bears) * (1 - rho[j]) * bear[j] +
      qpos.wolves * rho[j] * wolf[j] + (1 - qneg.wolves) * (1 - rho[j]) * wolf[j]

    infected[j] ~ dbern(p[j])
  }
}#end model

# Model 5.1, 5.2 or 5.3 depending on the priors
Yr_mod_corr_sp_test_norm <- function(){

  # PRIORS
  alpha ~ dnorm(0, prec) # correlation in the log hazards

  # qpos and neg
  qneg.bears ~ dbeta(beta.prior[1], beta.prior[2])
  qpos.bears ~ dbeta(beta.prior[1], beta.prior[2])
  qneg.wolves ~ dbeta(beta.prior[1], beta.prior[2])
  qpos.wolves ~ dbeta(beta.prior[1], beta.prior[2])

  # no wolf data
  for(i in c(1:25, n_year)){
    gamma.b[i] ~ dnorm(norm.prior[1], norm.prior[2])
    gamma.w[i] <- -99.99
  }
  # wolf data
  for(i in 26:(n_year-1)){
    gamma.w[i] ~ dnorm(norm.prior[1], norm.prior[2])
    beta.b[i]  ~ dnorm(norm.prior[1], norm.prior[2])
    gamma.b[i] <- beta.b[i] + alpha * gamma.w[i]
 }

  # Translate to annual probabilities
  for(i in 1:n_year){
    Prob.mo.b[i] <- 1 - exp( -exp(gamma.b[i]))
    Prob.mo.w[i] <- 1 - exp( -exp(gamma.w[i]))

    Prob.yr.b[i] <- 1 - ((1 - Prob.mo.b[i])^12)
    Prob.yr.w[i] <- 1 - ((1 - Prob.mo.w[i])^12)
  }

  # Likelihood
  for (j in 1:n) {
    for (k in left[j]:(right[j] - 1)) {
      #unit cumulative hazard
      UCH[j,k] <- exp(gamma.b[lookup[k]] * bear[j] + gamma.w[lookup[k]] * wolf[j])
    }

    # prob of being infected
    rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))

    p[j]<- qpos.bears * rho[j] * bear[j] + (1 - qneg.bears) * (1 - rho[j]) * bear[j] +
      qpos.wolves * rho[j] * wolf[j] + (1 - qneg.wolves) * (1 - rho[j]) * wolf[j]

    infected[j] ~ dbern(p[j])
  }
}#end model

# Model 6. Same as 5, but from bears to wolves, with normal priors
Yr_mod_corr_bw_sp_test_norm <- function(){
	
	# PRIORS
	alpha ~ dnorm(0, prec) # correlation in the log hazards
	
	# qpos and neg
	qneg.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qneg.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	
	# no wolf data
	for(i in c(1:25, n_year)){
		gamma.b[i] ~ dnorm(norm.prior[1], norm.prior[2])
		gamma.w[i] <- -99.99
	}
	# wolf data
	for(i in 26:(n_year-1)){
		gamma.b[i] ~ dnorm(norm.prior[1], norm.prior[2])
		beta.w[i]  ~ dnorm(norm.prior[1], norm.prior[2])
		gamma.w[i] <- beta.w[i] + alpha * gamma.b[i]
	}
	
	# Translate to annual probabilities
	for(i in 1:n_year){
		Prob.mo.b[i] <- 1 - exp( -exp(gamma.b[i]))
		Prob.mo.w[i] <- 1 - exp( -exp(gamma.w[i]))
		
		Prob.yr.b[i] <- 1 - ((1 - Prob.mo.b[i])^12)
		Prob.yr.w[i] <- 1 - ((1 - Prob.mo.w[i])^12)
	}
	
	# Likelihood
	for (j in 1:n) {
		for (k in left[j]:(right[j] - 1)) {
			#unit cumulative hazard
			UCH[j,k] <- exp(gamma.b[lookup[k]] * bear[j] + gamma.w[lookup[k]] * wolf[j])
		}
		
		# prob of being infected
		rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))
		
		p[j]<- qpos.bears * rho[j] * bear[j] + (1 - qneg.bears) * (1 - rho[j]) * bear[j] +
			qpos.wolves * rho[j] * wolf[j] + (1 - qneg.wolves) * (1 - rho[j]) * wolf[j]
		
		infected[j] ~ dbern(p[j])
	}
}#end model


#Model 8. Model with overall trend, AR terms, and wolves affect bears
Yr_tr_corr_sp_test_AR_norm <- function(){
	
	# PRIORS
	# Testing errors: qpos and neg
	qneg.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qneg.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	
	# intercepts
	beta.b0 ~ dnorm(norm.prior[1], norm.prior[2])
	beta.w0 ~ dnorm(norm.prior[1], norm.prior[2])

	# annual trend
	beta.b1 ~ dnorm(0, prec)
	beta.w1 ~ dnorm(0, prec)

	# AR1 and 2 terms
	betaLL.b1 ~ dnorm(0, prec)
	betaLL.b2 ~ dnorm(0, prec)
	betaLL.w1 ~ dnorm(0, prec)
	betaLL.w2 ~ dnorm(0, prec)
	
	# correlation in the log hazards
	alpha ~ dnorm(0, prec) 
	
	for(t in 3:n_year){
		gamma.b[t] ~ dnorm(beta.b0 + beta.b1 * t + betaLL.b1 * gamma.b[t-1] + 
											 betaLL.b2 * gamma.b[t-2] + alpha * gamma.w[t], norm.prior[2])
		gamma.w[t] ~ dnorm(beta.w0 + beta.w1 * t + betaLL.w1 * gamma.w[t-1] + 
											 	betaLL.w2 * gamma.w[t-2], norm.prior[2])
	}
	
	gamma.w[1] ~ dnorm(norm.prior[1], norm.prior[2])
	gamma.w[2] ~ dnorm(norm.prior[1], norm.prior[2])
	gamma.b[1] ~ dnorm(norm.prior[1], norm.prior[2])
	gamma.b[2] ~ dnorm(norm.prior[1], norm.prior[2])
	
	# Translate to annual probabilities
	for(t in 1:n_year){
		Prob.mo.b[t] <- 1 - exp( -exp(gamma.b[t]))
		Prob.mo.w[t] <- 1 - exp( -exp(gamma.w[t]))
		
		Prob.yr.b[t] <- 1 - ((1 - Prob.mo.b[t])^12)
		Prob.yr.w[t] <- 1 - ((1 - Prob.mo.w[t])^12)
	}
	
	# Likelihood
	for (j in 1:n) {
		for (k in left[j]:(right[j] - 1)) {
			#unit cumulative hazard
			UCH[j,k] <- exp(gamma.b[lookup[k]] * bear[j] + gamma.w[lookup[k]] * wolf[j])
		}
		
		# prob of being infected
		rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))
		# prob of being detected
		p[j]<- qpos.bears * rho[j] * bear[j] + (1 - qneg.bears) * (1 - rho[j]) * bear[j] +
			qpos.wolves * rho[j] * wolf[j] + (1 - qneg.wolves) * (1 - rho[j]) * wolf[j]
		# link to the observed data
		infected[j] ~ dbern(p[j])
	}
}#end model

#Model 7. Model with overall AR terms, bears affect wolves
Yr_corr_sp_test_AR_bw_norm <- function(){
	
	# PRIORS
	# Testing errors: qpos and neg
	qneg.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qneg.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	
	# intercepts
	beta.b0 ~ dnorm(norm.prior[1], norm.prior[2])
	beta.w0 ~ dnorm(norm.prior[1], norm.prior[2])
	
	# AR1 and 2 terms
	betaLL.b1 ~ dnorm(0, prec)
	betaLL.b2 ~ dnorm(0, prec)
	betaLL.w1 ~ dnorm(0, prec)
	betaLL.w2 ~ dnorm(0, prec)
	
	# correlation in the log hazards
	alpha ~ dnorm(0, prec) 
	
	for(t in 3:n_year){
		gamma.b[t] ~ dnorm(beta.b0 + betaLL.b1 * gamma.b[t-1] + 
											 	betaLL.b2 * gamma.b[t-2], norm.prior[2])
		gamma.w[t] ~ dnorm(beta.w0 + betaLL.w1 * gamma.w[t-1] + 
											 	betaLL.w2 * gamma.w[t-2] + alpha * gamma.b[t], norm.prior[2])
	}
	
	gamma.w[1] ~ dnorm(norm.prior[1], norm.prior[2])
	gamma.w[2] ~ dnorm(norm.prior[1], norm.prior[2])
	gamma.b[1] ~ dnorm(norm.prior[1], norm.prior[2])
	gamma.b[2] ~ dnorm(norm.prior[1], norm.prior[2])
	
	# Translate to annual probabilities
	for(t in 1:n_year){
		Prob.mo.b[t] <- 1 - exp( -exp(gamma.b[t]))
		Prob.mo.w[t] <- 1 - exp( -exp(gamma.w[t]))
		
		Prob.yr.b[t] <- 1 - ((1 - Prob.mo.b[t])^12)
		Prob.yr.w[t] <- 1 - ((1 - Prob.mo.w[t])^12)
	}
	
	# Likelihood
	for (j in 1:n) {
		for (k in left[j]:(right[j] - 1)) {
			#unit cumulative hazard
			UCH[j,k] <- exp(gamma.b[lookup[k]] * bear[j] + gamma.w[lookup[k]] * wolf[j])
		}
		
		# prob of being infected
		rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))
		# prob of being detected
		p[j]<- qpos.bears * rho[j] * bear[j] + (1 - qneg.bears) * (1 - rho[j]) * bear[j] +
			qpos.wolves * rho[j] * wolf[j] + (1 - qneg.wolves) * (1 - rho[j]) * wolf[j]
		# link to the observed data
		infected[j] ~ dbern(p[j])
	}
}#end model

#Model 8. Model with AR terms only, wolves affect bears
Yr_corr_sp_test_AR_norm <- function(){
	
	# PRIORS
	# Testing errors: qpos and neg
	qneg.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qneg.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	
	# intercepts
	beta.b0 ~ dnorm(norm.prior[1], norm.prior[2])
	beta.w0 ~ dnorm(norm.prior[1], norm.prior[2])
	
	# AR1 and 2 terms
	betaLL.b1 ~ dnorm(0, prec)
	betaLL.b2 ~ dnorm(0, prec)
	betaLL.w1 ~ dnorm(0, prec)
	betaLL.w2 ~ dnorm(0, prec)
	
	# correlation in the log hazards
	alpha ~ dnorm(0, prec) 
	
	for(t in 3:n_year){
		gamma.b[t] ~ dnorm(beta.b0 + betaLL.b1 * gamma.b[t-1] + 
											 	betaLL.b2 * gamma.b[t-2] + alpha * gamma.w[t], norm.prior[2])
		gamma.w[t] ~ dnorm(beta.w0 + betaLL.w1 * gamma.w[t-1] + 
											 	betaLL.w2 * gamma.w[t-2], norm.prior[2])
	}
	
	gamma.w[1] ~ dnorm(norm.prior[1], norm.prior[2])
	gamma.w[2] ~ dnorm(norm.prior[1], norm.prior[2])
	gamma.b[1] ~ dnorm(norm.prior[1], norm.prior[2])
	gamma.b[2] ~ dnorm(norm.prior[1], norm.prior[2])
	
	# Translate to annual probabilities
	for(t in 1:n_year){
		Prob.mo.b[t] <- 1 - exp( -exp(gamma.b[t]))
		Prob.mo.w[t] <- 1 - exp( -exp(gamma.w[t]))
		
		Prob.yr.b[t] <- 1 - ((1 - Prob.mo.b[t])^12)
		Prob.yr.w[t] <- 1 - ((1 - Prob.mo.w[t])^12)
	}
	
	# Likelihood
	for (j in 1:n) {
		for (k in left[j]:(right[j] - 1)) {
			#unit cumulative hazard
			UCH[j,k] <- exp(gamma.b[lookup[k]] * bear[j] + gamma.w[lookup[k]] * wolf[j])
		}
		
		# prob of being infected
		rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))
		# prob of being detected
		p[j]<- qpos.bears * rho[j] * bear[j] + (1 - qneg.bears) * (1 - rho[j]) * bear[j] +
			qpos.wolves * rho[j] * wolf[j] + (1 - qneg.wolves) * (1 - rho[j]) * wolf[j]
		# link to the observed data
		infected[j] ~ dbern(p[j])
	}
}#end model

#9. Model with overall AR terms, wolves affect bears at a lag of 1 year
Yr_corr_sp_test_AR_wblag_norm <- function(){
	
	# PRIORS
	# Testing errors: qpos and neg
	qneg.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qneg.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	
	# intercepts
	beta.b0 ~ dnorm(norm.prior[1], norm.prior[2])
	beta.w0 ~ dnorm(norm.prior[1], norm.prior[2])
	
	# AR1 and 2 terms
	betaLL.b1 ~ dnorm(0, prec)
	betaLL.b2 ~ dnorm(0, prec)
	betaLL.w1 ~ dnorm(0, prec)
	betaLL.w2 ~ dnorm(0, prec)
	
	# correlation in the log hazards
	alpha ~ dnorm(0, prec) 
	
	for(t in 3:n_year){
		gamma.b[t] ~ dnorm(beta.b0 + betaLL.b1 * gamma.b[t-1] + alpha * gamma.w[t-1] + 
											 	betaLL.b2 * gamma.b[t-2], norm.prior[2])
		gamma.w[t] ~ dnorm(beta.w0 + betaLL.w1 * gamma.w[t-1] + 
											 	betaLL.w2 * gamma.w[t-2] , norm.prior[2])
	}
	
	gamma.w[1] ~ dnorm(norm.prior[1], norm.prior[2])
	gamma.w[2] ~ dnorm(norm.prior[1], norm.prior[2])
	gamma.b[1] ~ dnorm(norm.prior[1], norm.prior[2])
	gamma.b[2] ~ dnorm(norm.prior[1], norm.prior[2])
	
	# Translate to annual probabilities
	for(t in 1:n_year){
		Prob.mo.b[t] <- 1 - exp( -exp(gamma.b[t]))
		Prob.mo.w[t] <- 1 - exp( -exp(gamma.w[t]))
		
		Prob.yr.b[t] <- 1 - ((1 - Prob.mo.b[t])^12)
		Prob.yr.w[t] <- 1 - ((1 - Prob.mo.w[t])^12)
	}
	
	# Likelihood
	for (j in 1:n) {
		for (k in left[j]:(right[j] - 1)) {
			#unit cumulative hazard
			UCH[j,k] <- exp(gamma.b[lookup[k]] * bear[j] + gamma.w[lookup[k]] * wolf[j])
		}
		
		# prob of being infected
		rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))
		# prob of being detected
		p[j]<- qpos.bears * rho[j] * bear[j] + (1 - qneg.bears) * (1 - rho[j]) * bear[j] +
			qpos.wolves * rho[j] * wolf[j] + (1 - qneg.wolves) * (1 - rho[j]) * wolf[j]
		# link to the observed data
		infected[j] ~ dbern(p[j])
	}
}#end model

#Model with two way transmission
Yr_corr_sp_test_2way_norm <- function(){
	
	# PRIORS
	alpha ~ dnorm(0, prec) # correlation in the log hazards
	
	# qpos and neg
	qneg.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.bears ~ dbeta(beta.prior[1], beta.prior[2])
	qneg.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	qpos.wolves ~ dbeta(beta.prior[1], beta.prior[2])
	
	# no wolf data
	for(i in c(1:25, n_year)){
		gamma.b[i] ~ dnorm(norm.prior[1], norm.prior[2])
		gamma.w[i] <- -99.99
	}
	
	# wolf data
	for(i in 26:(n_year-1)){
		beta.b[i] ~ dnorm(norm.prior[1], norm.prior[2])
		beta.w[i] ~ dnorm(norm.prior[1], norm.prior[2])

		gamma.b[i] <- beta.b[i] + alpha * gamma.w[i]
		gamma.w[i] <- beta.w[i] + alpha * gamma.b[i]
	}
	
	# Translate to annual probabilities
	for(i in 1:n_year){
		Prob.mo.b[i] <- 1 - exp( -exp(gamma.b[i]))
		Prob.mo.w[i] <- 1 - exp( -exp(gamma.w[i]))
		
		Prob.yr.b[i] <- 1 - ((1 - Prob.mo.b[i])^12)
		Prob.yr.w[i] <- 1 - ((1 - Prob.mo.w[i])^12)
	}
	
	# Likelihood
	for (j in 1:n) {
		for (k in left[j]:(right[j] - 1)) {
			#unit cumulative hazard
			UCH[j,k] <- exp(gamma.b[lookup[k]] * bear[j] + gamma.w[lookup[k]] * wolf[j])
		}
		
		# prob of being infected
		rho[j] <- 1 - exp(-sum(UCH[j,left[j]:(right[j] - 1)]))
		
		p[j]<- qpos.bears * rho[j] * bear[j] + (1 - qneg.bears) * (1 - rho[j]) * bear[j] +
			qpos.wolves * rho[j] * wolf[j] + (1 - qneg.wolves) * (1 - rho[j]) * wolf[j]
		
		infected[j] ~ dbern(p[j])
	}
}#end model

# Viana model w/ no AR1 and AR2
Viana_mod1 <- function(){
	# PRIORS
	#Individual precision
	tau.Xbear <- 1/(sigma.Xbear*sigma.Xbear)
	sigma.Xbear ~ dunif(0,5)
	tau.Xwolf <- 1/(sigma.Xwolf*sigma.Xwolf)
	sigma.Xwolf ~ dunif(0,5)
	
	#false detection
	Qi_bear ~ dbeta(0.5,25)
	Qi_wolf ~ dbeta(0.5,25)
	
	#detection
	Pi_bear ~ dbeta(25,0.5)
	Pi_wolf ~ dbeta(25,0.5)
	
	for (t in 1:Ntime) {
		#Linear predictor
		pr_bear[t] ~ dnorm(norm.prior[1], norm.prior[2])
		pr_wolf[t] ~ dnorm(norm.prior[1], norm.prior[2])
	}#end time loop t
	
	#Likelihood of the bear data / observations
	for(i in 1:Nbear){
		titer_bear[i] ~ dbern(p_bear[i])
		# probability of infection
		r_bear[i]<- 1 - prod(P_bear[i,Birth_bear[i]:Sample_bear[i]])
		# probability of detection
		p_bear[i] <- Pi_bear*r_bear[i] +(1-r_bear[i])*Qi_bear
		
		#add individuals error to linear predictor and logit
		for(t in 1:Ntime){
			Xpr_bear[i,t] <- pr_bear[t] + X_bear[i,t]
			X_bear[i,t] ~ dnorm(0,tau.Xbear)
			logit(L_bear[i,t]) <- Xpr_bear[i,t]  	# Probability of becoming infected in year t
			P_bear[i,t] <- 1 - L_bear[i,t]		# Probability of not becoming infected in year t
		}#end time loop t
	}#end bear loop i
	
	#Likelihood of the wolf data / observations
	for(i in 1:Nwolf){
		#likelihood of data
		titer_wolf[i] ~ dbern(p_wolf[i])
		#probability of infection
		r_wolf[i] <- 1 - prod(P_wolf[i,Birth_wolf[i]:Sample_wolf[i]])
		p_wolf[i] <- Pi_wolf*r_wolf[i] + (1-r_wolf[i])*Qi_wolf
		
		for(t in 1:Ntime){
			Xpr_wolf[i,t] <- pr_wolf[t] + X_wolf[i,t]
			X_wolf[i,t] ~ dnorm(0,tau.Xwolf)
			logit(L_wolf[i,t]) <- Xpr_wolf[i,t]
			P_wolf[i,t] <- 1 - L_wolf[i,t]
		}#end time loop t
	}#end dog loop i
}#end model

# Viana model w/ AR1 and AR2
Viana_AR12 <- function(){

  # Temporal profile
  for(t in 3:Ntime){
    #Linear predictor
    pr_bear[t] ~ dnorm(beta0  + beta1*t + betaLL.1*pr_bear[t-1] +
                         betaLL.2*pr_bear[t-2] + betaDL*pr_wolf[t-1], prec_bear)
    #dummybear[t]<- exp(pr_bear[t])/(1+exp(pr_bear[t]))

    pr_wolf[t] ~ dnorm(omega0 + omega1*t + omegaDD.1*pr_wolf[t-1] +
                         omegaDD.2*pr_wolf[t-2] + omegaLD*pr_bear[t-1], prec_wolf)
    #dummywolf[t]<- exp(pr_wolf[t])/(1+exp(pr_wolf[t]))

  }#end time loop t

  #AR component t=1

  pr_bear[1]~dnorm(beta0,0.001)
  pr_bear[2]~dnorm(beta0,0.001)

  pr_wolf[1]~dnorm(omega0, 0.001)
  pr_wolf[2]~dnorm(omega0, 0.001)


  #Likelihood of the bear data / observations

  for(i in 1:Nbear){

    titer_bear[i] ~ dbern(p_bear[i])

    # probability of infection
    r_bear[i]<- 1 - prod(P_bear[i,Birth_bear[i]:Sample_bear[i]])
    # probability of detection
    p_bear[i]<-Pi_bear*r_bear[i] +(1-r_bear[i])*Qi_bear

    #add individuals error to linear predictor and logit
    for(t in 1:Ntime){

      Xpr_bear[i,t] <- pr_bear[t] + X_bear[i,t]
      X_bear[i,t] ~ dnorm(0,tau.Xbear)

      logit(L_bear[i,t]) <- Xpr_bear[i,t]  	# Probability of becoming infected in year t
      P_bear[i,t] <- 1-L_bear[i,t]		# Probability of not becoming infected in year t

    }#end time loop t

  }#end bear loop i

  #Likelihood of the DOG data / observations
  for(i in 1:Nwolf){
    #likelihood of data
    titer_wolf[i] ~ dbern(p_wolf[i])

    #probability of infection
    r_wolf[i]<- 1-prod(P_wolf[i,Birth_wolf[i]:Sample_wolf[i]])
    p_wolf[i]<-Pi_wolf*r_wolf[i] + (1-r_wolf[i])*Qi_wolf

    for(t in 3:Ntime){
      Xpr_wolf[i,t]<- pr_wolf[t] + X_wolf[i,t]
      X_wolf[i,t]~dnorm(0,tau.Xwolf)

      logit(L_wolf[i,t])<- Xpr_wolf[i,t]
      P_wolf[i,t]<-1-L_wolf[i,t]

    }#end time loop t

    #infection at t=1 & t=2
    Xpr_wolf[i,1] <- pr_wolf[1]
    logit(L_wolf[i,1])<- Xpr_wolf[i,1]
    P_wolf[i,1]<-1-L_wolf[i,1]

    Xpr_wolf[i,2] <- pr_wolf[2]
    logit(L_wolf[i,2])<- Xpr_wolf[i,2]
    P_wolf[i,2]<-1-L_wolf[i,2]
  }#end dog loop i

  # PRIORS
  #Annual precision
  prec_bear <- 1/(sigma_bear*sigma_bear)
  sigma_bear ~ dunif(0,5)
  prec_wolf <- 1/(sigma_wolf*sigma_wolf)
  sigma_wolf ~ dunif(0,5)

  #Individual precision
  tau.Xbear <- 1/(sigma.Xbear*sigma.Xbear)
  sigma.Xbear ~ dunif(0,5)
  tau.Xwolf <- 1/(sigma.Xwolf*sigma.Xwolf)
  sigma.Xwolf ~ dunif(0,5)

  #false detection
  Qi_bear ~ dbeta(0.5,25)
  Qi_wolf ~ dbeta(0.5,25)

  #detection
  Pi_bear ~ dbeta(25,0.5)
  Pi_wolf ~ dbeta(25,0.5)

  lambda<- 1/2
  #bear
  beta0 ~ dnorm(0,0.001)
  beta1 ~ dnorm(0,0.001)
  betaLL.1 ~ dnorm(0,0.1)
  betaLL.2 ~ dnorm(0,0.1)
  betaDL ~ dexp(lambda)

  #wolf
  omega0 ~ dnorm(0,0.001)
  omega1 ~ dnorm(0,0.001)
  omegaDD.1 ~ dnorm(0,0.1)
  omegaDD.2 ~ dnorm(0,0.1)
  omegaLD ~ dexp(lambda)


}#end model