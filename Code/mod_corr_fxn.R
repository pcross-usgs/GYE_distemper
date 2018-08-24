mod_corr <-function(mod){
# probably some faster apply method here. Currenly a loop: 
#only grab the years where both wolves and bears are in the dataset
	x <- as.matrix(mod$BUGSoutput$sims.list$Prob.yr.b[,26:43])
y <- as.matrix(mod$BUGSoutput$sims.list$Prob.yr.w[,26:43])
n <- dim(x)[1]
correlation <- rep(NA,n)
	for (i in 1:n){
	d <- cbind(x[i, ], y[i,])
	correlation[i] <- corr(d)
	}
return(correlation)
}