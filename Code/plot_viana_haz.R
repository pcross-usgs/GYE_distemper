plot_viana_haz <- function(mod){

  plot(inv.logit(mod$BUGSoutput$mean$pr_bear), col = "blue",
     ylab = "Probability", ylim = c(0,1))

  offset = 0.2

for(i in 1:length(mod$BUGSoutput$mean$pr_bear)){

  lines(c(i,i), c(inv.logit(mod$BUGSoutput$mean$pr_bear[i] -
                              1.96*mod$BUGSoutput$sd$pr_bear[i]),
                  inv.logit(mod$BUGSoutput$mean$pr_bear[i] +
                              1.96*mod$BUGSoutput$sd$pr_bear[i])),
        col = "blue")

  points(i + offset, inv.logit(mod$BUGSoutput$mean$pr_wolf[i]), col = "red")


  lines(c(i+ offset,i+ offset), c(inv.logit(mod$BUGSoutput$mean$pr_wolf[i] -
                              1.96*mod$BUGSoutput$sd$pr_wolf[i]),
                  inv.logit(mod$BUGSoutput$mean$pr_wolf[i] +
                              1.96*mod$BUGSoutput$sd$pr_wolf[i])),
        col = "red")
  }
}
