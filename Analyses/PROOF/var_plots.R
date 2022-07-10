# Variable importance plots

#### Clean-up the R environment
rm(list=ls())
ls()

# Load ABC-RF regression objects

#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/reg_averageGenLoad_2_c",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/reg_logitpopstrongmsel_c",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/reg_logpopstrongselmean_c",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/reg_logPrPs_c",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/reg_loggammamean_c",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/reg_logthetaPS_c",".RData"))

#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/reg_logmu_c",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/reg_logrr_c",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/reg_logmeanNe2_c",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/reg_logncs_c",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/reg_logmeanNe2ncs_c",".RData"))

## SELECTION
pdf(file = "results/pipeline_v5/random_forests/varplot_sel.pdf", width = 9, height = 12)
par(mfrow=c(2, 3))
par(mar=c(5,5,4,1)+.1)

# PrPb
plot(x = reg_logPrPs, n.var = 20, main= expression(log[10](italic(P)[R] * italic(P)[B])))

# logit Proportion of strongly selected mutations
plot(x = reg_logitpopstrongmsel, n.var = 20, main=expression(logit(italic(P))), cex.axis = 0.5)

## gamma mean
plot(x = reg_loggammamean, n.var = 20, main=expression(log[10](italic(gamma))))

# gamma sel
plot(x = reg_logpopstrongselmean, n.var = 20, main= expression(bar(italic(s))))

# log10 Theta selected mutations
plot(x = reg_logthetaPS, n.var = 20, main=expression(log[10](italic(theta)[b])), cex.axis = 0.5)

# logit average genetic load
plot(x = reg_averageGenLoad_2, n.var = 20, main=expression(logit(italic(L))), cex.axis = 0.5)
dev.off()

## DEMOGRAPHY
pdf(file = "results/pipeline_v5/random_forests/varplot_demo.pdf", width = 9, height = 12)
par(mfrow=c(2, 3))
par(mar=c(5,5,4,1)+.1)

# log mu
plot(x = reg_logmu, n.var = 20, main=expression(log[10](italic(mu))), cex.axis = 0.5)

# log r
plot(x = reg_logrr, n.var = 20, main=expression(log[10](italic(r))))

# log N
plot(x = reg_logncs, n.var = 20, main=expression(log[10](italic(N))), cex.axis = 0.5)

# log mean Ne2
plot(x = reg_logmeanNe2, n.var = 20, main=expression(log[10](italic(N)[e])), cex.axis = 0.5)

# log Ne2/N
plot(x = reg_logmeanNe2ncs, n.var = 20, main=expression(log[10](italic(N)[e]/italic(N))), cex.axis = 0.5)
dev.off()

