# Prior Histograms

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

pdf(file = "results/pipeline_v5/random_forests/parameters_histograms.pdf", width = 9, height = 12)
par(mfrow=c(2, 6))
par(mar=c(5,5,4,1)+.1)

# log mu
hist(logmu, freq = TRUE, xlab = expression(log[10](italic(mu))), main = "", col = "#bdbdbd")

# log r
hist(logrr, freq = TRUE, xlab = expression(log[10](italic(c)[0])), main = "", col = "#bdbdbd")

# log N
hist(logncs, freq = TRUE, xlab = expression(log[10](italic(N))), main = "", col = "#bdbdbd")

# log PrPb
hist(logPrPs, freq = TRUE, xlab = expression(log[10](italic(P)[R] * italic(P)[B])), main = "", col = "#bdbdbd")

# log gamma mean
hist(loggammamean, freq = TRUE, xlab = expression(log[10](italic(gamma))), main = "", col = "#bdbdbd")

# logit Proportion of strongly selected mutations
hist(logitpopstrongmsel, freq = TRUE, xlab = expression(logit(italic(P))), main = "", col = "#bdbdbd")

# gamma sel
hist(logpopStrongSelMean, freq = TRUE, xlab = expression(bar(italic(s))), main = "", col = "#bdbdbd")

# log10 Theta selected mutations
hist(logthetaPS, freq = TRUE, xlab = expression(log[10](italic(theta)[b])), main = "", col = "#bdbdbd")

# logit average genetic load
hist(logitaverageGenload, freq = TRUE, xlab = expression(logit(italic(L))), main = "", col = "#bdbdbd")

# log mean Ne2
hist(logmeanNe2, freq = TRUE, xlab = expression(log[10](italic(N)[e])) , main = "", col = "#bdbdbd")

# log Ne2/NCS
hist(logmeanNe2ncs, freq = TRUE, xlab = expression(log[10](italic(N)[e]/italic(N))), main = "", col = "#bdbdbd")
dev.off()
