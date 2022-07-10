###############################################
##        Code table of content for          ##
##    the Analyses of feral Bee data set     ##
###############################################

## Table of contents
# 1. Export the Feral Bee data set observed reference table.
# 2. PCA of simulated and observed for model goodness-of-fit.
# 3. Random Forests Analyses.
# 4. Trained RF OOB, MSE and R^2 values in a table (same as in OOB plots).
# 5. Predicted Neutral simulations MSE and BIAS values in a table (for a supplementary table).
# 6. Predicted FST-NE versus NE MSE and R^2 in a table (same as in OOB plots).
# 7. Table with the posterior values and quantile for the sequenced data.
# 8. Density plots of posterior estimates for the sequenced data.
# S. Code to process the VCF files ready for the RF analyses.

# Create a vector with population names
pop_names <- c("Avalon", "Humboldt", "Davis", "Stanislaus", "Stebbins", "Riverside", "Placerita")

# 1. Export the Feral Bee data set observed reference table.
library(data.table)
source("src/Analyses/BEES/fun.R")

# Load list with observed reference table
load(file=paste0("~/My_repositories/Tracking-selection/data/ApisMellifera/globalStats_reftable/list_ObsSumStats",".RData"))

# Sanity check column names
column_names=NULL
for (i in seq(7))
{
  temp <- names(list.ObsSumStats[[i]])
  column_names <- cbind(column_names, temp)
}

colnames(column_names) <- pop_names

#write.csv(column_names,
#            file = "results/pipeline_v6_bees/sanity_check_summstats_names.csv",
#            quote = T, row.names = F)

write.table(column_names,
          file = "results/pipeline_v6_bees/sanity_check_summstats_names.txt",
          quote = T, row.names = F, col.names = T, sep = "\t")

# From list to data.frame
table_ObsSumStats <- rbindlist(list.ObsSumStats, use.names = FALSE)
colnames(table_ObsSumStats) <- scan(file = "results/pipeline_v6_bees/commom_summstats_names.txt", what = "character")

table_ObsSumStats <- data.frame(Population=pop_names,
                                table_ObsSumStats)

write.csv(table_ObsSumStats,
            file = "results/pipeline_v6_bees/summary_statistics_feral_bee_populations.csv",
            quote = T, row.names = F)


# 2. PCA of simulated and observed for model goodness-of-fit.
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/list_global_SumStats4Training",".RData"))
ls()

# Model check for Avalon population
cp_Ava <- modelCheckPCA(simSumStats = list.global.SumStats4Training[[1]],
                        obsSumStats = list.ObsSumStats[[1]], 
                        last.pc = 30,
                        name = pop_names[1])

ggsave(filename = paste0("results/pipeline_v6_bees/modelcheck_", pop_names[1],".pdf"),
       cp_Ava,
       device="pdf",
       width=9,
       height=11)

# Model check for Humboldt population
cp_Hum <- modelCheckPCA(simSumStats = list.global.SumStats4Training[[2]],
                        obsSumStats = list.ObsSumStats[[2]],
                        last.pc = 30,
                        name = pop_names[2])

ggsave(filename = paste0("results/pipeline_v6_bees/modelcheck_", pop_names[2],".pdf"),
       cp_Hum,
       device="pdf",
       width=9,
       height=11)

# Model check for Davis population
cp_Dav <- modelCheckPCA(simSumStats = list.global.SumStats4Training[[3]],
                        obsSumStats = list.ObsSumStats[[3]],
                        last.pc = 30,
                        name = pop_names[3])

ggsave(filename = paste0("results/pipeline_v6_bees/modelcheck_", pop_names[3],".pdf"),
       cp_Dav,
       device="pdf",
       width=9,
       height=11)

# Model check for Stanislaus population
cp_Sta <- modelCheckPCA(simSumStats = list.global.SumStats4Training[[4]],
                        obsSumStats = list.ObsSumStats[[4]],
                        last.pc = 30,
                        name = pop_names[4])

ggsave(filename = paste0("results/pipeline_v6_bees/modelcheck_", pop_names[4],".pdf"),
       cp_Sta,
       device="pdf",
       width=9,
       height=11)

# Model check for Stebbins population
cp_Ste <- modelCheckPCA(simSumStats = list.global.SumStats4Training[[5]],
                        obsSumStats = list.ObsSumStats[[5]],
                        last.pc = 30,
                        name = pop_names[5])

ggsave(filename = paste0("results/pipeline_v6_bees/modelcheck_", pop_names[5],".pdf"),
       cp_Ste,
       device="pdf",
       width=9,
       height=11)

# Model check for Riverside population
cp_Riv <- modelCheckPCA(simSumStats = list.global.SumStats4Training[[6]],
                        obsSumStats = list.ObsSumStats[[6]],
                        last.pc = 30,
                        name = pop_names[6])

ggsave(filename = paste0("results/pipeline_v6_bees/modelcheck_", pop_names[6],".pdf"),
       cp_Riv,
       device="pdf",
       width=9,
       height=11)

# Model check for "Placerita" population
cp_Pla <- modelCheckPCA(simSumStats = list.global.SumStats4Training[[7]],
                        obsSumStats = list.ObsSumStats[[7]],
                        last.pc = 30,
                        name = pop_names[7])

ggsave(filename = paste0("results/pipeline_v6_bees/modelcheck_", pop_names[7],".pdf"),
       cp_Pla,
       device="pdf",
       width=9,
       height=11)

# 3. Random Forests Analyses.
# see code in src/Analyses/BEES/RF_analyses.R

# 4. Trained RF OOB, MSE and R^2 values in a table (same as in OOB plots).
# see code in src/Analyses/BEES/oob_plots/combined_plot_<<parameter>>.R

# 5. Predicted Neutral simulations MSE and BIAS values in a table (for a supplementary table).
# see code in src/Analyses/BEES/oob_plots/combined_plot_<<parameter>>.R

# 6. Predicted FST-NE versus NE MSE and R^2 in a table (same as in OOB plots).
# see code in src/Analyses/BEES_/oob_plots/combined_plot_fstne.R

# 7. Table with the posterior values and quantile for the sequenced data.

# SELECTION
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_obs/list_posterior_averageGenLoad",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_obs/list_posterior_popstrongmsel",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_obs/list_posterior_logthetaPS",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_obs/list_posterior_loggammamean",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_obs/list_posterior_logPrPs",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_obs/list_posterior_popstrongselmean",".RData"))

# DEMOGRAPHY
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_obs/list_posterior_logmeanNe2",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_obs/list_posterior_logncs",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_obs/list_posterior_logmeanNe2ncs",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_obs/list_posterior_logmu",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_obs/list_posterior_logrr",".RData"))

# Load
L_row = matrix(data = NA, nrow = 1, ncol = 7)
for (i in 1:7){
  #print(formatC(inv.logit(list.posterior.averageGenLoad[[i]]$quantile), format = "e", digits = 2))
  L_row[, i] <- paste(formatC(inv.logit(list.posterior.averageGenLoad[[i]]$quantile), format = "e", digits = 2), collapse=" ")
}; rm(list.posterior.averageGenLoad)

# Pstrong
P_row = matrix(data = NA, nrow = 1, ncol = 7)
for (i in 1:7){
  print(formatC(inv.logit(list.posterior.popstrongmsel[[i]]$quantiles), format = "e", digits = 2))
  P_row[, i] <- paste(formatC(inv.logit(list.posterior.popstrongmsel[[i]]$quantile), format = "e", digits = 2), collapse=" ")
}; rm(list.posterior.popstrongmsel)

# Theta_b
thetab_row = matrix(data = NA, nrow = 1, ncol = 7)
for (i in 1:7){
  print(formatC(10^(list.posterior.logthetaPS[[i]]$quantiles), format = "e", digits = 2))
  thetab_row[, i] <- paste(formatC(10^(list.posterior.logthetaPS[[i]]$quantile), format = "e", digits = 2), collapse=" ")
}; rm(list.posterior.logthetaPS)

# gamma
gamma_row = matrix(data = NA, nrow = 1, ncol = 7)
for (i in 1:7){
  print(formatC(10^(list.posterior.loggammamean[[i]]$quantiles), format = "e", digits = 2))
  gamma_row[, i] <- paste(formatC(10^(list.posterior.loggammamean[[i]]$quantile), format = "e", digits = 2), collapse=" ")
}; rm(list.posterior.loggammamean)

# prps
prps_row = matrix(data = NA, nrow = 1, ncol = 7)
for (i in 1:7){
  print(formatC(10^(list.posterior.logPrPs[[i]]$quantiles), format = "e", digits = 2))
  prps_row[, i] <- paste(formatC(10^(list.posterior.logPrPs[[i]]$quantile), format = "e", digits = 2), collapse=" ")
}; rm(list.posterior.logPrPs)

# gmean
gmean_row = matrix(data = NA, nrow = 1, ncol = 7)
for (i in 1:7){
  print(formatC(10^(list.posterior.popstrongselmean[[i]]$quantiles), format = "e", digits = 2))
  gmean_row[, i] <- paste(formatC(10^(list.posterior.popstrongselmean[[i]]$quantile), format = "e", digits = 2), collapse=" ")
}; rm(list.posterior.popstrongselmean)

# ne
ne_row = matrix(data = NA, nrow = 1, ncol = 7)
for (i in 1:7){
  print(formatC(10^(list.posterior.logmeanNe2[[i]]$quantiles), format = "e", digits = 2))
  ne_row[, i] <- paste(formatC(10^(list.posterior.logmeanNe2[[i]]$quantile), format = "e", digits = 2), collapse=" ")
}; rm(list.posterior.logmeanNe2)

# n
n_row = matrix(data = NA, nrow = 1, ncol = 7)
for (i in 1:7){
  print(formatC(10^(list.posterior.logncs[[i]]$quantiles), format = "e", digits = 2))
  n_row[, i] <- paste(formatC(10^(list.posterior.logncs[[i]]$quantile), format = "e", digits = 2), collapse=" ")
}; rm(list.posterior.logncs)

# nen
nen_row = matrix(data = NA, nrow = 1, ncol = 7)
for (i in 1:7){
  print(formatC(10^(list.posterior.logmeanNe2ncs[[i]]$quantiles), format = "e", digits = 2))
  nen_row[, i] <- paste(formatC(10^(list.posterior.logmeanNe2ncs[[i]]$quantile), format = "e", digits = 2), collapse=" ")
}; rm(list.posterior.logmeanNe2ncs)

# mu
mu_row = matrix(data = NA, nrow = 1, ncol = 7)
for (i in 1:7){
  print(formatC(10^(list.posterior.logmu[[i]]$quantiles), format = "e", digits = 2))
  mu_row[, i] <- paste(formatC(10^(list.posterior.logmu[[i]]$quantile), format = "e", digits = 2), collapse=" ")
}; rm(list.posterior.logmu)

# r
r_row = matrix(data = NA, nrow = 1, ncol = 7)
for (i in 1:7){
  print(formatC(10^(list.posterior.logrr[[i]]$quantiles), format = "e", digits = 2))
  r_row[, i] <- paste(formatC(10^(list.posterior.logrr[[i]]$quantile), format = "e", digits = 2), collapse=" ")
}; rm(list.posterior.logrr)

# Obtain the quantile for FST-NE (from intra-locus FST estimates)

# New function that get the quantiles from the intra-locus estimates
quantileFSTNe <- function(x, tau){
  fst  <- x
  fst[which(fst < 0)] <- NA
  ne <- (tau*(1-fst))/(4*fst)
  q <- quantile(ne, probs = c(0.025, 0.5, 0.975), na.rm=T)
  r <- paste(formatC(q, format = "e", digits = 2), collapse=" ")
  return(r)
}

# List of taus
tau = c(104,49,47,38,18,15,15)

# List of input files
pop.names <- c("avalon", "humboldt", "davis", "stanislaus", "stebbins", "riverside", "placerita")
file.names <- paste0("~/My_repositories/Tracking-selection/data/ApisMellifera/egglib/", pop.names, "_summstats.txt")

# Create a list with each egglib output
datalist_egglib <- lapply(file.names, function(x){read.table(file= x, header=T, na.strings = "NA")})

# EXPORT TABLE WITH QUANTILES
fstne_row = matrix(data = NA, nrow = 1, ncol = 7)
for (i in 1:7){
  print(quantileFSTNe(datalist_egglib[[i]]$LSS.WCst, tau[i]))
  fstne_row[, i] <- quantileFSTNe(datalist_egglib[[i]]$LSS.WCst, tau[i]) # correct[check made on 30/JUN/22]
}

# Combine all parameters (rows)
combined_rows <- rbind(prps_row, P_row, gamma_row, gmean_row, thetab_row, L_row, 
                       mu_row, r_row, n_row, ne_row, nen_row, fstne_row)

colnames(combined_rows) <- c("Avalon", "Humboldt", "Davis", "Stanislaus", "Stebbins", "Riverside", "Placerita")
rownames(combined_rows) <- c("$P_RP_B$", "$P$", "$\\gamma$", "$\\bar{s}$", "$\\theta_{\\mathrm{b}}$", "$L$",
                             "$\\mu$", "$r$", "$N$", "$N_{\\mathrm{e}}$", "$N_{\\mathrm{e}}/N$", "$F_{\\mathrm{ST}}$-NE") # parameters in Latex code

# Export table as latex txt
write.table(combined_rows, 
            file = "~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_obs/combined_posterior_estimates.txt",
            row.names = T, col.names = T, quote = F)

# 8. Density plots of posterior estimates for the sequenced data.
# see code in src/Analyses/BEES/density_plots.R

# S. Code to process the VCF files ready for the RF analyses.
# see code in src/Analyses/BEES/utils