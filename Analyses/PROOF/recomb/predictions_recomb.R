####################
##  Script to RF  ##
####################

rm(list=ls())
ls()

print("Preparing the environment for the analysis ...")

# Load package
library(abcrf) #1.7.1
#library(ranger) # 0.11.1
library(dplyr)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(gtools) # for inv.logit

source("/Users/correapavinato.1/My_repositories/Tracking-selection/src/Analyses/PROOF/recomb/fun.R")
#setwd("./")
setwd("/Users/correapavinato.1/My_repositories/Tracking-selection/results/pipeline_v5/recomb_pods")

OutDir = "posteriors"

## Prepare recombination PODs reference table 
##
## Load reference table that was used to train the random forests
##path_reftable = "/Users/correapavinato.1/My_repositories/Tracking-selection/results/pipeline_v5/pooled_reftable_total_c.RData"
##load(file=path_reftable)
##
##write(colnames(pooled_reftable_total), file="feature_comb_reftable.txt")
##rm(path_reftable)
##rm(pooled_reftable_total)
##
## Load training reference table features
##rf_features <- readLines("feature_comb_reftable.txt")
##
## Load reference tables with variable recombination rates PODs
##load("pooled_reftable_0.RData")
##raw_recombpods_reftable_0 <- raw_reftable
##rm(raw_reftable)
##
##load("pooled_reftable_1.RData")
##raw_recombpods_reftable_1 <- pooled_raw_reftable
##rm(pooled_raw_reftable)
##
##load("pooled_reftable_2.RData")
##raw_recombpods_reftable_2 <- pooled_raw_reftable
##rm(pooled_raw_reftable)
##
## Combine the PODs reference tble
##raw_recombpods_reftable <- rbind(raw_recombpods_reftable_0,
##                                 raw_recombpods_reftable_1,
##                                 raw_recombpods_reftable_2)
##
##rm(raw_recombpods_reftable_0)
##rm(raw_recombpods_reftable_1)
##rm(raw_recombpods_reftable_2)
##
## Keep only features in common between the reference tables for training and prediction
##recombpods_reftable <- raw_recombpods_reftable[, names(raw_recombpods_reftable) %in% rf_features]
##rm(raw_recombpods_reftable)
##
## Sanity check if the features names are the same and in the same order
##sum(names(recombpods_reftable) == names(pooled_reftable_total)) == 509 # if pooled_reftable_total is still loaded
##sum(names(recombpods_reftable) == rf_features) == 509
##
## Remove any other row with missing data
##recombpods_reftable <- recombpods_reftable[complete.cases(recombpods_reftable), ]
##
## Sample only 1,000 PODs for prediction
##set.seed(12345)
##recombpods_reftable <- sample_n(recombpods_reftable, 1000)  
##
## Re-write PODs ids
##recombpods_reftable$sim <- seq(1:1000)
##head(recombpods_reftable$sim)
##
## Save recombination PODs reference table
##save(recombpods_reftable, file = "recombpods_reftable.RData")

# Load recombination PODs reference table
#load(file = "recombpods_reftable.RData")
#
#exp_parameters <- recombpods_reftable[,c(1:43)]
#obs_sumstats <- recombpods_reftable[,-c(1:104)]
#rm(recombpods_reftable)

# Parameter names
names_parameters <- c("prps","gamma","theta_b",
                      "mu","rr","ne","nc","ne_nc",
                      "pstrong","gamma_s","load","FST-NE")

# RF names
names_rfs <- c("reg_logPrPs_c", "reg_loggammamean_c", "reg_logthetaPS_c", 
               "reg_logmu_c", "reg_logrr_c", "reg_logmeanNe2_c","reg_logncs_c", "reg_logmeanNe2ncs_c",
               "reg_logitpopstrongmsel_c", "reg_logpopstrongselmean_c","reg_averageGenLoad_2_c", "estimated.fstNe2_c")

# Tables containing training observed and oob predicted values names
names_oobs <- c("oob.logPrPs_c", "oob.loggammamean_c", "oob.logthetaPS_c", 
                "oob.logmu_c", "oob.logrr_c", "oob.logmeanNe2_c", "oob.logncs_c", "oob.logmeanNe2ncs_c",
                "oob.lgtps_c","oob.logpopstrongselmean_c", "oob.lgtgl_c", "estimated.fstNe2_c")

# Sumstats tables names
sumstats_tables <- c(rep("global_sumstats",8),
                     "global_sumstats_popstrongmsel", "global_sumstats_popstrongselmean",
                     "global_sumstats_averageGenLoad", "global_sumstats")


# Parameter mapping
parmMap <- data.frame(parameters=names_parameters, 
                      regressions=names_rfs, 
                      oobs=names_oobs,
                      sumstats=sumstats_tables)
# Clean-up memory
rm(names_parameters)
rm(names_rfs)
rm(names_oobs)
rm(sumstats_tables)

## Load the trainig reftable summary statistics
#path_sumstats = "~/My_repositories/Tracking-selection/results/pipeline_v5/"
#path_rfs = "~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/"
#path_oobs = "~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/"

## Run RF prediction

print("Loading global summstats ...")
### RF trainined with global summstats
###--------------------------------------
#load(file=paste0(path_sumstats,"global_sumstats_c.RData"))

## PrPs
##-----
print("Running PrPS ...")
par = "prps"
## Check if outdir exists
#if (!file_test("-d", paste0(OutDir,"/",par))){
#  dir.create(file.path(paste0(OutDir,"/",par)), recursive = T)
#}
#
# Load files for prediction
#load(file = paste0(path_rfs, parmMap[which(parmMap$parameters == par), 2], ".RData"))
#load(file = paste0(path_oobs, parmMap[which(parmMap$parameters == par), 3], ".RData"))
#
# Rename vector of expected values used for training
#logPrPs = oob.logPrPs$x
#
# RF prediction
#log_posterior <- predict(object     = reg_logPrPs,
#                         obs        = obs_sumstats,
#                         training   = data.frame(logPrPs, global_sumstats),
#                         quantiles  = c(0.025,0.5,0.975),
#                         paral      = T,
#                         ncores     = 6,
#                         rf.weights = T)
#
## Save posteriors
#save(log_posterior, 
#     file = paste0(OutDir, "/",par,"/","posterior_", par, ".RData"))
#
# Clean-up memory
#rm(oob.logPrPs)
#rm(logPrPs)
#rm(reg_logPrPs)
#
## Export posterior estimates table
#posterior_table <- data.frame(x=log10(exp_parameters[, "PrGWSel"] * exp_parameters[, "PrMSel"]), 
#                              y=log_posterior$expectation)
#
#save(posterior_table, 
#     file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))
load(file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))

# Summary statistics of performance prediction 
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse"] <- mse.fun(posterior_table)

# R^2
parmMap[which(parmMap$parameters == par), "rsquared"] <- rsquared.fun(posterior_table)

# Mean bias - log scale
parmMap[which(parmMap$parameters == par), "bias"] <- bias.fun(posterior_table)

# Clean-up memory
#rm(log_posterior)
rm(posterior_table)
print("DONE!")

## Gamma Mean
##-----
print("Running gamma...")
par = "gamma"
# Check if outdir exists
#if (!file_test("-d", paste0(OutDir,"/",par))){
#  dir.create(file.path(paste0(OutDir,"/",par)), recursive = T)
#}
#
## Load files for prediction
#load(file = paste0(path_rfs, parmMap[which(parmMap$parameters == par), 2], ".RData"))
#load(file = paste0(path_oobs, parmMap[which(parmMap$parameters == par), 3], ".RData"))
#
## Rename vector of expected values used for training
#loggammamean = oob.loggammamean$x
#
## RF prediction
#log_posterior <- predict(object     = reg_loggammamean,
#                         obs        = obs_sumstats,
#                         training   = data.frame(loggammamean, global_sumstats),
#                         quantiles  = c(0.025,0.5,0.975),
#                         paral      = T,
#                         ncores     = 6,
#                         rf.weights = T)
#
## Save posteriors
#save(log_posterior, 
#     file = paste0(OutDir, "/",par,"/","posterior_", par, ".RData"))
#
## Clean-up memory
#rm(oob.loggammamean)
#rm(loggammamean)
#rm(reg_loggammamean)
#
## Export posterior estimates table
#posterior_table <- data.frame(x=log10(exp_parameters[, "gammaMean"]), 
#                              y=log_posterior$expectation)
#
#save(posterior_table, 
#     file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))
load(file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))

# Summary statistics of performance prediction 
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse"] <- mse.fun(posterior_table)

# R^2
parmMap[which(parmMap$parameters == par), "rsquared"] <- rsquared.fun(posterior_table)

# Mean bias - log scale
parmMap[which(parmMap$parameters == par), "bias"] <- bias.fun(posterior_table)

# Clean-up memory
#rm(log_posterior)
rm(posterior_table)
print("DONE!")

## Theta_b
##-----
print("Running theta_b...")
par = "theta_b"
## Check if outdir exists
#if (!file_test("-d", paste0(OutDir,"/",par))){
#  dir.create(file.path(paste0(OutDir,"/",par)), recursive = T)
#}
#
## Load files for prediction
#load(file = paste0(path_rfs, parmMap[which(parmMap$parameters == par), 2], ".RData"))
#load(file = paste0(path_oobs, parmMap[which(parmMap$parameters == par), 3], ".RData"))
#
## Rename vector of expected values used for training
#logthetaPS = oob.logthetaPS$x
#
## RF prediction
#log_posterior <- predict(object     = reg_logthetaPS,
#                         obs        = obs_sumstats,
#                         training   = data.frame(logthetaPS, global_sumstats),
#                         quantiles  = c(0.025,0.5,0.975),
#                         paral      = T,
#                         ncores     = 6,
#                         rf.weights = T)
#
## Save posteriors
#save(log_posterior, 
#     file = paste0(OutDir, "/",par,"/","posterior_", par, ".RData"))
#
## Clean-up memory
#rm(oob.logthetaPS)
#rm(logthetaPS)
#rm(reg_logthetaPS)
#
## Export posterior estimates table
#posterior_table <- data.frame(x=log10(4 * exp_parameters[, "Ncs"] * 
#                                         (exp_parameters[, "mu"] * 
#                                          exp_parameters[, "PrGWSel"] * 
#                                          exp_parameters[, "PrMSel"]) * 100e+6), 
#                              y=log_posterior$expectation)
#
#save(posterior_table, 
#     file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))
load(file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))

# Summary statistics of performance prediction 
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse"] <- mse.fun(posterior_table)

# R^2
parmMap[which(parmMap$parameters == par), "rsquared"] <- rsquared.fun(posterior_table)

# Mean bias - log scale
parmMap[which(parmMap$parameters == par), "bias"] <- bias.fun(posterior_table)

# Clean-up memory
#rm(log_posterior)
rm(posterior_table)
print("DONE!")

## mu
##-----
print("Running mu...")
par = "mu"
## Check if outdir exists
#if (!file_test("-d", paste0(OutDir,"/",par))){
#  dir.create(file.path(paste0(OutDir,"/",par)), recursive = T)
#}
#
## Load files for prediction
#load(file = paste0(path_rfs, parmMap[which(parmMap$parameters == par), 2], ".RData"))
#load(file = paste0(path_oobs, parmMap[which(parmMap$parameters == par), 3], ".RData"))
#
## Rename vector of expected values used for training
#logmu = oob.logmu$x
#
## RF prediction
#log_posterior <- predict(object     = reg_logmu,
#                         obs        = obs_sumstats,
#                         training   = data.frame(logmu, global_sumstats),
#                         quantiles  = c(0.025,0.5,0.975),
#                         paral      = T,
#                         ncores     = 6,
#                         rf.weights = T)
#
## Save posteriors
#save(log_posterior, 
#     file = paste0(OutDir, "/",par,"/","posterior_", par, ".RData"))
#
## Clean-up memory
#rm(oob.logmu)
#rm(logmu)
#rm(reg_logmu)
#
## Export posterior estimates table
#posterior_table <- data.frame(x=log10(exp_parameters[, "mu"]), 
#                              y=log_posterior$expectation)
#
#save(posterior_table, 
#     file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))
load(file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))

# Summary statistics of performance prediction 
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse"] <- mse.fun(posterior_table)

# R^2
parmMap[which(parmMap$parameters == par), "rsquared"] <- rsquared.fun(posterior_table)

# Mean bias - log scale
parmMap[which(parmMap$parameters == par), "bias"] <- bias.fun(posterior_table)

# Clean-up memory
#rm(log_posterior)
rm(posterior_table)
print("DONE!")

## rr
##-----
print("Running rr...")
par = "rr"
## Check if outdir exists
#if (!file_test("-d", paste0(OutDir,"/",par))){
#  dir.create(file.path(paste0(OutDir,"/",par)), recursive = T)
#}
#
## Load files for prediction
#load(file = paste0(path_rfs, parmMap[which(parmMap$parameters == par), 2], ".RData"))
#load(file = paste0(path_oobs, parmMap[which(parmMap$parameters == par), 3], ".RData"))
#
## Rename vector of expected values used for training
#logrr = oob.logrr$x
#
## RF prediction
#log_posterior <- predict(object     = reg_logrr,
#                         obs        = obs_sumstats,
#                         training   = data.frame(logrr, global_sumstats),
#                         quantiles  = c(0.025,0.5,0.975),
#                         paral      = T,
#                         ncores     = 6,
#                         rf.weights = T)
#
## Save posteriors
#save(log_posterior, 
#     file = paste0(OutDir, "/",par,"/","posterior_", par, ".RData"))
#
## Clean-up memory
#rm(oob.logrr)
#rm(logrr)
#rm(reg_logrr)
#
## Export posterior estimates table
#posterior_table <- data.frame(x=log10(exp_parameters[, "rr"]), 
#                              y=log_posterior$expectation)
#
#save(posterior_table, 
#     file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))
load(file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))

# Summary statistics of performance prediction 
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse"] <- mse.fun(posterior_table)

# R^2
parmMap[which(parmMap$parameters == par), "rsquared"] <- rsquared.fun(posterior_table)

# Mean bias - log scale
parmMap[which(parmMap$parameters == par), "bias"] <- bias.fun(posterior_table)

# Clean-up memory
#rm(log_posterior)
rm(posterior_table)
print("DONE!")

## ne
##-----
print("Running ne...")
par = "ne"
## Check if outdir exists
#if (!file_test("-d", paste0(OutDir,"/",par))){
#  dir.create(file.path(paste0(OutDir,"/",par)), recursive = T)
#}
#
## Load files for prediction
#load(file = paste0(path_rfs, parmMap[which(parmMap$parameters == par), 2], ".RData"))
#load(file = paste0(path_oobs, parmMap[which(parmMap$parameters == par), 3], ".RData"))
#
## Rename vector of expected values used for training
#logmeanNe2 = oob.logmeanNe2$x
#
## RF prediction
#log_posterior <- predict(object     = reg_logmeanNe2,
#                         obs        = obs_sumstats,
#                         training   = data.frame(logmeanNe2, global_sumstats),
#                         quantiles  = c(0.025,0.5,0.975),
#                         paral      = T,
#                         ncores     = 6,
#                         rf.weights = T)
#
## Save posteriors
#save(log_posterior, 
#     file = paste0(OutDir, "/",par,"/","posterior_", par, ".RData"))
#
## Clean-up memory
#rm(oob.logmeanNe2)
#rm(logmeanNe2)
#rm(reg_logmeanNe2)
#
## Export posterior estimates table
#posterior_table <- data.frame(x=log10(exp_parameters[, "meanNe2"]), 
#                              y=log_posterior$expectation)
#
#save(posterior_table, 
#     file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))
load(file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))

# Summary statistics of performance prediction 
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse"] <- mse.fun(posterior_table)

# R^2
parmMap[which(parmMap$parameters == par), "rsquared"] <- rsquared.fun(posterior_table)

# Mean bias - log scale
parmMap[which(parmMap$parameters == par), "bias"] <- bias.fun(posterior_table)

# Clean-up memory
#rm(log_posterior)
rm(posterior_table)
print("DONE!")

## nc
##-----
print("Running nc...")
par = "nc"
## Check if outdir exists
#if (!file_test("-d", paste0(OutDir,"/",par))){
#  dir.create(file.path(paste0(OutDir,"/",par)), recursive = T)
#}
#
## Load files for prediction
#load(file = paste0(path_rfs, parmMap[which(parmMap$parameters == par), 2], ".RData"))
#load(file = paste0(path_oobs, parmMap[which(parmMap$parameters == par), 3], ".RData"))
#
## Rename vector of expected values used for training
#logncs = oob.logncs$x
#
## RF prediction
#log_posterior <- predict(object     = reg_logncs,
#                         obs        = obs_sumstats,
#                         training   = data.frame(logncs, global_sumstats),
#                         quantiles  = c(0.025,0.5,0.975),
#                         paral      = T,
#                         ncores     = 6,
#                         rf.weights = T)
#
## Save posteriors
#save(log_posterior, 
#     file = paste0(OutDir, "/",par,"/","posterior_", par, ".RData"))
#
## Clean-up memory
#rm(oob.logncs )
#rm(logncs)
#rm(reg_logncs)
#
## Export posterior estimates table
#posterior_table <- data.frame(x=log10(exp_parameters[, "Ncs"]), 
#                              y=log_posterior$expectation)
#
#save(posterior_table, 
#     file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))
load(file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))

# Summary statistics of performance prediction 
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse"] <- mse.fun(posterior_table)

# R^2
parmMap[which(parmMap$parameters == par), "rsquared"] <- rsquared.fun(posterior_table)

# Mean bias - log scale
parmMap[which(parmMap$parameters == par), "bias"] <- bias.fun(posterior_table)

# Clean-up memory
#rm(log_posterior)
rm(posterior_table)
print("DONE!")

## ne_nc
##-----
print("Running ne/nc...")
par = "ne_nc"
## Check if outdir exists
#if (!file_test("-d", paste0(OutDir,"/",par))){
#  dir.create(file.path(paste0(OutDir,"/",par)), recursive = T)
#}
#
## Load files for prediction
#load(file = paste0(path_rfs, parmMap[which(parmMap$parameters == par), 2], ".RData"))
#load(file = paste0(path_oobs, parmMap[which(parmMap$parameters == par), 3], ".RData"))
#
## Rename vector of expected values used for training
#logmeanNe2ncs = oob.logmeanNe2ncs$x
#
## RF prediction
#log_posterior <- predict(object     = reg_logmeanNe2ncs,
#                         obs        = obs_sumstats,
#                         training   = data.frame(logmeanNe2ncs, global_sumstats),
#                         quantiles  = c(0.025,0.5,0.975),
#                         paral      = T,
#                         ncores     = 6,
#                         rf.weights = T)
#
## Save posteriors
#save(log_posterior, 
#     file = paste0(OutDir, "/",par,"/","posterior_", par, ".RData"))
#
## Clean-up memory
#rm(oob.logmeanNe2ncs)
#rm(logmeanNe2ncs)
#rm(reg_logmeanNe2ncs)
#
## Export posterior estimates table
#posterior_table <- data.frame(x=log10(exp_parameters[, "meanNe2"]/exp_parameters[, "Ncs"]), 
#                              y=log_posterior$expectation)
#
#save(posterior_table, 
#     file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))
load(file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))

# Summary statistics of performance prediction 
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse"] <- mse.fun(posterior_table)

# R^2
parmMap[which(parmMap$parameters == par), "rsquared"] <- rsquared.fun(posterior_table)

# Mean bias - log scale
parmMap[which(parmMap$parameters == par), "bias"] <- bias.fun(posterior_table)

# Clean-up memory
#rm(log_posterior)
rm(posterior_table)
print("DONE!")

## pstrong
##-----
print("Running pstrong...")
#load(file=paste0(path_sumstats,"global_sumstats_popstrongmsel_c.RData"))

par = "pstrong"
# Check if outdir exists
#if (!file_test("-d", paste0(OutDir,"/",par))){
#  dir.create(file.path(paste0(OutDir,"/",par)), recursive = T)
#}
#
## Load files for prediction
#load(file = paste0(path_rfs, parmMap[which(parmMap$parameters == par), 2], ".RData"))
#load(file = paste0(path_oobs, parmMap[which(parmMap$parameters == par), 3], ".RData"))
#
## Rename vector of expected values used for training
#logitpopstrongmsel = oob.lgtps$x
#
## RF prediction
#log_posterior <- predict(object     = reg_logitpopstrongmsel,
#                         obs        = obs_sumstats,
#                         training   = data.frame(logitpopstrongmsel, global_sumstats_popstrongmsel),
#                         quantiles  = c(0.025,0.5,0.975),
#                         paral      = T,
#                         ncores     = 6,
#                         rf.weights = T)
#
## Save posteriors
#save(log_posterior, 
#     file = paste0(OutDir, "/",par,"/","posterior_", par, ".RData"))
#
## Clean-up memory
#rm(oob.lgtps)
#rm(logitpopstrongmsel)
#rm(reg_logitpopstrongmsel)
#
## Export posterior estimates table
## Add a smaller value for -Inf
#tempvalues <- log(exp_parameters[, "POPPrStrongMSel"]/(1-exp_parameters[, "POPPrStrongMSel"]))
#tempvalues <- replace(tempvalues, tempvalues == -Inf, -17.5)
#
#posterior_table <- data.frame(x=tempvalues,
#                              y=log_posterior$expectation)
#
#save(posterior_table, 
#     file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))
load(file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))

# Separate values for neutral and non-neutral PODs
neutral_posteriors <- posterior_table$x == -17.5

# Neutral
neutral_posterior_table <- data.frame(x = 0, y= inv.logit(posterior_table[neutral_posteriors, "y"]))

# Non-neutral 
nonneutral_posterior_table <- posterior_table[!neutral_posteriors, ]

# Summary statistics of performance prediction of non-Neutral PODs
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse"] <- mse.fun(nonneutral_posterior_table)

# R^2
parmMap[which(parmMap$parameters == par), "rsquared"] <- rsquared.fun(nonneutral_posterior_table)

# Mean bias - log scale
parmMap[which(parmMap$parameters == par), "bias"] <- bias.fun(nonneutral_posterior_table)

# Summary statistics of performance prediction of Neutral PODs
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse0"] <- mse.fun(neutral_posterior_table)

# Mean bias - natural scale
parmMap[which(parmMap$parameters == par), "bias0"] <- bias.fun(neutral_posterior_table)

# Clean-up memory
#rm(log_posterior)
rm(posterior_table)
rm(neutral_posteriors)
rm(neutral_posterior_table)
rm(nonneutral_posterior_table)
print("DONE!")

## gamma_s
##-----
print("Running gamma_s...")
#load(file=paste0(path_sumstats,"global_sumstats_popstrongselmean_c.RData"))

par = "gamma_s"
## Check if outdir exists
#if (!file_test("-d", paste0(OutDir,"/",par))){
#  dir.create(file.path(paste0(OutDir,"/",par)), recursive = T)
#}
#
## Load files for prediction
#load(file = paste0(path_rfs, parmMap[which(parmMap$parameters == par), 2], ".RData"))
#load(file = paste0(path_oobs, parmMap[which(parmMap$parameters == par), 3], ".RData"))
#
## Rename vector of expected values used for training
#logpopStrongSelMean = oob.logpopstrongselmean$x
#
## RF prediction
#log_posterior <- predict(object     = reg_logpopstrongselmean,
#                         obs        = obs_sumstats,
#                         training   = data.frame(logpopStrongSelMean, global_sumstats_popstrongselmean),
#                         quantiles  = c(0.025,0.5,0.975),
#                         paral      = T,
#                         ncores     = 6,
#                         rf.weights = T)
#
## Save posteriors
#save(log_posterior, 
#     file = paste0(OutDir, "/",par,"/","posterior_", par, ".RData"))
#
## Clean-up memory
#rm(oob.logpopstrongselmean)
#rm(logpopStrongSelMean)
#rm(reg_logpopstrongselmean)
#
## Export posterior estimates table
## Add a smaller value for -Inf
#tempvalues <- log10(exp_parameters[, "POPStrongSelMean"])
#tempvalues <- replace(tempvalues, tempvalues == -Inf, -4)
#
#posterior_table <- data.frame(x=tempvalues,
#                              y=log_posterior$expectation)
#
#save(posterior_table, 
#     file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))
load(file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))

# Separate values for neutral and non-neutral PODs
neutral_posteriors <- posterior_table$x == -4

# Neutral
neutral_posterior_table <- data.frame(x = 0, y= 10^(posterior_table[neutral_posteriors, "y"]))

# Non-neutral 
nonneutral_posterior_table <- posterior_table[!neutral_posteriors, ]

# Summary statistics of performance prediction of non-Neutral PODs
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse"] <- mse.fun(nonneutral_posterior_table)

# R^2
parmMap[which(parmMap$parameters == par), "rsquared"] <- rsquared.fun(nonneutral_posterior_table)

# Mean bias - log scale
parmMap[which(parmMap$parameters == par), "bias"] <- bias.fun(nonneutral_posterior_table)

# Summary statistics of performance prediction of Neutral PODs
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse0"] <- mse.fun(neutral_posterior_table)

# Mean bias - natural scale
parmMap[which(parmMap$parameters == par), "bias0"] <- bias.fun(neutral_posterior_table)

# Clean-up memory
#rm(log_posterior)
rm(posterior_table)
rm(neutral_posteriors)
rm(neutral_posterior_table)
rm(nonneutral_posterior_table)
print("DONE!")


## load
##-----
print("Running load...")
#load(file=paste0(path_sumstats,"global_sumstats_averageGenLoad_c.RData"))

par = "load"
## Check if outdir exists
#if (!file_test("-d", paste0(OutDir,"/",par))){
#  dir.create(file.path(paste0(OutDir,"/",par)), recursive = T)
#}
#
## Load files for prediction
#load(file = paste0(path_rfs, parmMap[which(parmMap$parameters == par), 2], ".RData"))
#load(file = paste0(path_oobs, parmMap[which(parmMap$parameters == par), 3], ".RData"))
#
## Rename vector of expected values used for training
#logitaverageGenload = oob.lgtgl$x
#
## RF prediction
#log_posterior <- predict(object     = reg_averageGenLoad_2,
#                         obs        = obs_sumstats,
#                         training   = data.frame(logitaverageGenload, global_sumstats_averageGenLoad),
#                         quantiles  = c(0.025,0.5,0.975),
#                         paral      = T,
#                         ncores     = 6,
#                         rf.weights = T)
#
## Save posteriors
#save(log_posterior, 
#     file = paste0(OutDir, "/",par,"/","posterior_", par, ".RData"))
#
## Clean-up memory
#rm(oob.lgtgl)
#rm(logitaverageGenload)
#rm(reg_averageGenLoad_2)
#
## Export posterior estimates table
## Add a smaller value for -Inf
#tempvalues <- log(exp_parameters[, "averageGeneticLoad"]/(1-exp_parameters[, "averageGeneticLoad"]))
#tempvalues <- replace(tempvalues, tempvalues == -Inf, -37.5)
#
#posterior_table <- data.frame(x=tempvalues,
#                              y=log_posterior$expectation)
#
#save(posterior_table, 
#     file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))
load(file = paste0(OutDir, "/",par,"/","posterior_table_", par, ".RData"))

# Separate values for neutral and non-neutral PODs
neutral_posteriors <- posterior_table$x == -37.5

# Neutral
neutral_posterior_table <- data.frame(x = 0, y= inv.logit(posterior_table[neutral_posteriors, "y"]))

# Non-neutral 
nonneutral_posterior_table <- posterior_table[!neutral_posteriors, ]

# Summary statistics of performance prediction of non-Neutral PODs
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse"] <- mse.fun(nonneutral_posterior_table)

# R^2
parmMap[which(parmMap$parameters == par), "rsquared"] <- rsquared.fun(nonneutral_posterior_table)

# Mean bias - logit scale
parmMap[which(parmMap$parameters == par), "bias"] <- bias.fun(nonneutral_posterior_table)

# Summary statistics of performance prediction of Neutral PODs
# Root mean squared error natural scale
parmMap[which(parmMap$parameters == par), "mse0"] <- mse.fun(neutral_posterior_table)

# Mean bias - natural scale
parmMap[which(parmMap$parameters == par), "bias0"] <- bias.fun(neutral_posterior_table)

# Clean-up memory
#rm(log_posterior)
rm(posterior_table)
rm(neutral_posteriors)
rm(neutral_posterior_table)
rm(nonneutral_posterior_table)
print("DONE!")

## FST-NE
##-----
print("Running FST-NE...")

par = "FST-NE"
# Check if outdir exists
if (!file_test("-d", paste0(OutDir,"/",par))){
  dir.create(file.path(paste0(OutDir,"/",par)), recursive = T)
}

# FST-NE 
fstne_recomb_table <- data.frame(x = log10(exp_parameters[, "meanNe2"]),
                                 y = log10(globalFSTNe(x=obs_sumstats$GSS_WCst, tau=10)))

#save(fstne_recomb_table, 
#     file = paste0(OutDir, "/",par,"/","pred_table_", par, ".RData"))
load(file = paste0(OutDir, "/",par,"/","pred_table_", par, ".RData"))

# Summary statistics of performance prediction 
# Root mean squared error
parmMap[which(parmMap$parameters == par), "mse"] <- mse.fun(fstne_recomb_table)

# R^2
parmMap[which(parmMap$parameters == par), "rsquared"] <- rsquared.fun(fstne_recomb_table)

# Mean bias - log scale
parmMap[which(parmMap$parameters == par), "bias"] <- bias.fun(fstne_recomb_table)

# Clean-up memory
#rm(log_posterior)
rm(fstne_recomb_table)
print("DONE!")

# Export the posterior table
save(parmMap, 
     file = paste0(OutDir, "/","recomb_performance_table", ".RData"))

write.table(parmMap, 
            file = paste0(OutDir, "/","recomb_performance_table", ".txt"),
            row.names = F, col.names = T, quote = F, sep = "\t")

print("DONE!")
