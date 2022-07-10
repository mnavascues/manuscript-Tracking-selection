# OOB MSE and Rsquared

#### Clean-up the R environment
rm(list=ls())
ls()

source("src/Analyses/PROOF/fun.R")

# Load the trainig reftable summary statistics
path_rfs = "~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/"

# Parameter names
names_parameters <- c("prps","gamma","theta_b",
                      "mu","rr","ne","nc","ne_nc",
                      "pstrong","gamma_s","load","FST-NE")
# RF names
names_rfs <- c("reg_logPrPs_c", "reg_loggammamean_c", "reg_logthetaPS_c", 
               "reg_logmu_c", "reg_logrr_c", "reg_logmeanNe2_c","reg_logncs_c", "reg_logmeanNe2ncs_c",
               "reg_logitpopstrongmsel_c", "reg_logpopstrongselmean_c","reg_averageGenLoad_2_c", "estimated.fstNe2_c")

parmMap <- data.frame(parameters=names_parameters, 
                      regressions=names_rfs, 
                      mse=NA,
                      rsquared=NA,
                      mse0=NA,
                      bias0=NA)


# PrPb
reg_name = parmMap[1, 2]
load(file = paste0(path_rfs, reg_name, ".RData"))
parmMap[1,3] <- reg_logPrPs$model.rf$prediction.error
parmMap[1,4] <- reg_logPrPs$model.rf$r.squared
rm(reg_logPrPs)

# gamma
reg_name = parmMap[2, 2]
load(file = paste0(path_rfs, reg_name, ".RData"))
parmMap[2,3] <- reg_loggammamean$model.rf$prediction.error
parmMap[2,4] <- reg_loggammamean$model.rf$r.squared
rm(reg_loggammamean)

# theta_b
reg_name = parmMap[3, 2]
load(file = paste0(path_rfs, reg_name, ".RData"))
parmMap[3,3] <- reg_logthetaPS$model.rf$prediction.error
parmMap[3,4] <- reg_logthetaPS$model.rf$r.squared
rm(reg_logthetaPS)

#mu
reg_name = parmMap[4, 2]
load(file = paste0(path_rfs, reg_name, ".RData"))
parmMap[4,3] <- reg_logmu$model.rf$prediction.error
parmMap[4,4] <- reg_logmu$model.rf$r.squared
rm(reg_logmu)

#rr
reg_name = parmMap[5, 2]
load(file = paste0(path_rfs, reg_name, ".RData"))
parmMap[5,3] <- reg_logrr$model.rf$prediction.error
parmMap[5,4] <- reg_logrr$model.rf$r.squared
rm(reg_logrr)

#ne
reg_name = parmMap[6, 2]
load(file = paste0(path_rfs, reg_name, ".RData"))
parmMap[6,3] <- reg_logmeanNe2$model.rf$prediction.error
parmMap[6,4] <- reg_logmeanNe2$model.rf$r.squared
rm(reg_logmeanNe2)

#nc
reg_name = parmMap[7, 2]
load(file = paste0(path_rfs, reg_name, ".RData"))
parmMap[7,3] <- reg_logncs$model.rf$prediction.error
parmMap[7,4] <- reg_logncs$model.rf$r.squared
rm(reg_logncs)

#ne_nc
reg_name = parmMap[8, 2]
load(file = paste0(path_rfs, reg_name, ".RData"))
parmMap[8,3] <-  reg_logmeanNe2ncs$model.rf$prediction.error
parmMap[8,4] <-  reg_logmeanNe2ncs$model.rf$r.squared
rm(reg_logmeanNe2ncs)

#pstrong
reg_name = parmMap[9, 2]
load(file = paste0(path_rfs, reg_name, ".RData"))
parmMap[9,3] <-  reg_logitpopstrongmsel$model.rf$prediction.error
parmMap[9,4] <-  reg_logitpopstrongmsel$model.rf$r.squared
rm(reg_logitpopstrongmsel)

load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/posterior_pods/pred.lgtps.neutralsims",".RData"))

temp <- pred.lgtps.neutralsims[,c(1,3)]
colnames(temp) <- c("x", "y")
parmMap[9,5] <- mse.fun(temp)
parmMap[9,6] <- bias.fun(temp)
rm(pred.lgtps.neutralsims)
rm(temp)

# gamma sel
reg_name = parmMap[10, 2]
load(file = paste0(path_rfs, reg_name, ".RData"))
parmMap[10,3] <-  reg_logpopstrongselmean$model.rf$prediction.error
parmMap[10,4] <-  reg_logpopstrongselmean$model.rf$r.squared
rm(reg_logpopstrongselmean)

load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/posterior_pods/pred.logpopStrongSelMean.zeroValues",".RData"))

temp <- pred.logpopStrongSelMean.zeroValues[,c(1,3)]
colnames(temp) <- c("x", "y")
parmMap[10,5] <- mse.fun(temp)
parmMap[10,6] <- bias.fun(temp)
rm(pred.logpopStrongSelMean.zeroValues)
rm(temp)

# load
reg_name = parmMap[11, 2]
load(file = paste0(path_rfs, reg_name, ".RData"))
parmMap[11,3] <-  reg_averageGenLoad_2$model.rf$prediction.error
parmMap[11,4] <-  reg_averageGenLoad_2$model.rf$r.squared
rm(reg_averageGenLoad_2)

load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/posterior_pods/pred.lgtgl.neutralsims",".RData"))

temp <- pred.lgtgl.neutralsims[,c(1,3)]
colnames(temp) <- c("x", "y")
parmMap[11,5] <- mse.fun(temp)
parmMap[11,6] <- bias.fun(temp)
rm(pred.lgtgl.neutralsims)
rm(temp)

# FST-NE MSE and Rsquared
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/estimated.fstNe2_c",".RData"))

temp <- estimated.fstNe2[,c(1,2)]
colnames(temp) <- c("x", "y")
parmMap[12,3] <- mse.fun(temp)
parmMap[12,4] <- rsquared.fun(temp)
parmMap[12,6] <- bias.fun(temp)
rm(estimated.fstNe2)
rm(temp)

# Export Parameters' performance table
write.csv(parmMap,
          file = "results/pipeline_v5/random_forests/performance_table.txt", row.names = F, quote = F)
