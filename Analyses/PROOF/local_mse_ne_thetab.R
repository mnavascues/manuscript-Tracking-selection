# Local error rate as a function of theta for NE estimates.

rm(list=ls())
ls()

####
####
#### ORIGINAL PROOF SIMULATIONS
####
####

## Error Rate of ABC-RF NE estimates as function of population scaled mutation rate of beneficial mutations
##----------------------------

# Load necessary objects already saved.

# Table with True and estimated NE - columns x and y
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/oob.logmeanNe2_c",".RData"))

# Table with vector containing true theta_b from the reference table - x column
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/oob.logthetaPS_c",".RData"))

## RF-NE prediction as a function of theta*PS
ne_thetaPs <- data.frame(obs_ne=oob.logmeanNe2$x, 
                         rf_ne=oob.logmeanNe2$y,
                         #obs_ne_lo=oob.logmeanNe2$x-0.1, # this is not correct: should be the 95% CI
                         #obs_ne_up=oob.logmeanNe2$x+0.1, # this is not correct: should be the 95% CI
                         logthetaPS=oob.logthetaPS$x) 


#rfne.error <- (!(ne_thetaPs$rf_ne >= ne_thetaPs$obs_ne_lo & ne_thetaPs$rf_ne <= ne_thetaPs$obs_ne_up))
# this is not what we need. It gives the coverage which is interesting, but we need to see the MSE as
# a function of theta_b.

err <- ne_thetaPs$rf_ne - ne_thetaPs$obs_ne # error at each estimate # estimated - observed(true)
global_bias <- mean(err)
global_mse  <- mean(err*err)

tol<-0.2
#rfne.local_error <- array(NA,nrow(ne_thetaPs))
local_bias <- array(NA,nrow(ne_thetaPs))
local_mse <- array(NA,nrow(ne_thetaPs))
for (i in seq_len(nrow(ne_thetaPs))){
  
  distance <- abs(ne_thetaPs$logthetaPS-ne_thetaPs$logthetaPS[i])
  
  # calculate weigths from epachnikov kernel
  nacc <- ceiling(length(distance) * tol)
  ds   <- sort(distance)[nacc]
  weights <- 1 - (distance/ds)^2
  weights[which(weights<0)]<-0
  
  # calculated weighthed proportion of error
  #rfne.local_error[i]<-sum(rfne.error*weights)/sum(weights)
  local_bias[i] <- weighted.mean(err,weights)
  local_mse[i] <- weighted.mean(err*err,weights)
} # end of loop

#ne_thetaPs["error_rf_ne"] <- rfne.local_error
ne_thetaPs["local_bias"] <- local_bias
ne_thetaPs["local_mse"]  <- local_mse
ne_thetaPs <- ne_thetaPs[order(ne_thetaPs$logthetaPS), ]

## Combined: original reftable + randompods reftable
#save(ne_thetaPs, 
#     file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/ne_thetaPs_mse_bias_c",".RData"))
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/ne_thetaPs_mse_bias_c",".RData"))

## TRUE NE vs ABCRF-NE - colored by thetaPS
#plot(x    = ne_thetaPs$obs_ne,
#     y    = ne_thetaPs$rf_ne,
#     xlab = "True value",
#     ylab = "OOB predictions",
#     xlim = c(-2,4),
#     ylim = c(-2,4),
#     main = expression(log[10](italic(N)[e])),
#     cex.axis = 1.2,
#     cex.lab  = 1.2,
#     col=ifelse(ne_thetaPs$logthetaPS > 4, "red", "black"))
#abline(a=0, b=1, col = "green")
#
## LOCAL MSE PLOT
#plot(ne_thetaPs$logthetaPS,
#     ne_thetaPs$local_mse,
#     xlab=expression(log[10](italic(θ)[b])),
#     ylab="Local MSE",
#     xlim=c(-7, 7),
#     ylim=c(0,1),
#     type="l",lwd=2,col="black")
#abline(v=0, lty=3, col="gray")

## Error Rate of FST-NE estimates as function of population scaled mutation rate of beneficial mutations
##----------------------------

# Load necessary objects
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/estimated.fstNe2_c",".RData"))

## FST-NE prediction as a function of theta*PS
fstne_thetaPs <- data.frame(obs_ne=estimated.fstNe2$x, 
                            tm_ne =estimated.fstNe2$y,
                            #obs_ne_lo=oob.logmeanNe2$x-0.1, # this is not correct: should be the 95% CI
                            #obs_ne_up=oob.logmeanNe2$x+0.1, # this is not correct: should be the 95% CI
                            logthetaPS=oob.logthetaPS$x)


dim(fstne_thetaPs) #55634     3

# remove NAN from the dataset - FST-NE contains NaN after log transformation
fstne_thetaPs.nonan <- fstne_thetaPs[!is.nan(fstne_thetaPs$tm_ne), ]
dim(fstne_thetaPs.nonan) #55186     3

#fstne.error <- (!(fstne_thetaPs$tm_ne >= fstne_thetaPs$obs_ne_lo & fstne_thetaPs$tm_ne <= fstne_thetaPs$obs_ne_up))
# this is not what we need. It gives the coverage which is interesting, but we need to see the MSE as
# a function of theta_b.

err <- fstne_thetaPs.nonan$tm_ne - fstne_thetaPs.nonan$obs_ne # error at each estimate # estimated - observed(true)
global_bias <- mean(err)
global_mse  <- mean(err*err)

tol<-0.2
#fstne.local_error <- array(NA,nrow(fstne_thetaPs))
local_bias <- array(NA,nrow(fstne_thetaPs.nonan))
local_mse <- array(NA,nrow(fstne_thetaPs.nonan))
for (i in seq_len(nrow(fstne_thetaPs.nonan))){
  
  distance <- abs(fstne_thetaPs.nonan$logthetaPS-fstne_thetaPs.nonan$logthetaPS[i])
  
  # calculate weigths from epachnikov kernel
  nacc <- ceiling(length(distance) * tol)
  ds   <- sort(distance)[nacc]
  weights <- 1 - (distance/ds)^2
  weights[which(weights<0)]<-0
  
  # calculated weighthed proportion of error
  #fstne.local_error[i]<-sum(fstne.error*weights)/sum(weights)
  local_bias[i] <- weighted.mean(err,weights)
  local_mse[i] <- weighted.mean(err*err,weights)
}

#fstne_thetaPs["error_tm_ne"] <- fstne.local_error
fstne_thetaPs.nonan["local_bias"] <- local_bias
fstne_thetaPs.nonan["local_mse"]  <- local_mse
fstne_thetaPs.nonan <- fstne_thetaPs.nonan[order(fstne_thetaPs.nonan$logthetaPS),]

## Combined: original reftable + randompods reftable
#save(fstne_thetaPs.nonan, 
#     file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/fstne_thetaPs_mse_bias_c",".RData"))
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/regression/fstne_thetaPs_mse_bias_c",".RData"))

## TRUE NE vs FST-NE - colored by thetaPS
#plot(x    = fstne_thetaPs.nonan$obs_ne,
#     y    = fstne_thetaPs.nonan$tm_ne,
#     xlab = "True value",
#     ylab = "FST-NE estimates",
#     xlim = c(-2,4),
#     ylim = c(-2,4),
#     main = expression(log[10](italic(N)[e])),
#     cex.axis = 1.2,
#     cex.lab  = 1.2,
#     col=ifelse(fstne_thetaPs$logthetaPS > 4, "red", "black"))
#abline(a=0, b=1, col = "green")
#
## LOCAL MSE PLOT
#plot(fstne_thetaPs.nonan$logthetaPS,
#     fstne_thetaPs.nonan$local_mse,
#     xlab=expression(log[10](italic(θ)[b])),
#     ylab="Local MSE",
#     xlim=c(-7, 7),
#     ylim=c(0,1),
#     type="l",lwd=2,col="black")
#abline(v=0, lty=3, col="gray")

# ERROR RATE PLOT - combined estimates
#-------------------------------------
pdf(file = "~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/abcrfNE_fstNE_local_mse.pdf")
par(mar=c(5,5,4,1)+.1)
plot(ne_thetaPs$logthetaPS,
     ne_thetaPs$local_mse,
     xlab=expression(log[10](italic(theta)[b])),
     ylab=expression(paste("Local MSE", " ",  "from", " ", log[10](hat(italic(N))[e]), " estimates")),
     xlim=c(-7, 7),
     ylim=c(0, 1),
     type="l",lwd=2,col="#4575b4",
     cex.axis = 1.2,
     cex.lab  = 1.2)
lines(fstne_thetaPs.nonan$logthetaPS,
      fstne_thetaPs.nonan$local_mse,
      type="l",lwd=2,col="#fdae61")
#abline(v=0, lty=3, col="gray")
legend("topleft", legend = c(expression(paste(italic(N)[e], " ", "from ABC-RF")),
                             expression(paste(italic(N)[e], " ", "from Temporal", " ", italic(F)[ST]))), 
       col = c("#4575b4", "#fdae61"), lty = 1, lwd=2,bty = "n")
dev.off()

####
####
#### PODS WITH VARYING GENOME-WIDE RECOMBINATION RATES
####
####

# Error Rate of ABC-RF NE estimates as function of population scaled mutation rate of beneficial mutations
##----------------------------

# Load necessary objects already saved.

# Table with True and estimated NE - columns x and y
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/recomb_pods/posteriors/ne/posterior_table_ne",".RData"))
#posterior_table_ne <- posterior_table
#rm(posterior_table)

# Table with vector containing true theta_b from the reference table - x column
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/recomb_pods/posteriors/theta_b/posterior_table_theta_b",".RData"))
#posterior_table_thetab <- posterior_table
#rm(posterior_table)

## RF-NE prediction as a function of theta*PS
ne_thetaPs_recomb <- data.frame(obs_ne=posterior_table_ne$x, 
                                rf_ne =posterior_table_ne$y,
                                logthetaPS=posterior_table_thetab$x) 


#rfne.error <- (!(ne_thetaPs$rf_ne >= ne_thetaPs$obs_ne_lo & ne_thetaPs$rf_ne <= ne_thetaPs$obs_ne_up))
# this is not what we need. It gives the coverage which is interesting, but we need to see the MSE as
# a function of theta_b.

err <- ne_thetaPs_recomb$rf_ne - ne_thetaPs_recomb$obs_ne # error at each estimate # estimated - observed(true)
global_bias <- mean(err)
global_mse  <- mean(err*err)

tol<-0.2
#rfne.local_error <- array(NA,nrow(ne_thetaPs))
local_bias <- array(NA,nrow(ne_thetaPs_recomb))
local_mse <- array(NA,nrow(ne_thetaPs_recomb))
for (i in seq_len(nrow(ne_thetaPs_recomb))){
  
  distance <- abs(ne_thetaPs_recomb$logthetaPS - ne_thetaPs_recomb$logthetaPS[i])
  
  # calculate weigths from epachnikov kernel
  nacc <- ceiling(length(distance) * tol)
  ds   <- sort(distance)[nacc]
  weights <- 1 - (distance/ds)^2
  weights[which(weights<0)]<-0
  
  # calculated weighthed proportion of error
  #rfne.local_error[i]<-sum(rfne.error*weights)/sum(weights)
  local_bias[i] <- weighted.mean(err,weights)
  local_mse[i] <- weighted.mean(err*err,weights)
} # end of loop

#ne_thetaPs["error_rf_ne"] <- rfne.local_error
ne_thetaPs_recomb["local_bias"] <- local_bias
ne_thetaPs_recomb["local_mse"]  <- local_mse
ne_thetaPs_recomb <- ne_thetaPs_recomb[order(ne_thetaPs_recomb$logthetaPS), ]

## Combined: original reftable + randompods reftable

#save(ne_thetaPs_recomb, 
#     file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/recomb_pods/posteriors/ne/ne_thetaPs_mse_bias_recomb",".RData"))
load(file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/recomb_pods/posteriors/ne/ne_thetaPs_mse_bias_recomb",".RData"))

## TRUE NE vs ABCRF-NE - colored by thetaPS
#plot(x    = ne_thetaPs_recomb$obs_ne,
#     y    = ne_thetaPs_recomb$rf_ne,
#     xlab = "True value",
#     ylab = "OOB predictions",
#     xlim = c(-2,4),
#     ylim = c(-2,4),
#     main = expression(log[10](italic(N)[e])),
#     cex.axis = 1.2,
#     cex.lab  = 1.2,
#     col=ifelse(ne_thetaPs_recomb$logthetaPS > 4, "red", "black"))
#abline(a=0, b=1, col = "green")
#
## LOCAL MSE PLOT
#plot(ne_thetaPs_recomb$logthetaPS,
#     ne_thetaPs_recomb$local_mse,
#     xlab=expression(log[10](italic(θ)[b])),
#     ylab="Local MSE",
#     xlim=c(-7, 7),
#     ylim=c(0,1),
#     type="l",lwd=2,col="black")
#abline(v=0, lty=3, col="gray")

## Error Rate of FST-NE estimates as function of population scaled mutation rate of beneficial mutations
##----------------------------

# Load necessary objects
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/recomb_pods/posteriors/FST-NE/pred_table_FST-NE",".RData"))

## FST-NE prediction as a function of theta*PS
fstne_thetaPs_recomb <- data.frame(obs_ne=fstne_recomb_table$x, 
                                   tm_ne =fstne_recomb_table$y,
                                   logthetaPS=posterior_table_thetab$x)


dim(fstne_thetaPs_recomb) #1000     3

# remove NAN from the dataset - FST-NE contains NaN after log transformation
fstne_thetaPs.nonan_recomb <- fstne_thetaPs_recomb[!is.nan(fstne_thetaPs_recomb$tm_ne), ]
dim(fstne_thetaPs.nonan_recomb) #995     3

err <- fstne_thetaPs.nonan_recomb$tm_ne - fstne_thetaPs.nonan_recomb$obs_ne # error at each estimate # estimated - observed(true)
global_bias <- mean(err)
global_mse  <- mean(err*err)

tol<-0.2
#fstne.local_error <- array(NA,nrow(fstne_thetaPs))
local_bias <- array(NA,nrow(fstne_thetaPs.nonan_recomb))
local_mse <- array(NA,nrow(fstne_thetaPs.nonan_recomb))
for (i in seq_len(nrow(fstne_thetaPs.nonan_recomb))){
  
  distance <- abs(fstne_thetaPs.nonan_recomb$logthetaPS - fstne_thetaPs.nonan_recomb$logthetaPS[i])
  
  # calculate weigths from epachnikov kernel
  nacc <- ceiling(length(distance) * tol)
  ds   <- sort(distance)[nacc]
  weights <- 1 - (distance/ds)^2
  weights[which(weights<0)]<-0
  
  # calculated weighthed proportion of error
  #fstne.local_error[i]<-sum(fstne.error*weights)/sum(weights)
  local_bias[i] <- weighted.mean(err,weights)
  local_mse[i] <- weighted.mean(err*err,weights)
}

#fstne_thetaPs["error_tm_ne"] <- fstne.local_error
fstne_thetaPs.nonan_recomb["local_bias"] <- local_bias
fstne_thetaPs.nonan_recomb["local_mse"]  <- local_mse
fstne_thetaPs.nonan_recomb <- fstne_thetaPs.nonan_recomb[order(fstne_thetaPs.nonan_recomb$logthetaPS),]

## Combined: original reftable + randompods reftable
#save(fstne_thetaPs.nonan_recomb, 
#     file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/recomb_pods/posteriors/FST-NE/fstne_thetaPs_mse_bias_recomb",".RData"))
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/recomb_pods/posteriors/FST-NE/fstne_thetaPs_mse_bias_recomb",".RData"))

## TRUE NE vs FST-NE - colored by thetaPS
#plot(x    = fstne_thetaPs.nonan_recomb$obs_ne,
#     y    = fstne_thetaPs.nonan_recomb$tm_ne,
#     xlab = "True value",
#     ylab = "FST-NE estimates",
#     xlim = c(-2,4),
#     ylim = c(-2,4),
#     main = expression(log[10](italic(N)[e])),
#     cex.axis = 1.2,
#     cex.lab  = 1.2,
#     col=ifelse(fstne_thetaPs.nonan_recomb$logthetaPS > 4, "red", "black"))
#abline(a=0, b=1, col = "green")
#
## LOCAL MSE PLOT
#plot(fstne_thetaPs.nonan_recomb$logthetaPS,
#     fstne_thetaPs.nonan_recomb$local_mse,
#     xlab=expression(log[10](italic(θ)[b])),
#     ylab="Local MSE",
#     xlim=c(-7, 7),
#     ylim=c(0,1),
#     type="l",lwd=2,col="black")
#abline(v=0, lty=3, col="gray")

# ERROR RATE PLOT - combined estimates
#-------------------------------------
pdf(file = "~/My_repositories/Tracking-selection/results/pipeline_v5/recomb_pods/abcrfNE_fstNE_local_mse_recomb.pdf")
par(mar=c(5,5,4,1)+.1)
plot(ne_thetaPs$logthetaPS, # RF-NE PROOF
     ne_thetaPs$local_mse,  # RF-NE PROOF
     xlab=expression(log[10](italic(theta)[b])),
     ylab=expression(paste("Local MSE", " ",  "from", " ", log[10](hat(italic(N))[e]), " estimates")),
     xlim=c(-7, 7),
     ylim=c(0, 1),
     type="l",lwd=2,col="#4575b4",
     cex.axis = 1.2,
     cex.lab  = 1.2)
lines(fstne_thetaPs.nonan$logthetaPS, # FSTNE PROOF
      fstne_thetaPs.nonan$local_mse,  # FSTNE PROOF
      type="l",lwd=2,col="#fdae61")
lines(ne_thetaPs_recomb$logthetaPS, # RF-NE RECOMB
      ne_thetaPs_recomb$local_mse,  # RF-NE RECOMB
      type="l",lwd=2,col="#4575b4", lty = "dashed")
lines(fstne_thetaPs.nonan_recomb$logthetaPS, # FSTNE RECOMB
      fstne_thetaPs.nonan_recomb$local_mse,  # FSTNE RECOMB
      type="l",lwd=2,col="#fdae61", lty = "dashed")
legend("topleft", legend = c(expression(paste(italic(N)[e], " ", "from ABC-RF")),
                             expression(paste(italic(N)[e], " ", "from ABC-RF for PODs with varying genome-wide ", italic(r))),
                             expression(paste(italic(N)[e], " ", "from Temporal", " ", italic(F)[ST])), 
                             expression(paste(italic(N)[e], " ", "from Temporal", " ", italic(F)[ST], " for PODs with varying genome-wide ", italic(r)))
                             ), 
       col = c(rep("#4575b4",2), rep("#fdae61",2)), lty = c(1,2,1,2), lwd=2, bty = "n", cex = 0.8)
dev.off()