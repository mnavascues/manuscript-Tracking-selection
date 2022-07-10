########################################
##          Manuscript plots          ##
##          Proof of Concept          ##
########################################

## OOB plots

rm(list=ls())
ls()

#### Load librarires
library(ggplot2)
library(ggpubr)
library(cowplot)

### Load plot dataframes

## Selection

# Log PrPs - Proportion of beneficial mutation - parameter
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/oob.logPrPs_c",".RData"))

# Log gamma mean - parameter
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/oob.loggammamean_c",".RData"))

# Logit Proportion of strongly selected mutation - latent variable + boxplot with neutral simulation predictions
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/oob.lgtps_c",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/posterior_pods/pred.lgtps.neutralsims",".RData"))

# Log gamma mean - strongly selected mutation - latent variable + boxplot with neutral simulation predictions
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/oob.logpopstrongselmean_c",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/posterior_pods/pred.logpopStrongSelMean.zeroValues",".RData"))

# Logit Average Genetic Load - latent variable + boxplot with neutral simulation predictions
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/oob.lgtgl_c",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/posterior_pods/pred.lgtgl.neutralsims",".RData"))

# Log Theta Selected Mutations
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/oob.logthetaPS_c",".RData"))

## Demography

# Log Per site mutation rate mu - parameter
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/oob.logmu_c",".RData"))

# Log recombination rate rr - paramter
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/oob.logrr_c",".RData"))

# Log Harmonic mean of the effective population sizes of period 2 - latent variable
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/oob.logmeanNe2_c",".RData"))

# Log Population size of period 2 - parameter 
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/oob.logncs_c",".RData"))

# Log Ratio Mean effective size / population size of period 2 - MeanNe2/Ncs - latent variable
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/oob.logmeanNe2ncs_c",".RData"))

### Figures 

## Selection - scatterplots

# Log PrPs - Proportion of beneficial mutation - parameter
#par(mar=c(5,5,4,1)+.1)
my_breaks_1 <- c(20.085537, 7.389056, 2.718282, 1)
prps   <- ggplot(oob.logPrPs, aes(x,y))
prps2d <- prps + 
        stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
        scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                             ,
                             breaks = round(my_breaks_1),labels = round(my_breaks_1),
        ) +
        xlab(expression(paste("true", " ", log[10](italic(P)[R] * italic(P)[B])))) +
        ylab(expression(paste(log[10](hat(italic(P)[R] * italic(P)[B])), " ","(OOB prediction)")))+
        xlim(-6,0) +
        ylim(-6,0) +
        geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
        theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           legend.position = "right", axis.text = element_text(size = 12),
                           axis.title=element_text(size=12))
prps2d

# Log gamma mean - parameter
#par(mar=c(5,5,4,1)+.1)
my_breaks_2 <- c(20.085537, 7.389056, 2.718282, 1)
gammamean   <- ggplot(oob.loggammamean, aes(x,y))
gammamean2d <- gammamean + 
        stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
        scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                             ,
                             breaks = round(my_breaks_2),labels = round(my_breaks_2),
        ) +
        xlab(expression(paste("true", " ", log[10](italic(gamma))))) +
        ylab(expression(paste(log[10](italic(hat(gamma))), " ","(OOB prediction)")))+
        xlim(-3,0) +
        ylim(-3,0) +
        geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
        theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           legend.position = "right", axis.text = element_text(size = 12),
                           axis.title=element_text(size=12))
gammamean2d

# Logit Proportion of strongly selected mutation - latent variable
#par(mar=c(5,5,4,1)+.1)
my_breaks_3 <- c(54.598150, 7.389056, 1)
ps   <- ggplot(oob.lgtps, aes(x,y))
ps2d <- ps + 
        stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
        scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                             ,breaks = round(my_breaks_3),labels = round(my_breaks_3)
        ) +
        xlab(expression(paste("true", " ", logit(italic(P))))) +
        ylab(expression(paste(logit(italic(hat(P))), " ","(OOB prediction)")))+
        xlim(-15,2) +
        ylim(-15,2) +
        geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
        theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           legend.position = "right", axis.text = element_text(size = 12),
                           axis.title=element_text(size=12))
ps2d

# Log gamma mean - strongly selected mutation - latent variable
#par(mar=c(5,5,4,1)+.1)
my_breaks_4 <- c(54.598150, 7.389056, 1)
strongSelMean   <- ggplot(oob.logpopstrongselmean, aes(x,y))
strongSelMean2d <- strongSelMean + 
        stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
        scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                             ,
                             breaks = round(my_breaks_4),labels = round(my_breaks_4),
        ) +
        xlab(expression(paste("true", " ", log[10](italic(bar(s)))))) +
        ylab(expression(paste(log[10](hat(italic(bar(s)))), " ","(OOB prediction)")))+
        xlim(-3,1) +
        ylim(-3,1) +
        geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
        theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           legend.position = "right", axis.text = element_text(size = 12),
                           axis.title=element_text(size=12))
strongSelMean2d


# Logit Average Genetic Load - latent variable
#par(mar=c(5,5,4,1)+.1)
my_breaks_5 <- c(403.428793, 54.598150, 7.389056, 1)
gl   <- ggplot(oob.lgtgl, aes(x,y))
gl2d <- gl + 
        stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
        scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                             ,
                             breaks = round(my_breaks_5),labels = round(my_breaks_5),
                             ) +
        xlab(expression(paste("true", " ", logit(italic(L))))) +
        ylab(expression(paste(logit(italic(hat(L))), " ","(OOB prediction)")))+
        xlim(-17,6) +
        ylim(-17,6) +
        geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
        theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           legend.position = "right", axis.text = element_text(size = 12),
                           axis.title=element_text(size=12))
gl2d

# Log Theta Selected Mutations
#par(mar=c(5,5,4,1)+.1)
my_breaks_6 <- c(54.598150, 7.389056, 1)
thetaPS   <- ggplot(oob.logthetaPS, aes(x,y))
thetaPS2d <- thetaPS + 
             stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
             scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                                  ,
                                  breaks = round(my_breaks_6),labels = round(my_breaks_6)
                                  ) +
             xlab(expression(paste("true", " ", log[10](italic(θ)[b])))) +
             ylab(expression(paste(log[10](italic(hat(θ))[b]), " ","(OOB prediction)")))+
             xlim(-8,6) +
             ylim(-8,6) +
             geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
             theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           legend.position = "right", axis.text = element_text(size = 16),
                           axis.title=element_text(size=16))

thetaPS2d

## Boxplots for the estimated values for the neutral simulations

# Logit Proportion of strongly selected mutations - neutral simulations
neutral.p.plot <- ggplot(pred.lgtps.neutralsims, aes(x=col, y=est_lgtp)) + 
        geom_boxplot() + 
        #xlab("") +
        #ylab("") +
        xlab(expression(paste("true", " ", logit(italic(P))))) +
        ylab(expression(paste(logit(italic(hat(P))), " ","(OOB prediction)")))+
        ylim(-15,2) +
        #geom_abline(intercept = 0, slope = 0, color = "darkgrey",  linetype="dashed") +
        theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           legend.position = "right", axis.text = element_text(size = 12),
                           axis.title=element_text(size=12))
neutral.p.plot

# Log gamma mean of strongly selected mutations - neutral simulations
zerovalues.strongSelMean.plot <- ggplot(pred.logpopStrongSelMean.zeroValues, aes(x=col, y=est_logsmean)) + 
        geom_boxplot() + 
        #xlab("") +
        #ylab("") +
        xlab(expression(paste("true", " ", log[10](italic(bar(s)))))) +
        ylab(expression(paste(log[10](hat(italic(bar(s)))), " ","(OOB prediction)")))+
        ylim(-3,1) +
        #geom_abline(intercept = 0, slope = 0, color = "darkgrey",  linetype="dashed") +
        theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           legend.position = "right", axis.text = element_text(size = 12),
                           axis.title=element_text(size=12))

zerovalues.strongSelMean.plot


# Logit Average Genetic load - neutral simulations
neutral.l.plot <- ggplot(pred.lgtgl.neutralsims, aes(x=col, y=est_lgtl)) + 
                  geom_boxplot() + 
                  #xlab("") +
                  #ylab("") +
                  xlab(expression(paste("true", " ", logit(italic(L))))) +
                  ylab(expression(paste(logit(italic(hat(L))), " ","(OOB prediction)"))) +
                  ylim(-17,6) +
                  #geom_abline(intercept = 0, slope = 0, color = "darkgrey",  linetype="dashed") +
                  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                                     panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                     legend.position = "right", axis.text = element_text(size = 12),
                                     axis.title=element_text(size=12))
neutral.l.plot



box_plots <- cowplot::plot_grid(neutral.p.plot, neutral.l.plot, zerovalues.strongSelMean.plot,
                                nrow = 1, ncol = 3,
                                labels = NULL)

cowplot::plot_grid(prps2d, ps2d,
                   gammamean2d, strongSelMean2d,
                   gl2d, thetaPS2d,
                   box_plots,
                   nrow = 4,
                   ncol = 2,
                   labels = NULL)

## Demography - scatterplots

# Log Per site mutation rate mu - parameter
#par(mar=c(5,5,4,1)+.1)
my_breaks_7 <- c(54.598150, 7.389056, 1)
mu   <- ggplot(oob.logmu, aes(x,y))
mu2d <- mu + 
        stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
        scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                             ,
                             breaks = round(my_breaks_7),labels = round(my_breaks_7)
        ) +
        xlab(expression(paste("true", " ", log[10](italic(mu))))) +
        ylab(expression(paste(log[10](italic(hat(mu))), " ","(OOB prediction)")))+
        xlim(-10,-6) +
        ylim(-10,-6) +
        geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
        theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           legend.position = "right", axis.text = element_text(size = 12),
                           axis.title=element_text(size=12))
mu2d

# Log recombination rate rr - paramter
my_breaks_8 <- c(20.085537, 7.389056, 2.718282, 1)
rr   <- ggplot(oob.logrr, aes(x,y))
rr2d <- rr + 
        stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
        scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                             ,
                             breaks = round(my_breaks_8),labels = round(my_breaks_8)
        ) +
        xlab(expression(paste("true", " ", log[10](italic(c)[0])))) +
        ylab(expression(paste(log[10](hat(italic(c)[0])), " ","(OOB prediction)")))+
        xlim(-9.4,-6.4) +
        ylim(-9.4,-6.4) +
        geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
        theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           legend.position = "right", axis.text = element_text(size = 12),
                           axis.title=element_text(size=12))
rr2d

# Log Harmonic mean of the effective population sizes of period 2 - latent variable
#par(mar=c(5,5,4,1)+.1)
my_breaks_9 <- c(2980.957987,403.428793, 54.598150, 7.389056, 1)
ne   <- ggplot(oob.logmeanNe2, aes(x,y))
ne2d <- ne + 
        stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
        scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                             ,breaks = round(my_breaks_9),labels = round(my_breaks_9)
                             ) +
        xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
        ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(OOB prediction)")))+
        xlim(-0.5,3.5) +
        ylim(-0.5,3.5) +
        geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
        theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           legend.position = "right", axis.text = element_text(size = 16),
                           axis.title=element_text(size=16))
ne2d

# Log Population size of period 2 - parameter 
#par(mar=c(5,5,4,1)+.1)
my_breaks_10 <- c(403.428793, 54.598150, 7.389056, 1)
ncs   <- ggplot(oob.logncs, aes(x,y))
ncs2d <- ncs + 
        stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
        scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                             ,
                             breaks = round(my_breaks_10),labels = round(my_breaks_10)
                             ) +
        xlab(expression(paste("true", " ", log[10](italic(N))))) +
        ylab(expression(paste(log[10](italic(hat(N))), " ","(OOB prediction)")))+
        xlim(-0.5,3.5) +
        ylim(-0.5,3.5) +
        geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
        theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           legend.position = "right", axis.text = element_text(size = 16),
                           axis.title=element_text(size=16))
ncs2d

# Log Ratio Mean effective size / population size of period 2 - MeanNe2/Ncs - latent variable
#par(mar=c(5,5,4,1)+.1)
my_breaks_11 <- c(8103.08393, 403.42879, 20.08554, 1)
nencs   <- ggplot(oob.logmeanNe2ncs, aes(x,y))
nencs2d <- nencs + 
        stat_bin2d(bins=100,na.rm = T, show.legend=T) + 
        scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                             ,
                             breaks = round(my_breaks_11),labels = round(my_breaks_11)
                             ) +
        xlab(expression(paste("true", " ", log[10](italic(N)[e]/italic(N))))) +
        ylab(expression(paste(log[10](italic(hat(N))[e]/italic(N)), " ","(OOB prediction)")))+
        xlim(-3.5,0.5) +
        ylim(-3.5,0.5) +
        geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
        theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                           legend.position = "right", axis.text = element_text(size = 12),
                           axis.title=element_text(size=12))
nencs2d

## OOB Demography and selection (joint inference)
##-----------------------------------------

## Ne estimates based on temporal FST
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v5/random_forests/oob_tables/estimated.fstNe2_c",".RData"))

my_breaks_fstnerf <- c(403.428793, 54.598150, 7.389056, 1)
fstne   <- ggplot(estimated.fstNe2, aes(x,y))
fstne2d <- fstne + 
           stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
           scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                                ,breaks =round(my_breaks_fstnerf),labels = round(my_breaks_fstnerf)
                                ) +
           xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
           ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(from", " ", italic(hat(F))[ST], ")")))+
           xlim(-0.5,3.5) +
           ylim(-0.5,3.5) +
           geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
           theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                              legend.position = "right", axis.text = element_text(size = 16),
                              axis.title=element_text(size=16))
fstne2d

# Joint Inference of Demography and Selection
join_demo_sel_plot <- cowplot::plot_grid(thetaPS2d,ncs2d, 
                                         ne2d, fstne2d,
                                         nrow = 2,
                                         ncol = 2,
                                         labels=""
                                         #,
                                         #align = "hv"
                                         )

ggsave(join_demo_sel_plot, filename = "results/pipeline_v5/random_forests/join_demo_sel.pdf", device = cairo_pdf, 
       width = 13, height = 9, units = "in", dpi = "retina")
