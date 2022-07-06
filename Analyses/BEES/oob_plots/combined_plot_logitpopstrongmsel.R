## Logit Proportion of Strongly Selected Mutations

#### Clean-up the R environment
rm(list=ls())
ls()

#### Load librarires
library(ggplot2)
library(cowplot)
library(ggpubr)
library(gtools) # for inv.logit
source("src/Analyses/BEES/fun.R")

#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_vector_logitpopstrongmsel",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_reg_logitpopstrongmsel",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_pods/list_zero_popstrongmsel",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_pods/list_posterior_zero_popstrongmsel",".RData"))
#
#oob.lgtps.ava <- data.frame(x = c(rep(-17.5, length(list.zero.popstrongmsel$Ava)), list.logitpopstrongmsel$Ava),
#                            y = c(list.posterior.zero.popstrongmsel$Ava$expectation, list.reg.logitpopstrongmsel$Ava$model.rf$predictions))
#
#oob.lgtps.hum <- data.frame(x = c(rep(-17.5, length(list.zero.popstrongmsel$Hum)), list.logitpopstrongmsel$Hum),
#                            y = c(list.posterior.zero.popstrongmsel$Hum$expectation, list.reg.logitpopstrongmsel$Hum$model.rf$predictions))
#
#oob.lgtps.dav <- data.frame(x = c(rep(-17.5, length(list.zero.popstrongmsel$Dav)), list.logitpopstrongmsel$Dav),
#                            y = c(list.posterior.zero.popstrongmsel$Dav$expectation, list.reg.logitpopstrongmsel$Dav$model.rf$predictions))
#
#oob.lgtps.sta <- data.frame(x = c(rep(-17.5, length(list.zero.popstrongmsel$Sta)), list.logitpopstrongmsel$Sta),
#                            y = c(list.posterior.zero.popstrongmsel$Sta$expectation, list.reg.logitpopstrongmsel$Sta$model.rf$predictions))
#
#oob.lgtps.ste <- data.frame(x = c(rep(-17.5, length(list.zero.popstrongmsel$Ste)), list.logitpopstrongmsel$Ste),
#                            y = c(list.posterior.zero.popstrongmsel$Ste$expectation, list.reg.logitpopstrongmsel$Ste$model.rf$predictions))
#
#oob.lgtps.riv <- data.frame(x = c(rep(-17.5, length(list.zero.popstrongmsel$Riv)), list.logitpopstrongmsel$Riv),
#                            y = c(list.posterior.zero.popstrongmsel$Riv$expectation, list.reg.logitpopstrongmsel$Riv$model.rf$predictions))
#
#oob.lgtps.pla <- data.frame(x = c(rep(-17.5, length(list.zero.popstrongmsel$Pla)), list.logitpopstrongmsel$Pla),
#                            y = c(list.posterior.zero.popstrongmsel$Pla$expectation, list.reg.logitpopstrongmsel$Pla$model.rf$predictions))
#
### List of oob table for plot
#list.table.oob.pstrong <- list(Ava=oob.lgtps.ava, Hum=oob.lgtps.hum, 
#                            Dav=oob.lgtps.dav, Sta=oob.lgtps.sta, 
#                            Ste=oob.lgtps.ste, Riv=oob.lgtps.riv,
#                            Pla=oob.lgtps.pla)
#
#save(list.table.oob.pstrong, 
#    file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_pstrong",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_pstrong",".RData"))

perf_table <- data.frame(pop = c("Ava", "Hum", "Dav", "Sta", "Ste", "Riv", "Pla"),
                         mse = NA,
                         rsq = NA,
                         mse0= NA,
                         bias0= NA)

# Non-neutral MSE and Rsquared
for (i in seq(perf_table$pop))
{
  # MSE
  perf_table[i,2] <- list.reg.logitpopstrongmsel[[i]]$model.rf$prediction.error
  
  # Rsquared
  perf_table[i,3] <- list.reg.logitpopstrongmsel[[i]]$model.rf$r.squared
}

# Neutral MSE and BIAS
for (i in seq(perf_table$pop))
{
  # MSE
  temp <- data.frame(x=list.zero.popstrongmsel[[i]], 
                     y=inv.logit(list.posterior.zero.popstrongmsel[[i]]$med))
  
  perf_table[i,4] <- mse.fun(data = temp)
  # BIAS
  perf_table[i,5] <- bias.fun(data = temp)
  rm(temp)
}

# Clean-up memory
rm(list.zero.popstrongmsel)
rm(list.logitpopstrongmsel)
rm(list.posterior.zero.popstrongmsel)
rm(list.reg.logitpopstrongmsel)

# Export LOAD performance table
#write.csv(perf_table,
#          file = "results/pipeline_v6_bees/random_forests/performance_pstrong.txt", row.names = F, quote = F)

## AVALON
my_breaks_1 <- c(54.598150, 7.389056, 1)
ps.Ava   <- ggplot(list.table.oob.pstrong[[1]], aes(x,y))
ps2d.Ava <- ps.Ava + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", logit(italic(P))))) +
  ylab(expression(paste(logit(italic(hat(P))), " ","(OOB prediction)")))+
  ggtitle("Avalon") +
  annotate("text", x = -1, y = -12, label = paste("MSE =", round(perf_table[1,2], 3))) + 
  annotate("text", x = -1, y = -14, label = "italic(R) ^ 2 == 0.814", parse = TRUE) +
  xlim(-18,2) +
  ylim(-15,2) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ps2d.Ava

## HUMBOLDT
my_breaks_2 <- c(54.598150, 7.389056, 1)
ps.Hum   <- ggplot(list.table.oob.pstrong[[2]], aes(x,y))
ps2d.Hum <- ps.Hum + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2)
  ) +
  xlab(expression(paste("true", " ", logit(italic(P))))) +
  ylab(expression(paste(logit(italic(hat(P))), " ","(OOB prediction)")))+
  ggtitle("Humboldt") +
  annotate("text", x = -1, y = -12, label = paste("MSE =", round(perf_table[2,2], 3))) + 
  annotate("text", x = -1, y = -14, label = "italic(R) ^ 2 == 0.810", parse = TRUE) +
  xlim(-18,2) +
  ylim(-15,2) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ps2d.Hum

## DAVIS
my_breaks_3 <- c(54.598150, 7.389056, 1)
ps.Dav   <- ggplot(list.table.oob.pstrong[[3]], aes(x,y))
ps2d.Dav <- ps.Dav + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_3),labels = round(my_breaks_3)
  ) +
  xlab(expression(paste("true", " ", logit(italic(P))))) +
  ylab(expression(paste(logit(italic(hat(P))), " ","(OOB prediction)")))+
  ggtitle("Davis") +
  annotate("text", x = -1, y = -12, label = paste("MSE =", round(perf_table[3,2], 3))) + 
  annotate("text", x = -1, y = -14, label = "italic(R) ^ 2 == 0.787", parse = TRUE) +
  xlim(-18,2) +
  ylim(-15,2) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ps2d.Dav

## STANISLAUS
my_breaks_4 <- c(54.598150, 7.389056, 1)
ps.Sta   <- ggplot(list.table.oob.pstrong[[4]], aes(x,y))
ps2d.Sta <- ps.Sta + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_4),labels = round(my_breaks_4)
  ) +
  xlab(expression(paste("true", " ", logit(italic(P))))) +
  ylab(expression(paste(logit(italic(hat(P))), " ","(OOB prediction)")))+
  ggtitle("Stanislaus") +
  annotate("text", x = -1, y = -12, label = paste("MSE =", round(perf_table[4,2], 3))) + 
  annotate("text", x = -1, y = -14, label = "italic(R) ^ 2 == 0.782", parse = TRUE) +
  xlim(-18,2) +
  ylim(-15,2) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ps2d.Sta

## STEBBINS
my_breaks_5 <- c(54.598150, 7.389056, 1)
ps.Ste   <- ggplot(list.table.oob.pstrong[[5]], aes(x,y))
ps2d.Ste <- ps.Ste + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_5),labels = round(my_breaks_5)
  ) +
  xlab(expression(paste("true", " ", logit(italic(P))))) +
  ylab(expression(paste(logit(italic(hat(P))), " ","(OOB prediction)")))+
  ggtitle("Stebbins") +
  annotate("text", x = -1, y = -12, label = paste("MSE =", round(perf_table[5,2], 3))) + 
  annotate("text", x = -1, y = -14, label = "italic(R) ^ 2 == 0.768", parse = TRUE) +
  xlim(-18,2) +
  ylim(-15,2) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ps2d.Ste

## RIVERSIDE
my_breaks_6 <- c(54.598150, 7.389056, 1)
ps.Riv   <- ggplot(list.table.oob.pstrong[[6]], aes(x,y))
ps2d.Riv <- ps.Riv + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_6),labels = round(my_breaks_6)
  ) +
  xlab(expression(paste("true", " ", logit(italic(P))))) +
  ylab(expression(paste(logit(italic(hat(P))), " ","(OOB prediction)")))+
  ggtitle("Riverside") +
  annotate("text", x = -1, y = -12, label = paste("MSE =", round(perf_table[6,2], 3))) + 
  annotate("text", x = -1, y = -14, label = "italic(R) ^ 2 == 0.730", parse = TRUE) +
  xlim(-18,2) +
  ylim(-15,2) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ps2d.Riv

## PLACERITA
my_breaks_7 <- c(54.598150, 7.389056, 1)
ps.Pla   <- ggplot(list.table.oob.pstrong[[7]], aes(x,y))
ps2d.Pla <- ps.Pla + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_7),labels = round(my_breaks_7)
  ) +
  xlab(expression(paste("true", " ", logit(italic(P))))) +
  ylab(expression(paste(logit(italic(hat(P))), " ","(OOB prediction)")))+
  ggtitle("Placerita") +
  annotate("text", x = -1, y = -12, label = paste("MSE =", round(perf_table[7,2], 3))) + 
  annotate("text", x = -1, y = -14, label = "italic(R) ^ 2 == 0.756", parse = TRUE) +
  xlim(-18,2) +
  ylim(-15,2) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ps2d.Pla

# Combined plot
par(mfrow=c(3, 1))
combined_pstrong<-ggarrange(ps2d.Ava, ps2d.Hum,
                            ps2d.Dav, ps2d.Sta,
                            ps2d.Ste, ps2d.Riv,
                            ps2d.Pla,
                            nrow = 4,
                            ncol = 2,
                            labels = NULL)

#combined_pstrong

# Save plot
ggsave("results/pipeline_v6_bees/random_forests/oob_plots/combined_plot_pstrong.pdf",
       combined_pstrong,
       device="pdf",
       width=9,
       height=12)
