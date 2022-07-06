## logpopstrongselmean

#### Clean-up the R environment
rm(list=ls())
ls()


#### Load librarires
library(ggplot2)
library(cowplot)
library(ggpubr)
source("src/Analyses/BEES/fun.R")

#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_vector_logpopstrongselmean",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_reg_logpopstrongselmean",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_pods/list_zero_popstrongselmean",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_pods/list_posterior_zero_popstrongselmean",".RData"))
#
#oob.logpopstrongselmean.ava <- data.frame(x = c(rep(-4, length(list.zero.popstrongselmean$Ava)), list.logpopstrongselmean$Ava),
#                                          y = c(list.posterior.zero.popstrongselmean$Ava$expectation, list.reg.logpopstrongselmean$Ava$model.rf$predictions))
#
#oob.logpopstrongselmean.hum <- data.frame(x = c(rep(-4, length(list.zero.popstrongselmean$Hum)), list.logpopstrongselmean$Hum),
#                                          y = c(list.posterior.zero.popstrongselmean$Hum$expectation, list.reg.logpopstrongselmean$Hum$model.rf$predictions))
#
#oob.logpopstrongselmean.dav <- data.frame(x = c(rep(-4, length(list.zero.popstrongselmean$Dav)), list.logpopstrongselmean$Dav),
#                                          y = c(list.posterior.zero.popstrongselmean$Dav$expectation, list.reg.logpopstrongselmean$Dav$model.rf$predictions))
#
#oob.logpopstrongselmean.sta <- data.frame(x = c(rep(-4, length(list.zero.popstrongselmean$Sta)), list.logpopstrongselmean$Sta),
#                                          y = c(list.posterior.zero.popstrongselmean$Sta$expectation, list.reg.logpopstrongselmean$Sta$model.rf$predictions))
#
#oob.logpopstrongselmean.ste <- data.frame(x = c(rep(-4, length(list.zero.popstrongselmean$Ste)), list.logpopstrongselmean$Ste),
#                                          y = c(list.posterior.zero.popstrongselmean$Ste$expectation, list.reg.logpopstrongselmean$Ste$model.rf$predictions))
#
#oob.logpopstrongselmean.riv <- data.frame(x = c(rep(-4, length(list.zero.popstrongselmean$Riv)), list.logpopstrongselmean$Riv),
#                                          y = c(list.posterior.zero.popstrongselmean$Riv$expectation, list.reg.logpopstrongselmean$Riv$model.rf$predictions))
#
#oob.logpopstrongselmean.pla <- data.frame(x = c(rep(-4, length(list.zero.popstrongselmean$Pla)), list.logpopstrongselmean$Pla),
#                                          y = c(list.posterior.zero.popstrongselmean$Pla$expectation, list.reg.logpopstrongselmean$Pla$model.rf$predictions))
#
### List of oob table for plot
#list.table.oob.gammasel <- list(Ava=oob.logpopstrongselmean.ava, Hum=oob.logpopstrongselmean.hum, 
#                                Dav=oob.logpopstrongselmean.dav, Sta=oob.logpopstrongselmean.sta, 
#                                Ste=oob.logpopstrongselmean.ste, Riv=oob.logpopstrongselmean.riv,
#                                Pla=oob.logpopstrongselmean.pla)
#
#save(list.table.oob.gammasel, 
#    file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_strong_gammmasel",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_strong_gammmasel",".RData"))

perf_table <- data.frame(pop = c("Ava", "Hum", "Dav", "Sta", "Ste", "Riv", "Pla"),
                         mse = NA,
                         rsq = NA,
                         mse0= NA,
                         bias0= NA)

# Non-neutral MSE and Rsquared
for (i in seq(perf_table$pop))
{
  # MSE
  perf_table[i,2] <- list.reg.logpopstrongselmean[[i]]$model.rf$prediction.error
  
  # Rsquared
  perf_table[i,3] <- list.reg.logpopstrongselmean[[i]]$model.rf$r.squared
}

# Neutral MSE and BIAS
for (i in seq(perf_table$pop))
{
  # MSE
  temp <- data.frame(x=list.zero.popstrongselmean[[i]], 
                     y=10^list.posterior.zero.popstrongselmean[[i]]$med)
  
  perf_table[i,4] <- mse.fun(data = temp)
  # BIAS
  perf_table[i,5] <- bias.fun(data = temp)
  rm(temp)
}

# Clean-up memory
rm(list.zero.popstrongselmean)
rm(list.logpopstrongselmean)
rm(list.posterior.zero.popstrongselmean)
rm(list.reg.logpopstrongselmean)

# Export LOAD performance table
#write.csv(perf_table,
#          file = "results/pipeline_v6_bees/random_forests/performance_gammasel.txt", row.names = F, quote = F)


## AVALON
my_breaks_1 <- c(54.598150, 7.389056, 1)
gammas.Ava   <- ggplot(list.table.oob.gammasel[[1]], aes(x,y))
gammas2d.Ava <- gammas.Ava + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(bar(s)))))) +
  ylab(expression(paste(log[10](hat(italic(bar(s)))), " ","(OOB prediction)")))+
  ggtitle("Avalon") +
  annotate("text", x = 0, y = -2.4, label = paste("MSE =", round(perf_table[1,2], 3))) + 
  annotate("text", x = 0, y = -2.8, label = "italic(R) ^ 2 == 0.436", parse = TRUE) +
  xlim(-4.5,1) +
  ylim(-3,1) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammas2d.Ava

## HUMBOLDT
my_breaks_2 <- c(54.598150, 7.389056, 1)
gammas.Hum   <- ggplot(list.table.oob.gammasel[[2]], aes(x,y))
gammas2d.Hum <- gammas.Hum + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(bar(s)))))) +
  ylab(expression(paste(log[10](hat(italic(bar(s)))), " ","(OOB prediction)")))+
  ggtitle("Humboldt") +
  annotate("text", x = 0, y = -2.4, label = paste("MSE =", round(perf_table[2,2], 3))) + 
  annotate("text", x = 0, y = -2.8, label = "italic(R) ^ 2 == 0.388", parse = TRUE) +
  xlim(-4.5,1) +
  ylim(-3,1) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammas2d.Hum

## DAVIS
my_breaks_3 <- c(54.598150, 7.389056, 1)
gammas.Dav   <- ggplot(list.table.oob.gammasel[[3]], aes(x,y))
gammas2d.Dav <- gammas.Dav + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_3),labels = round(my_breaks_3)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(bar(s)))))) +
  ylab(expression(paste(log[10](hat(italic(bar(s)))), " ","(OOB prediction)")))+
  ggtitle("Davis") +
  annotate("text", x = 0, y = -2.4, label = paste("MSE =", round(perf_table[3,2], 3))) + 
  annotate("text", x = 0, y = -2.8, label = "italic(R) ^ 2 == 0.347", parse = TRUE) +
  xlim(-4.5,1) +
  ylim(-3,1) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammas2d.Dav

## STANISLAUS
my_breaks_4 <- c(54.598150, 7.389056, 1)
gammas.Sta   <- ggplot(list.table.oob.gammasel[[4]], aes(x,y))
gammas2d.Sta <- gammas.Sta + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_4),labels = round(my_breaks_4)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(bar(s)))))) +
  ylab(expression(paste(log[10](hat(italic(bar(s)))), " ","(OOB prediction)")))+
  ggtitle("Stanislaus") +
  annotate("text", x = 0, y = -2.4, label = paste("MSE =", round(perf_table[4,2], 3))) + 
  annotate("text", x = 0, y = -2.8, label = "italic(R) ^ 2 == 0.328", parse = TRUE) +
  xlim(-4.5,1) +
  ylim(-3,1) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammas2d.Sta

## STEBBINS
my_breaks_5 <- c(54.598150, 7.389056, 1)
gammas.Ste   <- ggplot(list.table.oob.gammasel[[5]], aes(x,y))
gammas2d.Ste <- gammas.Ste + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_5),labels = round(my_breaks_5)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(bar(s)))))) +
  ylab(expression(paste(log[10](hat(italic(bar(s)))), " ","(OOB prediction)")))+
  ggtitle("Stebbins") +
  annotate("text", x = 0, y = -2.4, label = paste("MSE =", round(perf_table[5,2], 3))) + 
  annotate("text", x = 0, y = -2.8, label = "italic(R) ^ 2 == 0.264", parse = TRUE) +
  xlim(-4.5,1) +
  ylim(-3,1) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammas2d.Ste

## RIVERSIDE
my_breaks_6 <- c(54.598150, 7.389056, 1)
gammas.Riv   <- ggplot(list.table.oob.gammasel[[6]], aes(x,y))
gammas2d.Riv <- gammas.Riv + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_6),labels = round(my_breaks_6)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(bar(s)))))) +
  ylab(expression(paste(log[10](hat(italic(bar(s)))), " ","(OOB prediction)")))+
  ggtitle("Riverside") +
  annotate("text", x = 0, y = -2.4, label = paste("MSE =", round(perf_table[6,2], 3))) + 
  annotate("text", x = 0, y = -2.8, label = "italic(R) ^ 2 == 0.239", parse = TRUE) +
  xlim(-4.5,1) +
  ylim(-3,1) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammas2d.Riv

## PLACERITA
my_breaks_7 <- c(403.428793, 54.598150, 7.389056, 1)
gammas.Pla   <- ggplot(list.table.oob.gammasel[[7]], aes(x,y))
gammas2d.Pla <- gammas.Pla + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_7),labels = round(my_breaks_7)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(bar(s)))))) +
  ylab(expression(paste(log[10](hat(italic(bar(s)))), " ","(OOB prediction)")))+
  ggtitle("Placerita") +
  annotate("text", x = 0, y = -2.4  , label = paste("MSE =", round(perf_table[7,2], 3))) + 
  annotate("text", x = 0, y = -2.8, label = "italic(R) ^ 2 == 0.248", parse = TRUE) +
  xlim(-4.5,1) +
  ylim(-3,1) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammas2d.Pla

# Combined plot
par(mfrow=c(3, 1))
combined_gammasel<-ggarrange(gammas2d.Ava, gammas2d.Hum,
                             gammas2d.Dav, gammas2d.Sta,
                             gammas2d.Ste, gammas2d.Riv,
                             gammas2d.Pla,
                             nrow = 4,
                             ncol = 2,
                             labels = NULL)

#combined_gammasel

# Save plot
ggsave("results/pipeline_v6_bees/random_forests/oob_plots/combined_plot_gammasel.pdf",
       combined_gammasel,
       device="pdf",
       width=9,
       height=12)