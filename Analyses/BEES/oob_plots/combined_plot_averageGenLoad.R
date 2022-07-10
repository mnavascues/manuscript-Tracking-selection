## GENETIC LOAD

#### Clean-up the R environment
rm(list=ls())
ls()

#### Load librarires
library(ggplot2)
library(cowplot)
library(ggpubr)
library(gtools) # for inv.logit
source("src/Analyses/BEES/fun.R")

#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_vector_averageGenLoad",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_reg_averageGenLoad",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_pods/list_zero_averageGenLoad",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/posterior_pods/list_posterior_zero_averageGenLoad",".RData"))
#
#oob.lgtgl.ava <- data.frame(x = c(rep(-37.5, length(list.zero.averageGenLoad$Ava)), list.averageGenLoad$Ava),
#                            y = c(list.posterior.zero.averageGenLoad$Ava$expectation, list.reg.averageGenLoad$Ava$model.rf$predictions))
#
#oob.lgtgl.hum <- data.frame(x = c(rep(-37.5, length(list.zero.averageGenLoad$Hum)), list.averageGenLoad$Hum),
#                            y = c(list.posterior.zero.averageGenLoad$Hum$expectation, list.reg.averageGenLoad$Hum$model.rf$predictions))
#
#oob.lgtgl.dav <- data.frame(x = c(rep(-37.5, length(list.zero.averageGenLoad$Dav)), list.averageGenLoad$Dav),
#                            y = c(list.posterior.zero.averageGenLoad$Dav$expectation, list.reg.averageGenLoad$Dav$model.rf$predictions))
#
#oob.lgtgl.sta <- data.frame(x = c(rep(-37.5, length(list.zero.averageGenLoad$Sta)), list.averageGenLoad$Sta),
#                            y = c(list.posterior.zero.averageGenLoad$Sta$expectation, list.reg.averageGenLoad$Sta$model.rf$predictions))
#
#oob.lgtgl.ste <- data.frame(x = c(rep(-37.5, length(list.zero.averageGenLoad$Ste)), list.averageGenLoad$Ste),
#                            y = c(list.posterior.zero.averageGenLoad$Ste$expectation, list.reg.averageGenLoad$Ste$model.rf$predictions))
#
#oob.lgtgl.riv <- data.frame(x = c(rep(-37.5, length(list.zero.averageGenLoad$Riv)), list.averageGenLoad$Riv),
#                            y = c(list.posterior.zero.averageGenLoad$Riv$expectation, list.reg.averageGenLoad$Riv$model.rf$predictions))
#
#oob.lgtgl.pla <- data.frame(x = c(rep(-37.5, length(list.zero.averageGenLoad$Pla)), list.averageGenLoad$Pla),
#                            y = c(list.posterior.zero.averageGenLoad$Pla$expectation, list.reg.averageGenLoad$Pla$model.rf$predictions))
#
## List of oob table for plot
#list.table.oob.load <- list(Ava=oob.lgtgl.ava, Hum=oob.lgtgl.hum, 
#                            Dav=oob.lgtgl.dav, Sta=oob.lgtgl.sta, 
#                            Ste=oob.lgtgl.ste, Riv=oob.lgtgl.riv,
#                            Pla=oob.lgtgl.pla)
#
#save(list.table.oob.load, 
#    file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_load",".RData"))
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table/list_table_oob_load",".RData"))

perf_table <- data.frame(pop = c("Ava", "Hum", "Dav", "Sta", "Ste", "Riv", "Pla"),
                         mse = NA,
                         rsq = NA,
                         mse0= NA,
                         bias0= NA)

# Non-neutral MSE and Rsquared
for (i in seq(perf_table$pop))
{
  # MSE
  perf_table[i,2] <- list.reg.averageGenLoad[[i]]$model.rf$prediction.error
  
  # Rsquared
  perf_table[i,3] <- list.reg.averageGenLoad[[i]]$model.rf$r.squared
}

# Neutral MSE and BIAS
for (i in seq(perf_table$pop))
{
  # MSE
  temp <- data.frame(x=list.zero.averageGenLoad[[i]], 
                     y=inv.logit(list.posterior.zero.averageGenLoad[[i]]$expectation))
  
  perf_table[i,4] <- mse.fun(data = temp)
  # BIAS
  perf_table[i,5] <- bias.fun(data = temp)
  rm(temp)
}

# Clean-up memory
rm(list.zero.averageGenLoad)
rm(list.averageGenLoad)
rm(list.posterior.zero.averageGenLoad)
rm(list.reg.averageGenLoad)

# Export LOAD performance table
#write.csv(perf_table,
#          file = "results/pipeline_v6_bees/random_forests/perf_table/performance_load.txt", row.names = F, quote = F)


## AVALON
my_breaks_1 <- c(54.598150, 7.389056, 1)
gl.Ava   <- ggplot(list.table.oob.load[[1]], aes(x,y))
gl2d.Ava <- gl.Ava + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", logit(italic(L))))) +
  ylab(expression(paste(logit(italic(hat(L))), " ","(OOB prediction)")))+
  ggtitle("Avalon") +
  annotate("text", x = 0, y = -17, label = paste("MSE =", round(perf_table[1,2], 3))) + 
  annotate("text", x = 0, y = -19, label = "italic(R) ^ 2 == 0.494", parse = TRUE) +
  xlim(-38,7) +
  ylim(-20,7) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gl2d.Ava

## HUMBOLDT
my_breaks_2 <- c(54.598150, 7.389056, 1)
gl.Hum   <- ggplot(list.table.oob.load[[2]], aes(x,y))
gl2d.Hum <- gl.Hum + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2)
  ) +
  xlab(expression(paste("true", " ", logit(italic(L))))) +
  ylab(expression(paste(logit(italic(hat(L))), " ","(OOB prediction)")))+
  ggtitle("Humboldt") +
  annotate("text", x = 0, y = -17, label = paste("MSE =", round(perf_table[2,2], 3))) + 
  annotate("text", x = 0, y = -19, label = "italic(R) ^ 2 == 0.492", parse = TRUE) +
  xlim(-38,7) +
  ylim(-20,7) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gl2d.Hum

## Davis
my_breaks_3 <- c(54.598150, 7.389056, 1)
gl.Dav   <- ggplot(list.table.oob.load[[3]], aes(x,y))
gl2d.Dav <- gl.Dav + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_3),labels = round(my_breaks_3)
  ) +
  xlab(expression(paste("true", " ", logit(italic(L))))) +
  ylab(expression(paste(logit(italic(hat(L))), " ","(OOB prediction)")))+
  ggtitle("Davis") +
  annotate("text", x = 0, y = -17, label = paste("MSE =", round(perf_table[3,2], 3))) + 
  annotate("text", x = 0, y = -19, label = "italic(R) ^ 2 == 0.441", parse = TRUE) +
  xlim(-38,7) +
  ylim(-20,7) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gl2d.Dav

## Stanislaus
my_breaks_4 <- c(54.598150, 7.389056, 1)
gl.Sta   <- ggplot(list.table.oob.load[[4]], aes(x,y))
gl2d.Sta <- gl.Sta + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_4),labels = round(my_breaks_4)
  ) +
  xlab(expression(paste("true", " ", logit(italic(L))))) +
  ylab(expression(paste(logit(italic(hat(L))), " ","(OOB prediction)")))+
  ggtitle("Stanislaus") +
  annotate("text", x = 0, y = -17, label = paste("MSE =", round(perf_table[4,2], 3))) + 
  annotate("text", x = 0, y = -19, label = "italic(R) ^ 2 == 0.437", parse = TRUE) +
  xlim(-38,7) +
  ylim(-20,7) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gl2d.Sta

## STEBBINS
my_breaks_5 <- c(54.598150, 7.389056, 1)
gl.Ste   <- ggplot(list.table.oob.load[[5]], aes(x,y))
gl2d.Ste <- gl.Ste + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_5),labels = round(my_breaks_5)
  ) +
  xlab(expression(paste("true", " ", logit(italic(L))))) +
  ylab(expression(paste(logit(italic(hat(L))), " ","(OOB prediction)")))+
  ggtitle("Stebbins") +
  annotate("text", x = 0, y = -17, label = paste("MSE =", round(perf_table[5,2], 3))) + 
  annotate("text", x = 0, y = -19, label = "italic(R) ^ 2 == 0.409", parse = TRUE) +
  xlim(-38,7) +
  ylim(-20,7) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gl2d.Ste

## RIVERSIDE
my_breaks_6 <- c(54.598150, 7.389056, 1)
gl.Riv   <- ggplot(list.table.oob.load[[6]], aes(x,y))
gl2d.Riv <- gl.Riv + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_6),labels = round(my_breaks_6)
  ) +
  xlab(expression(paste("true", " ", logit(italic(L))))) +
  ylab(expression(paste(logit(italic(hat(L))), " ","(OOB prediction)")))+
  ggtitle("Riverside") +
  annotate("text", x = 0, y = -17, label = paste("MSE =", round(perf_table[6,2], 3))) + 
  annotate("text", x = 0, y = -19, label = "italic(R) ^ 2 == 0.353", parse = TRUE) +
  xlim(-38,7) +
  ylim(-20,7) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gl2d.Riv

## Placerita
my_breaks_7 <- c(54.598150, 7.389056, 1)
gl.Pla   <- ggplot(list.table.oob.load[[7]], aes(x,y))
gl2d.Pla <- gl.Pla + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_7),labels = round(my_breaks_7)
  ) +
  xlab(expression(paste("true", " ", logit(italic(L))))) +
  ylab(expression(paste(logit(italic(hat(L))), " ","(OOB prediction)")))+
  ggtitle("Placerita") +
  annotate("text", x = 0, y = -17, label = paste("MSE =", round(perf_table[7,2], 3))) + 
  annotate("text", x = 0, y = -19, label = "italic(R) ^ 2 == 0.388", parse = TRUE) +
  xlim(-38,7) +
  ylim(-20,7) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gl2d.Pla

# Combined plot
par(mfrow=c(3, 1))
combined_load<-ggarrange(gl2d.Ava, gl2d.Hum,
                          gl2d.Dav,gl2d.Sta,
                          gl2d.Ste,gl2d.Riv,
                          gl2d.Pla,
                          nrow = 4,
                          ncol = 2,
                          labels = NULL)

#combined_load

# Save plot
ggsave("results/pipeline_v6_bees/random_forests/oob_plots/combined_plot_load.pdf",
       combined_load,
       device="pdf",
       width=9,
       height=12)

