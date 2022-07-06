## loggammamean

#### Clean-up the R environment
rm(list=ls())
ls()

#### Load librarires
library(ggplot2)
library(cowplot)
library(ggpubr)

load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_vector_loggammamean",".RData"))
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_reg_loggammamean",".RData"))

oob.loggammamean.ava <- data.frame(x = list.loggammamean$Ava,
                                   y = list.reg.loggammamean$Ava$model.rf$predictions)

oob.loggammamean.hum <- data.frame(x = list.loggammamean$Hum,
                                   y = list.reg.loggammamean$Hum$model.rf$predictions)

oob.loggammamean.dav <- data.frame(x = list.loggammamean$Dav,
                                   y = list.reg.loggammamean$Dav$model.rf$predictions)

oob.loggammamean.sta <- data.frame(x = list.loggammamean$Sta,
                                   y = list.reg.loggammamean$Sta$model.rf$predictions)

oob.loggammamean.ste <- data.frame(x = list.loggammamean$Ste,
                                   y = list.reg.loggammamean$Ste$model.rf$predictions)

oob.loggammamean.riv <- data.frame(x = list.loggammamean$Riv,
                                   y = list.reg.loggammamean$Riv$model.rf$predictions)

oob.loggammamean.pla <- data.frame(x = list.loggammamean$Pla,
                                   y = list.reg.loggammamean$Pla$model.rf$predictions)

## List of oob table for plot
list.table.oob.gamma <- list(Ava=oob.loggammamean.ava, Hum=oob.loggammamean.hum, 
                             Dav=oob.loggammamean.dav, Sta=oob.loggammamean.sta, 
                             Ste=oob.loggammamean.ste, Riv=oob.loggammamean.riv,
                             Pla=oob.loggammamean.pla)

save(list.table.oob.gamma, 
    file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_gamma",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_gamma",".RData"))

perf_table <- data.frame(pop = c("Ava", "Hum", "Dav", "Sta", "Ste", "Riv", "Pla"),
                         mse = NA,
                         rsq = NA)

# Non-neutral MSE and Rsquared
for (i in seq(perf_table$pop))
{
  # MSE
  perf_table[i,2] <- list.reg.loggammamean[[i]]$model.rf$prediction.error
  
  # Rsquared
  perf_table[i,3] <- list.reg.loggammamean[[i]]$model.rf$r.squared
}

# Clean-up memory
rm(list.loggammamean)
rm(list.reg.loggammamean)

# Export LOAD performance table
write.csv(perf_table,
          file = "results/pipeline_v6_bees/random_forests/performance_gamma.txt", row.names = F, quote = F)

## AVALON
my_breaks_1 <- c(7.389056, 2.718282, 1)
gammamean.Ava   <- ggplot(list.table.oob.gamma[[1]], aes(x,y))
gammamean2d.Ava <- gammamean.Ava + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1),
  ) +
  xlab(expression(paste("true", " ", log[10](italic(gamma))))) +
  ylab(expression(paste(log[10](italic(hat(gamma))), " ","(OOB prediction)")))+
  ggtitle("Avalon") +
  annotate("text", x = -0.7, y = -2.6, label = paste("MSE =", round(perf_table[1,2], 3))) + 
  annotate("text", x = -0.7, y = -2.9, label = "italic(R) ^ 2 == 0.154", parse = TRUE) +
  xlim(-3,0) +
  ylim(-3,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammamean2d.Ava

## HUMBOLDT
my_breaks_2 <- c(7.389056, 2.718282, 1)
gammamean.Hum   <- ggplot(list.table.oob.gamma[[2]], aes(x,y))
gammamean2d.Hum <- gammamean.Hum + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2),
  ) +
  xlab(expression(paste("true", " ", log[10](italic(gamma))))) +
  ylab(expression(paste(log[10](italic(hat(gamma))), " ","(OOB prediction)")))+
  ggtitle("Humboldt") +
  annotate("text", x = -0.7, y = -2.6, label = paste("MSE =", round(perf_table[2,2], 3))) + 
  annotate("text", x = -0.7, y = -2.9, label = "italic(R) ^ 2 == 0.146", parse = TRUE) +
  xlim(-3,0) +
  ylim(-3,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammamean2d.Hum

## DAVIS
my_breaks_3 <- c(7.389056, 2.718282, 1)
gammamean.Dav   <- ggplot(list.table.oob.gamma[[3]], aes(x,y))
gammamean2d.Dav <- gammamean.Dav + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_3),labels = round(my_breaks_3),
  ) +
  xlab(expression(paste("true", " ", log[10](italic(gamma))))) +
  ylab(expression(paste(log[10](italic(hat(gamma))), " ","(OOB prediction)")))+
  ggtitle("Davis") +
  annotate("text", x = -0.7, y = -2.6, label = paste("MSE =", round(perf_table[3,2], 3))) + 
  annotate("text", x = -0.7, y = -2.9, label = "italic(R) ^ 2 == 0.140", parse = TRUE) +
  xlim(-3,0) +
  ylim(-3,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammamean2d.Dav

## STANISLAUS
my_breaks_4 <- c(7.389056, 2.718282, 1)
gammamean.Sta   <- ggplot(list.table.oob.gamma[[4]], aes(x,y))
gammamean2d.Sta <- gammamean.Sta + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_4),labels = round(my_breaks_4),
  ) +
  xlab(expression(paste("true", " ", log[10](italic(gamma))))) +
  ylab(expression(paste(log[10](italic(hat(gamma))), " ","(OOB prediction)")))+
  ggtitle("Stanislaus") +
  annotate("text", x = -0.7, y = -2.6, label = paste("MSE =", round(perf_table[4,2], 3))) + 
  annotate("text", x = -0.7, y = -2.9, label = "italic(R) ^ 2 == 0.146", parse = TRUE) +
  xlim(-3,0) +
  ylim(-3,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammamean2d.Sta

## STEBBINS
my_breaks_5 <- c(7.389056, 2.718282, 1)
gammamean.Ste   <- ggplot(list.table.oob.gamma[[5]], aes(x,y))
gammamean2d.Ste <- gammamean.Ste + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_5),labels = round(my_breaks_5),
  ) +
  xlab(expression(paste("true", " ", log[10](italic(gamma))))) +
  ylab(expression(paste(log[10](italic(hat(gamma))), " ","(OOB prediction)")))+
  ggtitle("Stebbins") +
  annotate("text", x = -0.7, y = -2.6, label = paste("MSE =", round(perf_table[5,2], 3))) + 
  annotate("text", x = -0.7, y = -2.9, label = "italic(R) ^ 2 == 0.144", parse = TRUE) +
  xlim(-3,0) +
  ylim(-3,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammamean2d.Ste

## RIVERSIDE
my_breaks_6 <- c(7.389056, 2.718282, 1)
gammamean.Riv   <- ggplot(list.table.oob.gamma[[6]], aes(x,y))
gammamean2d.Riv <- gammamean.Riv + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_6),labels = round(my_breaks_6),
  ) +
  xlab(expression(paste("true", " ", log[10](italic(gamma))))) +
  ylab(expression(paste(log[10](italic(hat(gamma))), " ","(OOB prediction)")))+
  ggtitle("Riverside") +
  annotate("text", x = -0.7, y = -2.6, label = paste("MSE =", round(perf_table[6,2], 3))) + 
  annotate("text", x = -0.7, y = -2.9, label = "italic(R) ^ 2 == 0.139", parse = TRUE) +
  xlim(-3,0) +
  ylim(-3,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammamean2d.Riv

## PLACERITA
my_breaks_7 <- c(7.389056, 2.718282, 1)
gammamean.Pla   <- ggplot(list.table.oob.gamma[[7]], aes(x,y))
gammamean2d.Pla <- gammamean.Pla + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_7),labels = round(my_breaks_7),
  ) +
  xlab(expression(paste("true", " ", log[10](italic(gamma))))) +
  ylab(expression(paste(log[10](italic(hat(gamma))), " ","(OOB prediction)")))+
  ggtitle("Placerita") +
  annotate("text", x = -0.7, y = -2.6, label = paste("MSE =", round(perf_table[7,2], 3))) + 
  annotate("text", x = -0.7, y = -2.9, label = "italic(R) ^ 2 == 0.142", parse = TRUE) +
  xlim(-3,0) +
  ylim(-3,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
gammamean2d.Pla

# Combined plot
par(mfrow=c(3, 1))
combined_gamma<-ggarrange(gammamean2d.Ava, gammamean2d.Hum,
                          gammamean2d.Dav, gammamean2d.Sta,
                          gammamean2d.Ste, gammamean2d.Riv,
                          gammamean2d.Pla,
                          nrow = 4,
                          ncol = 2,
                          labels = NULL)

#combined_gamma

# Save plot
ggsave("results/pipeline_v6_bees/random_forests/oob_plots/combined_plot_gamma.pdf",
       combined_gamma,
       device="pdf",
       width=9,
       height=12)
