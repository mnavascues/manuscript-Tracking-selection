## thetab

#### Clean-up the R environment
rm(list=ls())
ls()

#### Load librarires
library(ggplot2)
library(cowplot)
library(ggpubr)

load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_vector_logthetaPS",".RData"))
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_reg_logthetaPS",".RData"))

oob.logthetaPS.ava <- data.frame(x = list.logthetaPS$Ava,
                                 y = list.reg.logthetaPS$Ava$model.rf$predictions)

oob.logthetaPS.hum <- data.frame(x = list.logthetaPS$Hum,
                                 y = list.reg.logthetaPS$Hum$model.rf$predictions)

oob.logthetaPS.dav <- data.frame(x = list.logthetaPS$Dav,
                                 y = list.reg.logthetaPS$Dav$model.rf$predictions)

oob.logthetaPS.sta <- data.frame(x = list.logthetaPS$Sta,
                                 y = list.reg.logthetaPS$Sta$model.rf$predictions)

oob.logthetaPS.ste <- data.frame(x = list.logthetaPS$Ste,
                                 y = list.reg.logthetaPS$Ste$model.rf$predictions)

oob.logthetaPS.riv <- data.frame(x = list.logthetaPS$Riv,
                                 y = list.reg.logthetaPS$Riv$model.rf$predictions)

oob.logthetaPS.pla <- data.frame(x = list.logthetaPS$Pla,
                                 y = list.reg.logthetaPS$Pla$model.rf$predictions)

## List of oob table for plot
list.table.oob.thetab <- list(Ava=oob.logthetaPS.ava, Hum=oob.logthetaPS.hum, 
                              Dav=oob.logthetaPS.dav, Sta=oob.logthetaPS.sta, 
                              Ste=oob.logthetaPS.ste, Riv=oob.logthetaPS.riv,
                              Pla=oob.logthetaPS.pla)

#save(list.table.oob.thetab, 
#     file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_thetab",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_thetab",".RData"))

perf_table <- data.frame(pop = c("Ava", "Hum", "Dav", "Sta", "Ste", "Riv", "Pla"),
                         mse = NA,
                         rsq = NA)

# Non-neutral MSE and Rsquared
for (i in seq(perf_table$pop))
{
  # MSE
  perf_table[i,2] <- list.reg.logthetaPS[[i]]$model.rf$prediction.error
  
  # Rsquared
  perf_table[i,3] <- list.reg.logthetaPS[[i]]$model.rf$r.squared
}

# Clean-up memory
rm(list.logthetaPS)
rm(list.reg.logthetaPS)

# Export LOAD performance table
write.csv(perf_table,
          file = "results/pipeline_v6_bees/random_forests/performance_thetab.txt", row.names = F, quote = F)

## AVALON
my_breaks_1 <- c(20.085537, 7.389056, 2.718282 , 1)
thetaPS.Ava   <- ggplot(list.table.oob.thetab[[1]], aes(x,y))
thetaPS2d.Ava <- thetaPS.Ava + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(theta)[b])))) +
  ylab(expression(paste(log[10](italic(hat(theta))[b]), " ","(OOB prediction)")))+
  ggtitle("Avalon") +
  annotate("text", x = 4, y = -6.8, label = paste("MSE =", round(perf_table[1,2], 3))) + 
  annotate("text", x = 4, y = -7.8, label = "italic(R) ^ 2 == 0.570", parse = TRUE) +
  xlim(-8,6) +
  ylim(-8,6) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
thetaPS2d.Ava

## HUMBOLDT
my_breaks_2 <- c(54.598150, 7.389056, 1)
thetaPS.Hum   <- ggplot(list.table.oob.thetab[[2]], aes(x,y))
thetaPS2d.Hum <- thetaPS.Hum + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(theta)[b])))) +
  ylab(expression(paste(log[10](italic(hat(theta))[b]), " ","(OOB prediction)")))+
  ggtitle("Humboldt") +
  annotate("text", x = 4, y = -6.8, label = paste("MSE =", round(perf_table[2,2], 3))) + 
  annotate("text", x = 4, y = -7.8, label = "italic(R) ^ 2 == 0.568", parse = TRUE) +
  xlim(-8,6) +
  ylim(-8,6) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
thetaPS2d.Hum

## DAVIS
my_breaks_3 <- c(20.085537, 7.389056, 2.718282 , 1)
thetaPS.Dav   <- ggplot(list.table.oob.thetab[[3]], aes(x,y))
thetaPS2d.Dav <- thetaPS.Dav + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_3),labels = round(my_breaks_3)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(theta)[b])))) +
  ylab(expression(paste(log[10](italic(hat(theta))[b]), " ","(OOB prediction)")))+
  ggtitle("Davis") +
  annotate("text", x = 4, y = -6.8, label = paste("MSE =", round(perf_table[3,2], 3))) + 
  annotate("text", x = 4, y = -7.8, label = "italic(R) ^ 2 == 0.534", parse = TRUE) +
  xlim(-8,6) +
  ylim(-8,6) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
thetaPS2d.Dav

## STANISLAUS
my_breaks_4 <- c(20.085537, 7.389056, 2.718282 , 1)
thetaPS.Sta   <- ggplot(list.table.oob.thetab[[4]], aes(x,y))
thetaPS2d.Sta <- thetaPS.Sta + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_4),labels = round(my_breaks_4)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(theta)[b])))) +
  ylab(expression(paste(log[10](italic(hat(theta))[b]), " ","(OOB prediction)")))+
  ggtitle("Stanislaus") +
  annotate("text", x = 4, y = -6.8, label = paste("MSE =", round(perf_table[4,2], 3))) + 
  annotate("text", x = 4, y = -7.8, label = "italic(R) ^ 2 == 0.536", parse = TRUE) +
  xlim(-8,6) +
  ylim(-8,6) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
thetaPS2d.Sta

## STEBBINS
my_breaks_5 <- c(20.085537, 7.389056, 2.718282 , 1)
thetaPS.Ste   <- ggplot(list.table.oob.thetab[[5]], aes(x,y))
thetaPS2d.Ste <- thetaPS.Ste + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_5),labels = round(my_breaks_5)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(theta)[b])))) +
  ylab(expression(paste(log[10](italic(hat(theta))[b]), " ","(OOB prediction)")))+
  ggtitle("Stebbins") +
  annotate("text", x = 4, y = -6.8, label = paste("MSE =", round(perf_table[5,2], 3))) + 
  annotate("text", x = 4, y = -7.8, label = "italic(R) ^ 2 == 0.531", parse = TRUE) +
  xlim(-8,6) +
  ylim(-8,6) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
thetaPS2d.Ste

## RIVERSIDE
my_breaks_6 <- c(20.085537, 7.389056, 2.718282 , 1)
thetaPS.Riv   <- ggplot(list.table.oob.thetab[[6]], aes(x,y))
thetaPS2d.Riv <- thetaPS.Riv + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_6),labels = round(my_breaks_6)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(theta)[b])))) +
  ylab(expression(paste(log[10](italic(hat(theta))[b]), " ","(OOB prediction)")))+
  ggtitle("Riverside") +
  annotate("text", x = 4, y = -6.8, label = paste("MSE =", round(perf_table[6,2], 3))) + 
  annotate("text", x = 4, y = -7.8, label = "italic(R) ^ 2 == 0.491", parse = TRUE) +
  xlim(-8,6) +
  ylim(-8,6) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
thetaPS2d.Riv

## PLACERITA
my_breaks_7 <- c(20.085537, 7.389056, 2.718282 , 1)
thetaPS.Pla   <- ggplot(list.table.oob.thetab[[7]], aes(x,y))
thetaPS2d.Pla <- thetaPS.Pla + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_7),labels = round(my_breaks_7)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(theta)[b])))) +
  ylab(expression(paste(log[10](italic(hat(theta))[b]), " ","(OOB prediction)")))+
  ggtitle("Placerita") +
  annotate("text", x = 4, y = -6.8, label = paste("MSE =", round(perf_table[7,2], 3))) + 
  annotate("text", x = 4, y = -7.8, label = "italic(R) ^ 2 == 0.527", parse = TRUE) +
  xlim(-8,6) +
  ylim(-8,6) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
thetaPS2d.Pla

# Combined plot
par(mfrow=c(3, 1))
combined_thetab<-ggarrange(thetaPS2d.Ava, thetaPS2d.Hum,
                           thetaPS2d.Dav, thetaPS2d.Sta,
                           thetaPS2d.Ste, thetaPS2d.Riv,
                           thetaPS2d.Pla,
                           nrow = 4,
                           ncol = 2,
                           labels = NULL)

#combined_thetab

# Save plot
ggsave("results/pipeline_v6_bees/random_forests/oob_plots/combined_plot_thetab.pdf",
       combined_thetab,
       device="pdf",
       width=9,
       height=12)
