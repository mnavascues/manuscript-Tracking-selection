## logmeanNe2

#### Clean-up the R environment
rm(list=ls())
ls()

#### Load librarires
library(ggplot2)
library(cowplot)
library(ggpubr)

load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_vector_logmeanNe2",".RData"))
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_reg_logmeanNe2",".RData"))

oob.logmeanNe2.ava <- data.frame(x = list.logmeanNe2$Ava,
                                 y = list.reg.logmeanNe2$Ava$model.rf$predictions)

oob.logmeanNe2.hum <- data.frame(x = list.logmeanNe2$Hum,
                                 y = list.reg.logmeanNe2$Hum$model.rf$predictions)

oob.logmeanNe2.dav <- data.frame(x = list.logmeanNe2$Dav,
                                 y = list.reg.logmeanNe2$Dav$model.rf$predictions)

oob.logmeanNe2.sta <- data.frame(x = list.logmeanNe2$Sta,
                                 y = list.reg.logmeanNe2$Sta$model.rf$predictions)

oob.logmeanNe2.ste <- data.frame(x = list.logmeanNe2$Ste,
                                 y = list.reg.logmeanNe2$Ste$model.rf$predictions)

oob.logmeanNe2.riv <- data.frame(x = list.logmeanNe2$Riv,
                                 y = list.reg.logmeanNe2$Riv$model.rf$predictions)

oob.logmeanNe2.pla <- data.frame(x = list.logmeanNe2$Pla,
                                 y = list.reg.logmeanNe2$Pla$model.rf$predictions)

## List of oob table for plot
list.table.oob.ne <- list(Ava=oob.logmeanNe2.ava, Hum=oob.logmeanNe2.hum, 
                          Dav=oob.logmeanNe2.dav, Sta=oob.logmeanNe2.sta, 
                          Ste=oob.logmeanNe2.ste, Riv=oob.logmeanNe2.riv,
                          Pla=oob.logmeanNe2.pla)

#save(list.table.oob.ne, 
#     file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_ne",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_ne",".RData"))

perf_table <- data.frame(pop = c("Ava", "Hum", "Dav", "Sta", "Ste", "Riv", "Pla"),
                         mse = NA,
                         rsq = NA)

# Non-neutral MSE and Rsquared
for (i in seq(perf_table$pop))
{
  # MSE
  perf_table[i,2] <- list.reg.logmeanNe2[[i]]$model.rf$prediction.error
  
  # Rsquared
  perf_table[i,3] <- list.reg.logmeanNe2[[i]]$model.rf$r.squared
}

# Clean-up memory
rm(list.logmeanNe2)
rm(list.reg.logmeanNe2)

# Export LOAD performance table
write.csv(perf_table,
          file = "results/pipeline_v6_bees/random_forests/performance_ne.txt", row.names = F, quote = F)

## AVALON POPULATION
my_breaks_1 <- c(54.598150, 7.389056, 1)
ne.Ava   <- ggplot(list.table.oob.ne[[1]], aes(x,y))
ne2d.Ava <- ne.Ava + 
            stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
            scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                                 ,
                                 breaks = round(my_breaks_1),labels = round(my_breaks_1)
            ) +
            xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
            ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(OOB prediction)")))+
            ggtitle("Avalon") +
            annotate("text", x = 2.8, y = -0.1, label = paste("MSE =", round(perf_table[1,2], 3))) + 
            annotate("text", x = 2.8, y = -0.4, label = "italic(R) ^ 2 == 0.937", parse = TRUE) +
            xlim(-0.5,3.5) +
            ylim(-0.5,3.5) +
            geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
            theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                               legend.position = "right", axis.text = element_text(size = 12),
                               axis.title=element_text(size=12))
ne2d.Ava

## HUMBOLDT POPULATION
#my_breaks_1 <- c(54.598150, 7.389056, 1)
ne.Hum   <- ggplot(list.table.oob.ne[[2]], aes(x,y))
ne2d.Hum <- ne.Hum + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(OOB prediction)")))+
  ggtitle("Humboldt") +
  annotate("text", x = 2.8, y = -0.1, label = paste("MSE =", round(perf_table[2,2], 3))) + 
  annotate("text", x = 2.8, y = -0.4, label = "italic(R) ^ 2 == 0.965", parse = TRUE) +
  xlim(-0.5,3.5) +
  ylim(-0.5,3.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ne2d.Hum

## DAVIS POPULATION
ne.Dav   <- ggplot(list.table.oob.ne[[3]], aes(x,y))
ne2d.Dav <- ne.Dav + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(OOB prediction)")))+
  ggtitle("Davis") +
  annotate("text", x = 2.8, y = -0.1, label = paste("MSE =", round(perf_table[3,2], 3))) + 
  annotate("text", x = 2.8, y = -0.4, label = "italic(R) ^ 2 == 0.945", parse = TRUE) +
  xlim(-0.5,3.5) +
  ylim(-0.5,3.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ne2d.Dav

## STANISLAUS POPULATION
ne.Sta   <- ggplot(list.table.oob.ne[[4]], aes(x,y))
ne2d.Sta <- ne.Sta + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(OOB prediction)")))+
  ggtitle("Stanislaus") +
  annotate("text", x = 2.8, y = -0.1, label = paste("MSE =", round(perf_table[4,2], 3))) + 
  annotate("text", x = 2.8, y = -0.4, label = "italic(R) ^ 2 == 0.948", parse = TRUE) +
  xlim(-0.5,3.5) +
  ylim(-0.5,3.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ne2d.Sta

## STEBBINS POPULATION
ne.Ste   <- ggplot(list.table.oob.ne[[5]], aes(x,y))
ne2d.Ste <- ne.Ste + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(OOB prediction)")))+
  ggtitle("Stebbins") +
  annotate("text", x = 2.8, y = -0.1, label = paste("MSE =", round(perf_table[5,2], 3))) + 
  annotate("text", x = 2.8, y = -0.4, label = "italic(R) ^ 2 == 0.956", parse = TRUE) +
  xlim(-0.5,3.5) +
  ylim(-0.5,3.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ne2d.Ste

## RIVERSIDE POPULATION
ne.Riv   <- ggplot(list.table.oob.ne[[6]], aes(x,y))
ne2d.Riv <- ne.Riv + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(OOB prediction)")))+
  ggtitle("Riverside") +
  annotate("text", x = 2.8, y = -0.1, label = paste("MSE =", round(perf_table[6,2], 3))) + 
  annotate("text", x = 2.8, y = -0.4, label = "italic(R) ^ 2 == 0.939", parse = TRUE) +
  xlim(-0.5,3.5) +
  ylim(-0.5,3.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ne2d.Riv

## PLACERITA POPULATION
my_breaks_2 <- c(20.085537, 7.389056, 2.718282, 1)
ne.Pla   <- ggplot(list.table.oob.ne[[7]], aes(x,y))
ne2d.Pla <- ne.Pla + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(OOB prediction)")))+
  ggtitle("Placerita") +
  annotate("text", x = 2.8, y = -0.1, label = paste("MSE =", round(perf_table[7,2], 3))) + 
  annotate("text", x = 2.8, y = -0.4, label = "italic(R) ^ 2 == 0.954", parse = TRUE) +
  xlim(-0.5,3.5) +
  ylim(-0.5,3.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ne2d.Pla

# Combined plot
par(mfrow=c(3, 1))
combined_ne<-ggarrange(ne2d.Ava, ne2d.Hum,
                       ne2d.Dav, ne2d.Sta,
                       ne2d.Ste, ne2d.Riv,
                       ne2d.Pla,
                       nrow = 4,
                       ncol = 2,
                       labels = NULL)

#combined_ne

# Save plot
ggsave("results/pipeline_v6_bees/random_forests/oob_plots/combined_plot_ne.pdf",
       combined_ne,
       device="pdf",
       width=9,
       height=12)
