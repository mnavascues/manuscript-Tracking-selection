## logmu

#### Clean-up the R environment
rm(list=ls())
ls()

#### Load librarires
library(ggplot2)
library(cowplot)
library(ggpubr)

load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_vector_logmu",".RData"))
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_reg_logmu",".RData"))

oob.logmu.ava <- data.frame(x = list.logmu$Ava,
                            y = list.reg.logmu$Ava$model.rf$predictions)

oob.logmu.hum <- data.frame(x = list.logmu$Hum,
                            y = list.reg.logmu$Hum$model.rf$predictions)

oob.logmu.dav <- data.frame(x = list.logmu$Dav,
                            y = list.reg.logmu$Dav$model.rf$predictions)

oob.logmu.sta <- data.frame(x = list.logmu$Sta,
                            y = list.reg.logmu$Sta$model.rf$predictions)

oob.logmu.ste <- data.frame(x = list.logmu$Ste,
                            y = list.reg.logmu$Ste$model.rf$predictions)

oob.logmu.riv <- data.frame(x = list.logmu$Riv,
                            y = list.reg.logmu$Riv$model.rf$predictions)

oob.logmu.pla <- data.frame(x = list.logmu$Pla,
                            y = list.reg.logmu$Pla$model.rf$predictions)

## List of oob table for plot
list.table.oob.mu <- list(Ava=oob.logmu.ava, Hum=oob.logmu.hum, 
                          Dav=oob.logmu.dav, Sta=oob.logmu.sta, 
                          Ste=oob.logmu.ste, Riv=oob.logmu.riv,
                          Pla=oob.logmu.pla)

#save(list.table.oob.mu, 
#     file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_mu",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_mu",".RData"))

perf_table <- data.frame(pop = c("Ava", "Hum", "Dav", "Sta", "Ste", "Riv", "Pla"),
                         mse = NA,
                         rsq = NA)

# Non-neutral MSE and Rsquared
for (i in seq(perf_table$pop))
{
  # MSE
  perf_table[i,2] <- list.reg.logmu[[i]]$model.rf$prediction.error
  
  # Rsquared
  perf_table[i,3] <- list.reg.logmu[[i]]$model.rf$r.squared
}

# Clean-up memory
rm(list.logmu)
rm(list.reg.logmu)

# Export LOAD performance table
write.csv(perf_table,
          file = "results/pipeline_v6_bees/random_forests/performance_mu.txt", row.names = F, quote = F)

## AVALON
my_breaks_1 <- c(54.598150, 7.389056, 1)
mu.Ava   <- ggplot(list.table.oob.mu[[1]] , aes(x,y))
mu2d.Ava <- mu.Ava + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(mu))))) +
  ylab(expression(paste(log[10](italic(hat(mu))), " ","(OOB prediction)")))+
  ggtitle("Avalon") +
  annotate("text", x = -7.1, y = -9.6, label = paste("MSE =", round(perf_table[1,2], 3))) + 
  annotate("text", x = -7.1, y = -9.9, label = "italic(R) ^ 2 == 0.938", parse = TRUE) +
  xlim(-10,-6.5) +
  ylim(-10,-6.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
mu2d.Ava

## HUMBOLDT POPULATION
mu.Hum   <- ggplot(list.table.oob.mu[[2]] , aes(x,y))
mu2d.Hum <- mu.Hum + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(mu))))) +
  ylab(expression(paste(log[10](italic(hat(mu))), " ","(OOB prediction)")))+
  ggtitle("Humboldt") +
  annotate("text", x = -7.1, y = -9.6, label = paste("MSE =", round(perf_table[2,2], 3))) + 
  annotate("text", x = -7.1, y = -9.9, label = "italic(R) ^ 2 == 0.951", parse = TRUE) +
  xlim(-10,-6.5) +
  ylim(-10,-6.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
mu2d.Hum

## DAVIS POPULATION
mu.Dav   <- ggplot(list.table.oob.mu[[3]] , aes(x,y))
mu2d.Dav <- mu.Dav + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(mu))))) +
  ylab(expression(paste(log[10](italic(hat(mu))), " ","(OOB prediction)")))+
  ggtitle("Davis") +
  annotate("text", x = -7.1, y = -9.6, label = paste("MSE =", round(perf_table[3,2], 3))) + 
  annotate("text", x = -7.1, y = -9.9, label = "italic(R) ^ 2 == 0.934", parse = TRUE) +
  xlim(-10,-6.5) +
  ylim(-10,-6.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
mu2d.Dav

## STANISLAUS POPULATION
mu.Sta   <- ggplot(list.table.oob.mu[[4]] , aes(x,y))
mu2d.Sta <- mu.Sta + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(mu))))) +
  ylab(expression(paste(log[10](italic(hat(mu))), " ","(OOB prediction)")))+
  ggtitle("Stanislaus") +
  annotate("text", x = -7.1, y = -9.6, label = paste("MSE =", round(perf_table[4,2], 3))) + 
  annotate("text", x = -7.1, y = -9.9, label = "italic(R) ^ 2 == 0.932", parse = TRUE) +
  xlim(-10,-6.5) +
  ylim(-10,-6.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
mu2d.Sta

## STEBBINS POPULATION
mu.Ste   <- ggplot(list.table.oob.mu[[5]] , aes(x,y))
mu2d.Ste <- mu.Ste + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(mu))))) +
  ylab(expression(paste(log[10](italic(hat(mu))), " ","(OOB prediction)")))+
  ggtitle("Stebbins") +
  annotate("text", x = -7.1, y = -9.6, label = paste("MSE =", round(perf_table[5,2], 3))) + 
  annotate("text", x = -7.1, y = -9.9, label = "italic(R) ^ 2 == 0.941", parse = TRUE) +
  xlim(-10,-6.5) +
  ylim(-10,-6.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
mu2d.Ste

## RIVERSIDE POPULATION
mu.Riv   <- ggplot(list.table.oob.mu[[6]] , aes(x,y))
mu2d.Riv <- mu.Riv + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(mu))))) +
  ylab(expression(paste(log[10](italic(hat(mu))), " ","(OOB prediction)")))+
  ggtitle("Riverside") +
  annotate("text", x = -7.1, y = -9.6, label = paste("MSE =", round(perf_table[6,2], 3))) + 
  annotate("text", x = -7.1, y = -9.9, label = "italic(R) ^ 2 == 0.909", parse = TRUE) +
  xlim(-10,-6.5) +
  ylim(-10,-6.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
mu2d.Riv

## PLACERITA POPULATION
mu.Pla   <- ggplot(list.table.oob.mu[[7]] , aes(x,y))
mu2d.Pla <- mu.Pla + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(mu))))) +
  ylab(expression(paste(log[10](italic(hat(mu))), " ","(OOB prediction)")))+
  ggtitle("Placerita") +
  annotate("text", x = -7.1, y = -9.6, label = paste("MSE =", round(perf_table[7,2], 3))) + 
  annotate("text", x = -7.1, y = -9.9, label = "italic(R) ^ 2 == 0.937", parse = TRUE) +
  xlim(-10,-6.5) +
  ylim(-10,-6.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
mu2d.Pla

# Combined plot
par(mfrow=c(3, 1))
combined_mu<-ggarrange(mu2d.Ava, mu2d.Hum,
                       mu2d.Dav, mu2d.Sta,
                       mu2d.Ste, mu2d.Riv,
                       mu2d.Pla,
                       nrow = 4,
                       ncol = 2,
                       labels = NULL)

#combined_mu

# Save plot
ggsave("results/pipeline_v6_bees/random_forests/oob_plots/combined_plot_mu.pdf",
       combined_mu,
       device="pdf",
       width=9,
       height=12)
