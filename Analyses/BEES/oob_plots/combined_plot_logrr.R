## logrr

#### Clean-up the R environment
rm(list=ls())
ls()

#### Load librarires
library(ggplot2)
library(cowplot)
library(ggpubr)

load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_vector_logrr",".RData"))
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_reg_logrr",".RData"))

oob.logrr.ava <- data.frame(x = list.logrr$Ava,
                            y = list.reg.logrr$Ava$model.rf$predictions)

oob.logrr.hum <- data.frame(x = list.logrr$Hum,
                            y = list.reg.logrr$Hum$model.rf$predictions)

oob.logrr.dav <- data.frame(x = list.logrr$Dav,
                            y = list.reg.logrr$Dav$model.rf$predictions)

oob.logrr.sta <- data.frame(x = list.logrr$Sta,
                            y = list.reg.logrr$Sta$model.rf$predictions)

oob.logrr.ste <- data.frame(x = list.logrr$Ste,
                            y = list.reg.logrr$Ste$model.rf$predictions)

oob.logrr.riv <- data.frame(x = list.logrr$Riv,
                            y = list.reg.logrr$Riv$model.rf$predictions)

oob.logrr.pla <- data.frame(x = list.logrr$Pla,
                            y = list.reg.logrr$Pla$model.rf$predictions)

## List of oob table for plot
list.table.oob.rr <- list(Ava=oob.logrr.ava, Hum=oob.logrr.hum, 
                          Dav=oob.logrr.dav, Sta=oob.logrr.sta, 
                          Ste=oob.logrr.ste, Riv=oob.logrr.riv,
                          Pla=oob.logrr.pla)

#save(list.table.oob.rr, 
#     file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_rr",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_rr",".RData"))

perf_table <- data.frame(pop = c("Ava", "Hum", "Dav", "Sta", "Ste", "Riv", "Pla"),
                         mse = NA,
                         rsq = NA)

# Non-neutral MSE and Rsquared
for (i in seq(perf_table$pop))
{
  # MSE
  perf_table[i,2] <- list.reg.logrr[[i]]$model.rf$prediction.error
  
  # Rsquared
  perf_table[i,3] <- list.reg.logrr[[i]]$model.rf$r.squared
}

# Clean-up memory
rm(list.logrr)
rm(list.reg.logrr)

# Export LOAD performance table
write.csv(perf_table,
          file = "results/pipeline_v6_bees/random_forests/performance_rr.txt", row.names = F, quote = F)

## AVALON
my_breaks_1 <- c(7.389056, 2.718282, 1)
rr.Ava   <- ggplot(list.table.oob.rr[[1]], aes(x,y))
rr2d.Ava <- rr.Ava + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(r))))) +
  ylab(expression(paste(log[10](hat(italic(r))), " ","(OOB prediction)")))+
  ggtitle("Avalon") +
  annotate("text", x = -4.8, y = -7.5, label = paste("MSE =", round(perf_table[1,2], 3))) + 
  annotate("text", x = -4.8, y = -7.9, label = "italic(R) ^ 2 == 0.294", parse = TRUE) +
  xlim(-8,-4) +
  ylim(-8,-4) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
rr2d.Ava

## HUMBOLDT POPULATION
rr.Hum   <- ggplot(list.table.oob.rr[[2]], aes(x,y))
rr2d.Hum <- rr.Hum + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(r))))) +
  ylab(expression(paste(log[10](hat(italic(r))), " ","(OOB prediction)")))+
  ggtitle("Humboldt") +
  annotate("text", x = -4.8, y = -7.5, label = paste("MSE =", round(perf_table[2,2], 3))) + 
  annotate("text", x = -4.8, y = -7.9, label = "italic(R) ^ 2 == 0.229", parse = TRUE) +
  xlim(-8,-4) +
  ylim(-8,-4) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
rr2d.Hum

## DAVIS POPULATION
rr.Dav   <- ggplot(list.table.oob.rr[[3]], aes(x,y))
rr2d.Dav <- rr.Dav + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(r))))) +
  ylab(expression(paste(log[10](hat(italic(r))), " ","(OOB prediction)")))+
  ggtitle("Davis") +
  annotate("text", x = -4.8, y = -7.5, label = paste("MSE =", round(perf_table[3,2], 3))) + 
  annotate("text", x = -4.8, y = -7.9, label = "italic(R) ^ 2 == 0.219", parse = TRUE) +
  xlim(-8,-4) +
  ylim(-8,-4) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
rr2d.Dav

## STANISLAUS POPULATION
rr.Sta   <- ggplot(list.table.oob.rr[[4]], aes(x,y))
rr2d.Sta <- rr.Sta + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(r))))) +
  ylab(expression(paste(log[10](hat(italic(r))), " ","(OOB prediction)")))+
  ggtitle("Stanislaus") +
  annotate("text", x = -4.8, y = -7.5, label = paste("MSE =", round(perf_table[4,2], 3))) + 
  annotate("text", x = -4.8, y = -7.9, label = "italic(R) ^ 2 == 0.223", parse = TRUE) +
  xlim(-8,-4) +
  ylim(-8,-4) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
rr2d.Sta

## STEBBINS POPULATION
rr.Ste   <- ggplot(list.table.oob.rr[[5]], aes(x,y))
rr2d.Ste <- rr.Ste + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(r))))) +
  ylab(expression(paste(log[10](hat(italic(r))), " ","(OOB prediction)")))+
  ggtitle("Stebbins") +
  annotate("text", x = -4.8, y = -7.5, label = paste("MSE =", round(perf_table[5,2], 3))) + 
  annotate("text", x = -4.8, y = -7.9, label = "italic(R) ^ 2 == 0.209", parse = TRUE) +
  xlim(-8,-4) +
  ylim(-8,-4) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
rr2d.Ste

## RIVERSIDE POPULATION
rr.Riv   <- ggplot(list.table.oob.rr[[6]], aes(x,y))
rr2d.Riv <- rr.Riv + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(r))))) +
  ylab(expression(paste(log[10](hat(italic(r))), " ","(OOB prediction)")))+
  ggtitle("Riverside") +
  annotate("text", x = -4.8, y = -7.5, label = paste("MSE =", round(perf_table[6,2], 3))) + 
  annotate("text", x = -4.8, y = -7.9, label = "italic(R) ^ 2 == 0.206", parse = TRUE) +
  xlim(-8,-4) +
  ylim(-8,-4) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
rr2d.Riv

## PLACERITA POPULATION
rr.Pla   <- ggplot(list.table.oob.rr[[7]], aes(x,y))
rr2d.Pla <- rr.Pla + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(r))))) +
  ylab(expression(paste(log[10](hat(italic(r))), " ","(OOB prediction)")))+
  ggtitle("Placerita") +
  annotate("text", x = -4.8, y = -7.5, label = paste("MSE =", round(perf_table[7,2], 3))) + 
  annotate("text", x = -4.8, y = -7.9, label = "italic(R) ^ 2 == 0.208", parse = TRUE) +
  xlim(-8,-4) +
  ylim(-8,-4) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
rr2d.Pla

# Combined plot
par(mfrow=c(3, 1))
combined_rr<-ggarrange(rr2d.Ava, rr2d.Hum,
                       rr2d.Dav, rr2d.Sta,
                       rr2d.Ste, rr2d.Riv,
                       rr2d.Pla,
                       nrow = 4,
                       ncol = 2,
                       labels = NULL)

#combined_rr

# Save plot
ggsave("results/pipeline_v6_bees/random_forests/oob_plots/combined_plot_rr.pdf",
       combined_rr,
       device="pdf",
       width=9,
       height=12)
