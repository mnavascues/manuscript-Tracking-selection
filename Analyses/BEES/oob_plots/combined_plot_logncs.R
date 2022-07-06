## logncs

#### Clean-up the R environment
rm(list=ls())
ls()

#### Load librarires
library(ggplot2)
library(cowplot)
library(ggpubr)

load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_vector_logncs",".RData"))
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_reg_logncs",".RData"))

oob.logncs.ava <- data.frame(x = list.logncs$Ava,
                             y = list.reg.logncs$Ava$model.rf$predictions)

oob.logncs.hum <- data.frame(x = list.logncs$Hum,
                             y = list.reg.logncs$Hum$model.rf$predictions)

oob.logncs.dav <- data.frame(x = list.logncs$Dav,
                             y = list.reg.logncs$Dav$model.rf$predictions)

oob.logncs.sta <- data.frame(x = list.logncs$Sta,
                             y = list.reg.logncs$Sta$model.rf$predictions)

oob.logncs.ste <- data.frame(x = list.logncs$Ste,
                             y = list.reg.logncs$Ste$model.rf$predictions)

oob.logncs.riv <- data.frame(x = list.logncs$Riv,
                             y = list.reg.logncs$Riv$model.rf$predictions)

oob.logncs.pla <- data.frame(x = list.logncs$Pla,
                             y = list.reg.logncs$Pla$model.rf$predictions)

## List of oob table for plot
list.table.oob.nc <- list(Ava=oob.logncs.ava, Hum=oob.logncs.hum, 
                          Dav=oob.logncs.dav, Sta=oob.logncs.sta, 
                          Ste=oob.logncs.ste, Riv=oob.logncs.riv,
                          Pla=oob.logncs.pla)

#save(list.table.oob.nc, 
#     file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_nc",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_nc",".RData"))

perf_table <- data.frame(pop = c("Ava", "Hum", "Dav", "Sta", "Ste", "Riv", "Pla"),
                         mse = NA,
                         rsq = NA)

# Non-neutral MSE and Rsquared
for (i in seq(perf_table$pop))
{
  # MSE
  perf_table[i,2] <- list.reg.logncs[[i]]$model.rf$prediction.error
  
  # Rsquared
  perf_table[i,3] <- list.reg.logncs[[i]]$model.rf$r.squared
}

# Clean-up memory
rm(list.logncs)
rm(list.reg.logncs)

# Export LOAD performance table
write.csv(perf_table,
          file = "results/pipeline_v6_bees/random_forests/performance_nc.txt", row.names = F, quote = F)

## AVALON
my_breaks_1 <- c(54.598150, 7.389056, 1)
ncs.Ava   <- ggplot(list.table.oob.nc[[1]], aes(x,y))
ncs2d.Ava <- ncs.Ava + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))), " ","(OOB prediction)")))+
  ggtitle("Avalon") +
  annotate("text", x = 2.8, y =  0.0, label = paste("MSE =", round(perf_table[1,2], 3))) + 
  annotate("text", x = 2.8, y = -0.3, label = "italic(R) ^ 2 == 0.898", parse = TRUE) +
  xlim(-0.5,3.5) +
  ylim(-0.5,3.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ncs2d.Ava

## HUMBOLDT POPULATION
ncs.Hum   <- ggplot(list.table.oob.nc[[2]], aes(x,y))
ncs2d.Hum <- ncs.Hum + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))), " ","(OOB prediction)")))+
  ggtitle("Humboldt") +
  annotate("text", x = 2.8, y =  0.0, label = paste("MSE =", round(perf_table[2,2], 3))) + 
  annotate("text", x = 2.8, y = -0.3, label = "italic(R) ^ 2 == 0.917", parse = TRUE) +
  xlim(-0.5,3.5) +
  ylim(-0.5,3.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ncs2d.Hum

## DAVIS POPULATION
ncs.Dav   <- ggplot(list.table.oob.nc[[3]], aes(x,y))
ncs2d.Dav <- ncs.Dav + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))), " ","(OOB prediction)")))+
  ggtitle("Davis") +
  annotate("text", x = 2.8, y =  0.0, label = paste("MSE =", round(perf_table[3,2], 3))) + 
  annotate("text", x = 2.8, y = -0.3, label = "italic(R) ^ 2 == 0.889", parse = TRUE) +
  xlim(-0.5,3.5) +
  ylim(-0.5,3.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ncs2d.Dav

## STANISLAUS POPULATION
ncs.Sta   <- ggplot(list.table.oob.nc[[4]], aes(x,y))
ncs2d.Sta <- ncs.Sta + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))), " ","(OOB prediction)")))+
  ggtitle("Stanislaus") +
  annotate("text", x = 2.8, y =  0.0, label = paste("MSE =", round(perf_table[4,2], 3))) + 
  annotate("text", x = 2.8, y = -0.3, label = "italic(R) ^ 2 == 0.904", parse = TRUE) +
  xlim(-0.5,3.5) +
  ylim(-0.5,3.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ncs2d.Sta

## STEBBINS POPULATION
ncs.Ste   <- ggplot(list.table.oob.nc[[5]], aes(x,y))
ncs2d.Ste <- ncs.Ste + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))), " ","(OOB prediction)")))+
  ggtitle("Stebbins") +
  annotate("text", x = 2.8, y =  0.0, label = paste("MSE =", round(perf_table[5,2], 3))) + 
  annotate("text", x = 2.8, y = -0.3, label = "italic(R) ^ 2 == 0.921", parse = TRUE) +
  xlim(-0.5,3.5) +
  ylim(-0.5,3.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ncs2d.Ste

## RIVERSIDE POPULATION
ncs.Riv   <- ggplot(list.table.oob.nc[[6]], aes(x,y))
ncs2d.Riv <- ncs.Riv + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))), " ","(OOB prediction)")))+
  ggtitle("Riverside") +
  annotate("text", x = 2.8, y =  0.0, label = paste("MSE =", round(perf_table[6,2], 3))) + 
  annotate("text", x = 2.8, y = -0.3, label = "italic(R) ^ 2 == 0.894", parse = TRUE) +
  xlim(-0.5,3.5) +
  ylim(-0.5,3.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ncs2d.Riv

## PLACERITA POPULATION
ncs.Pla   <- ggplot(list.table.oob.nc[[7]], aes(x,y))
ncs2d.Pla <- ncs.Pla + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))), " ","(OOB prediction)")))+
  ggtitle("Placerita") +
  annotate("text", x = 2.8, y =  0.0, label = paste("MSE =", round(perf_table[7,2], 3))) + 
  annotate("text", x = 2.8, y = -0.3, label = "italic(R) ^ 2 == 0.918", parse = TRUE) +
  xlim(-0.5,3.5) +
  ylim(-0.5,3.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
ncs2d.Pla

# Combined plot
par(mfrow=c(3, 1))
combined_nc<-ggarrange(ncs2d.Ava, ncs2d.Hum,
                       ncs2d.Dav, ncs2d.Sta,
                       ncs2d.Ste, ncs2d.Riv,
                       ncs2d.Pla,
                       nrow = 4,
                       ncol = 2,
                       labels = NULL)

#combined_nc

# Save plot
ggsave("results/pipeline_v6_bees/random_forests/oob_plots/combined_plot_nc.pdf",
       combined_nc,
       device="pdf",
       width=9,
       height=12)
