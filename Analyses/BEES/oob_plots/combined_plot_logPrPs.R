## logPrPs

#### Clean-up the R environment
rm(list=ls())
ls()

#### Load librarires
library(ggplot2)
library(cowplot)
library(ggpubr)

load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_vector_logPrPs",".RData"))
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_reg_logPrPs",".RData"))

oob.logPrPs.ava <- data.frame(x = list.logPrPs$Ava,
                              y = list.reg.logPrPs$Ava$model.rf$predictions)

oob.logPrPs.hum <- data.frame(x = list.logPrPs$Hum,
                              y = list.reg.logPrPs$Hum$model.rf$predictions)

oob.logPrPs.dav <- data.frame(x = list.logPrPs$Dav,
                              y = list.reg.logPrPs$Dav$model.rf$predictions)

oob.logPrPs.sta <- data.frame(x = list.logPrPs$Sta,
                              y = list.reg.logPrPs$Sta$model.rf$predictions)

oob.logPrPs.ste <- data.frame(x = list.logPrPs$Ste,
                              y = list.reg.logPrPs$Ste$model.rf$predictions)

oob.logPrPs.riv <- data.frame(x = list.logPrPs$Riv,
                              y = list.reg.logPrPs$Riv$model.rf$predictions)

oob.logPrPs.pla <- data.frame(x = list.logPrPs$Pla,
                              y = list.reg.logPrPs$Pla$model.rf$predictions)

## List of oob table for plot
list.table.oob.prps <- list(Ava=oob.logPrPs.ava, Hum=oob.logPrPs.hum, 
                            Dav=oob.logPrPs.dav, Sta=oob.logPrPs.sta, 
                            Ste=oob.logPrPs.ste, Riv=oob.logPrPs.riv,
                            Pla=oob.logPrPs.pla)

#save(list.table.oob.prps, 
#     file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_prps",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_prps",".RData"))

perf_table <- data.frame(pop = c("Ava", "Hum", "Dav", "Sta", "Ste", "Riv", "Pla"),
                         mse = NA,
                         rsq = NA)

# Non-neutral MSE and Rsquared
for (i in seq(perf_table$pop))
{
  # MSE
  perf_table[i,2] <- list.reg.logPrPs[[i]]$model.rf$prediction.error
  
  # Rsquared
  perf_table[i,3] <- list.reg.logPrPs[[i]]$model.rf$r.squared
}

# Clean-up memory
rm(list.logPrPs)
rm(list.reg.logPrPs)

# Export LOAD performance table
write.csv(perf_table,
          file = "results/pipeline_v6_bees/random_forests/performance_prps.txt", row.names = F, quote = F)

## AVALON
my_breaks_1 <- c(7.389056, 2.718282, 1)
prps.Ava   <- ggplot(list.table.oob.prps[[1]], aes(x,y))
prps2d.Ava <- prps.Ava + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(P)[R] * italic(P)[B])))) +
  ylab(expression(paste(log[10](hat(italic(P)[R] * italic(P)[B])), " ","(OOB prediction)")))+
  ggtitle("Avalon") +
  annotate("text", x = -1.0, y = -5.4, label = paste("MSE =", round(perf_table[1,2], 3))) + 
  annotate("text", x = -1.0, y = -5.9, label = "italic(R) ^ 2 == 0.388", parse = TRUE) +
  xlim(-6,0) +
  ylim(-6,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
prps2d.Ava

## HUMBOLDT
my_breaks_2 <- c(7.389056, 2.718282, 1)
prps.Hum   <- ggplot(list.table.oob.prps[[2]], aes(x,y))
prps2d.Hum <- prps.Hum + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(P)[R] * italic(P)[B])))) +
  ylab(expression(paste(log[10](hat(italic(P)[R] * italic(P)[B])), " ","(OOB prediction)")))+
  ggtitle("Humboldt") +
  annotate("text", x = -1.0, y = -5.4, label = paste("MSE =", round(perf_table[2,2], 3))) + 
  annotate("text", x = -1.0, y = -5.9, label = "italic(R) ^ 2 == 0.373", parse = TRUE) +
  xlim(-6,0) +
  ylim(-6,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
prps2d.Hum

## DAVIS
my_breaks_3 <- c(7.389056, 2.718282, 1)
prps.Dav   <- ggplot(list.table.oob.prps[[3]], aes(x,y))
prps2d.Dav <- prps.Dav + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_3),labels = round(my_breaks_3)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(P)[R] * italic(P)[B])))) +
  ylab(expression(paste(log[10](hat(italic(P)[R] * italic(P)[B])), " ","(OOB prediction)")))+
  ggtitle("Davis") +
  annotate("text", x = -1.0, y = -5.4, label = paste("MSE =", round(perf_table[3,2], 3))) + 
  annotate("text", x = -1.0, y = -5.9, label = "italic(R) ^ 2 == 0.349", parse = TRUE) +
  xlim(-6,0) +
  ylim(-6,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
prps2d.Dav

## STANISLAUS
my_breaks_4 <- c(7.389056, 2.718282, 1)
prps.Sta   <- ggplot(list.table.oob.prps[[4]], aes(x,y))
prps2d.Sta <- prps.Sta + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_4),labels = round(my_breaks_4)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(P)[R] * italic(P)[B])))) +
  ylab(expression(paste(log[10](hat(italic(P)[R] * italic(P)[B])), " ","(OOB prediction)")))+
  ggtitle("Stanislaus") +
  annotate("text", x = -1.0, y = -5.4, label = paste("MSE =", round(perf_table[4,2], 3))) + 
  annotate("text", x = -1.0, y = -5.9, label = "italic(R) ^ 2 == 0.345", parse = TRUE) +
  xlim(-6,0) +
  ylim(-6,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
prps2d.Sta

## STEBBINS
my_breaks_5 <- c(7.389056, 2.718282, 1)
prps.Ste   <- ggplot(list.table.oob.prps[[5]], aes(x,y))
prps2d.Ste <- prps.Ste + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_5),labels = round(my_breaks_5)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(P)[R] * italic(P)[B])))) +
  ylab(expression(paste(log[10](hat(italic(P)[R] * italic(P)[B])), " ","(OOB prediction)")))+
  ggtitle("Stebbins") +
  annotate("text", x = -1.0, y = -5.4, label = paste("MSE =", round(perf_table[5,2], 3))) + 
  annotate("text", x = -1.0, y = -5.9, label = "italic(R) ^ 2 == 0.321", parse = TRUE) +
  xlim(-6,0) +
  ylim(-6,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
prps2d.Ste

## RIVERSIDE
my_breaks_7 <- c(7.389056, 2.718282, 1)
prps.Riv   <- ggplot(list.table.oob.prps[[6]], aes(x,y))
prps2d.Riv <- prps.Riv + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_7),labels = round(my_breaks_7)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(P)[R] * italic(P)[B])))) +
  ylab(expression(paste(log[10](hat(italic(P)[R] * italic(P)[B])), " ","(OOB prediction)")))+
  ggtitle("Riverside") +
  annotate("text", x = -1.0, y = -5.4, label = paste("MSE =", round(perf_table[6,2], 3))) + 
  annotate("text", x = -1.0, y = -5.9, label = "italic(R) ^ 2 == 0.288", parse = TRUE) +
  xlim(-6,0) +
  ylim(-6,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
prps2d.Riv

## PLACERITA
my_breaks_8 <- c(7.389056, 2.718282, 1)
prps.Pla   <- ggplot(list.table.oob.prps[[7]], aes(x,y))
prps2d.Pla <- prps.Pla + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_8),labels = round(my_breaks_8)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(P)[R] * italic(P)[B])))) +
  ylab(expression(paste(log[10](hat(italic(P)[R] * italic(P)[B])), " ","(OOB prediction)")))+
  ggtitle("Placerita") +
  annotate("text", x = -1.0, y = -5.4, label = paste("MSE =", round(perf_table[7,2], 3))) + 
  annotate("text", x = -1.0, y = -5.9, label = "italic(R) ^ 2 == 0.317", parse = TRUE) +
  xlim(-6,0) +
  ylim(-6,0) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
prps2d.Pla

# Combined plot
par(mfrow=c(3, 1))
combined_prps<-ggarrange(prps2d.Ava, prps2d.Hum,
                         prps2d.Dav, prps2d.Sta,
                         prps2d.Ste, prps2d.Riv,
                         prps2d.Pla,
                         nrow = 4,
                         ncol = 2,
                         labels = NULL)

#combined_prps

# Save plot
ggsave("results/pipeline_v6_bees/random_forests/oob_plots/combined_plot_prps.pdf",
       combined_prps,
       device="pdf",
       width=9,
       height=12)
