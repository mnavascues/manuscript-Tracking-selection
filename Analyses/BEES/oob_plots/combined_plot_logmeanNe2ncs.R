## logmeanNe2ncs

#### Clean-up the R environment
rm(list=ls())
ls()

#### Load librarires
library(ggplot2)
library(cowplot)
library(ggpubr)

load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_vector_logmeanNe2ncs",".RData"))
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_reg_logmeanNe2ncs",".RData"))

oob.logmeanNe2ncs.ava <- data.frame(x = list.logmeanNe2ncs$Ava,
                                    y = list.reg.logmeanNe2ncs$Ava$model.rf$predictions)

oob.logmeanNe2ncs.hum <- data.frame(x = list.logmeanNe2ncs$Hum,
                                    y = list.reg.logmeanNe2ncs$Hum$model.rf$predictions)

oob.logmeanNe2ncs.dav <- data.frame(x = list.logmeanNe2ncs$Dav,
                                    y = list.reg.logmeanNe2ncs$Dav$model.rf$predictions)

oob.logmeanNe2ncs.sta <- data.frame(x = list.logmeanNe2ncs$Sta,
                                    y = list.reg.logmeanNe2ncs$Sta$model.rf$predictions)

oob.logmeanNe2ncs.ste <- data.frame(x = list.logmeanNe2ncs$Ste,
                                    y = list.reg.logmeanNe2ncs$Ste$model.rf$predictions)

oob.logmeanNe2ncs.riv <- data.frame(x = list.logmeanNe2ncs$Riv,
                                    y = list.reg.logmeanNe2ncs$Riv$model.rf$predictions)

oob.logmeanNe2ncs.pla <- data.frame(x = list.logmeanNe2ncs$Pla,
                                    y = list.reg.logmeanNe2ncs$Pla$model.rf$predictions)

## List of oob table for plot
list.table.oob.nenc <- list(Ava=oob.logmeanNe2ncs.ava, Hum=oob.logmeanNe2ncs.hum, 
                            Dav=oob.logmeanNe2ncs.dav, Sta=oob.logmeanNe2ncs.sta, 
                            Ste=oob.logmeanNe2ncs.ste, Riv=oob.logmeanNe2ncs.riv,
                            Pla=oob.logmeanNe2ncs.pla)

#save(list.table.oob.nenc, 
#     file = paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_nenc",".RData"))
#load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table_oob_nenc",".RData"))

perf_table <- data.frame(pop = c("Ava", "Hum", "Dav", "Sta", "Ste", "Riv", "Pla"),
                         mse = NA,
                         rsq = NA)

# Non-neutral MSE and Rsquared
for (i in seq(perf_table$pop))
{
  # MSE
  perf_table[i,2] <- list.reg.logmeanNe2ncs[[i]]$model.rf$prediction.error
  
  # Rsquared
  perf_table[i,3] <- list.reg.logmeanNe2ncs[[i]]$model.rf$r.squared
}

# Clean-up memory
rm(list.logmeanNe2ncs)
rm(list.reg.logmeanNe2ncs)

# Export LOAD performance table
write.csv(perf_table,
          file = "results/pipeline_v6_bees/random_forests/performance_nenc.txt", row.names = F, quote = F)

## AVALON
my_breaks_1 <- c(403.428793, 54.598150, 7.389056, 1)
nencs.Ava   <- ggplot(list.table.oob.nenc[[1]], aes(x,y))
nencs2d.Ava <- nencs.Ava + 
  stat_bin2d(bins=100,na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e]/italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]/italic(N)), " ","(OOB prediction)")))+
  ggtitle("Avalon") +
  annotate("text", x = -0.4, y = -3.0, label = paste("MSE =", round(perf_table[1,2], 3))) + 
  annotate("text", x = -0.4, y = -3.3, label = "italic(R) ^ 2 == 0.723", parse = TRUE) +
  xlim(-3.5,0.5) +
  ylim(-3.5,0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
nencs2d.Ava

## HUMBOLDT POPULATION
my_breaks_2 <- c(2980.957987 ,403.428793, 54.598150, 7.389056, 1)
nencs.Hum   <- ggplot(list.table.oob.nenc[[2]], aes(x,y))
nencs2d.Hum <- nencs.Hum + 
  stat_bin2d(bins=100,na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e]/italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]/italic(N)), " ","(OOB prediction)")))+
  ggtitle("Humboldt") +
  annotate("text", x = -0.4, y = -3.0, label = paste("MSE =", round(perf_table[2,2], 3))) + 
  annotate("text", x = -0.4, y = -3.3, label = "italic(R) ^ 2 == 0.744", parse = TRUE) +
  xlim(-3.5,0.5) +
  ylim(-3.5,0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
nencs2d.Hum

## DAVIS POPULATION
nencs.Dav   <- ggplot(list.table.oob.nenc[[3]], aes(x,y))
nencs2d.Dav <- nencs.Dav + 
  stat_bin2d(bins=100,na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e]/italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]/italic(N)), " ","(OOB prediction)")))+
  ggtitle("Davis") +
  annotate("text", x = -0.4, y = -3.0, label = paste("MSE =", round(perf_table[3,2], 3))) + 
  annotate("text", x = -0.4, y = -3.3, label = "italic(R) ^ 2 == 0.708", parse = TRUE) +
  xlim(-3.5,0.5) +
  ylim(-3.5,0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
nencs2d.Dav

## STANISLAUS POPULATION
nencs.Sta   <- ggplot(list.table.oob.nenc[[4]], aes(x,y))
nencs2d.Sta <- nencs.Sta + 
  stat_bin2d(bins=100,na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e]/italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]/italic(N)), " ","(OOB prediction)")))+
  ggtitle("Stanislaus") +
  annotate("text", x = -0.4, y = -3.0, label = paste("MSE =", round(perf_table[4,2], 3))) + 
  annotate("text", x = -0.4, y = -3.3, label = "italic(R) ^ 2 == 0.722", parse = TRUE) +
  xlim(-3.5,0.5) +
  ylim(-3.5,0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
nencs2d.Sta

## STEBBINS POPULATION
nencs.Ste   <- ggplot(list.table.oob.nenc[[5]], aes(x,y))
nencs2d.Ste <- nencs.Ste + 
  stat_bin2d(bins=100,na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e]/italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]/italic(N)), " ","(OOB prediction)")))+
  ggtitle("Stebbins") +
  annotate("text", x = -0.4, y = -3.0, label = paste("MSE =", round(perf_table[5,2], 3))) + 
  annotate("text", x = -0.4, y = -3.3, label = "italic(R) ^ 2 == 0.740", parse = TRUE) +
  xlim(-3.5,0.5) +
  ylim(-3.5,0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
nencs2d.Ste

## RIVERSIDE POPULATION
nencs.Riv   <- ggplot(list.table.oob.nenc[[6]], aes(x,y))
nencs2d.Riv <- nencs.Riv + 
  stat_bin2d(bins=100,na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e]/italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]/italic(N)), " ","(OOB prediction)")))+
  ggtitle("Riverside") +
  annotate("text", x = -0.4, y = -3.0, label = paste("MSE =", round(perf_table[6,2], 3))) + 
  annotate("text", x = -0.4, y = -3.3, label = "italic(R) ^ 2 == 0.690", parse = TRUE) +
  xlim(-3.5,0.5) +
  ylim(-3.5,0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
nencs2d.Riv

## PLACERITA POPULATION
nencs.Pla   <- ggplot(list.table.oob.nenc[[7]], aes(x,y))
nencs2d.Pla <- nencs.Pla + 
  stat_bin2d(bins=100,na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e]/italic(N))))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]/italic(N)), " ","(OOB prediction)")))+
  ggtitle("Placerita") +
  annotate("text", x = -0.4, y = -3.0, label = paste("MSE =", round(perf_table[7,2], 3))) + 
  annotate("text", x = -0.4, y = -3.3, label = "italic(R) ^ 2 == 0.722", parse = TRUE) +
  xlim(-3.5,0.5) +
  ylim(-3.5,0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
nencs2d.Pla

# Combined plot
par(mfrow=c(3, 1))
combined_nenc<-ggarrange(nencs2d.Ava, nencs2d.Hum,
                         nencs2d.Dav, nencs2d.Sta,
                         nencs2d.Ste, nencs2d.Riv,
                         nencs2d.Pla,
                         nrow = 4,
                         ncol = 2,
                         labels = NULL)

#combined_nenc

# Save plot
ggsave("results/pipeline_v6_bees/random_forests/oob_plots/combined_plot_nenc.pdf",
       combined_nenc,
       device="pdf",
       width=9,
       height=12)
