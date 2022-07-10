## FST-Ne2

#### Clean-up the R environment
rm(list=ls())
ls()

#### Load librarires and source file
library(ggplot2)
library(cowplot)
library(ggpubr)
#library(grid) # for legends

source("src/Analyses/BEES/fun.R")

##### Load the list with data sets containing true vs FST-NE estimated NEs
load(file=paste0("~/My_repositories/Tracking-selection/results/pipeline_v6_bees/random_forests/list_table/list_table_fstNe2",".RData"))

# FST-NE simulated vs estimated MSE, Rsquared in a table (for a supplementary table)
perf_table <- data.frame(pop = c("Ava", "Hum", "Dav", "Sta", "Ste", "Riv", "Pla"),
                         mse = NA,
                         rsq = NA,
                         bias = NA)

# MSE
for (i in seq(perf_table$pop))
{
    perf_table[i,2] <- mse.fun(list.table.fstNe2[[i]])
}

# Rsquared
for (i in seq(perf_table$pop))
{
  perf_table[i,3] <- rsquared.fun(list.table.fstNe2[[i]])
}

# Bias
for (i in seq(perf_table$pop))
{
  perf_table[i,4] <- bias.fun(list.table.fstNe2[[i]])
}

print(perf_table)

# Export FST-NE performance table
#write.csv(perf_table,
#          file = "results/pipeline_v6_bees/random_forests/perf_table/performance_fstNE2.txt", row.names = F, quote = F)

## AVALON POPULATION
my_breaks_1 <- c(54.598150, 7.389056, 1)
fstne.Ava   <- ggplot(list.table.fstNe2[[1]], aes(x,y))
fstne2d.Ava <- fstne.Ava + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_1),labels = round(my_breaks_1)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(from", " ", italic(hat(F))[ST], ")"))) +
  xlim(-0.5,4) +
  ylim(-0.5,4) +
  ggtitle("Avalon") +
  annotate("text", x = 3.5, y = 0, label = paste("MSE =", round(perf_table[1,2], 3))) + 
  annotate("text", x = 3.5, y = -0.4, label = "italic(R) ^ 2 == 0.415", parse = TRUE) + 
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12),
                     )
fstne2d.Ava


## HUMBOLDT POPULATION
my_breaks_2 <- c(54.598150, 7.389056, 1)
fstne.Hum   <- ggplot(list.table.fstNe2[[2]], aes(x,y))
fstne2d.Hum <- fstne.Hum + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_2),labels = round(my_breaks_2)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(from", " ", italic(hat(F))[ST], ")"))) +
  xlim(-0.5,4) +
  ylim(-0.5,4) +
  ggtitle("Humboldt") +
  annotate("text", x = 3.5, y = 0, label = paste("MSE =", round(perf_table[2,2], 3))) + 
  annotate("text", x = 3.5, y = -0.4, label = "italic(R) ^ 2 == 0.700", parse = TRUE) + 
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
fstne2d.Hum

## DAVIS
my_breaks_3 <- c(20.085537, 7.389056, 2.718282, 1)
fstne.Dav   <- ggplot(list.table.fstNe2[[3]], aes(x,y))
fstne2d.Dav <- fstne.Dav + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_3),labels = round(my_breaks_3)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(from", " ", italic(hat(F))[ST], ")"))) +
  xlim(-0.5,4) +
  ylim(-0.5,4) +
  ggtitle("Davis") +
  annotate("text", x = 3.5, y = 0, label = paste("MSE =", round(perf_table[3,2], 3))) + 
  annotate("text", x = 3.5, y = -0.4, label = "italic(R) ^ 2 == 0.432", parse = TRUE) + 
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
fstne2d.Dav

## STANISLAUS
my_breaks_4 <- c(20.085537, 7.389056, 2.718282, 1)
fstne.Sta   <- ggplot(list.table.fstNe2[[4]], aes(x,y))
fstne2d.Sta <- fstne.Sta + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_4),labels = round(my_breaks_4)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(from", " ", italic(hat(F))[ST], ")"))) +
  xlim(-0.5,4) +
  ylim(-0.5,4) +
  ggtitle("Stanislaus") +
  annotate("text", x = 3.5, y = 0, label = paste("MSE =", round(perf_table[4,2], 3))) + 
  annotate("text", x = 3.5, y = -0.4, label = "italic(R) ^ 2 == 0.422", parse = TRUE) + 
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
fstne2d.Sta

## STEBBINS
my_breaks_5 <- c(20.085537, 7.389056, 2.718282, 1)
fstne.Ste   <- ggplot(list.table.fstNe2[[5]], aes(x,y))
fstne2d.Ste <- fstne.Ste + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_5),labels = round(my_breaks_5)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(from", " ", italic(hat(F))[ST], ")"))) +
  xlim(-0.5,4) +
  ylim(-0.5,4) +
  ggtitle("Stebbins") +
  annotate("text", x = 3.5, y = 0, label = paste("MSE =", round(perf_table[5,2], 3))) + 
  annotate("text", x = 3.5, y = -0.4, label = "italic(R) ^ 2 == 0.647", parse = TRUE) + 
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
fstne2d.Ste

## RIVERSIDE
my_breaks_6 <- c(20.085537, 7.389056, 2.718282, 1)
fstne.Riv   <- ggplot(list.table.fstNe2[[6]], aes(x,y))
fstne2d.Riv <- fstne.Riv + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_6),labels = round(my_breaks_6)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(from", " ", italic(hat(F))[ST], ")"))) +
  xlim(-0.5,4) +
  ylim(-0.5,4) +
  ggtitle("Riverside") +
  annotate("text", x = 3.5, y = 0, label = paste("MSE =", round(perf_table[6,2], 3))) + 
  annotate("text", x = 3.5, y = -0.4, label = "italic(R) ^ 2 == 0.402", parse = TRUE) + 
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
fstne2d.Riv

## PLACERITA
my_breaks_7 <- c(20.085537, 7.389056, 2.718282, 1)
fstne.Pla   <- ggplot(list.table.fstNe2[[7]], aes(x,y))
fstne2d.Pla <- fstne.Pla + 
  stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
  scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                       ,
                       breaks = round(my_breaks_7),labels = round(my_breaks_7)
  ) +
  xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
  ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(from", " ", italic(hat(F))[ST], ")"))) +
  xlim(-0.5,4) +
  ylim(-0.5,4) +
  ggtitle("Placerita") +
  annotate("text", x = 3.5, y = 0, label = paste("MSE =", round(perf_table[7,2], 3))) + 
  annotate("text", x = 3.5, y = -0.4, label = "italic(R) ^ 2 == 0.610", parse = TRUE) + 
  geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
  theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "right", axis.text = element_text(size = 12),
                     axis.title=element_text(size=12))
fstne2d.Pla

# Combined plot
par(mfrow=c(3, 1))
combined_fstne<-ggarrange(fstne2d.Ava, fstne2d.Hum,
                          fstne2d.Dav, fstne2d.Sta,
                          fstne2d.Ste, fstne2d.Riv,
                          fstne2d.Pla,
                          nrow = 4,
                          ncol = 2,
                          labels = NULL)

#combined_fstne

# Save plot
ggsave("results/pipeline_v6_bees/random_forests/oob_plots/combined_plot_fstNE.pdf",
       combined_fstne,
       device="pdf",
       width=9,
       height=12)