## PrPs
oob_plot_prps <- function(indir, par)
{
  #library(ggplot2)
  #library(cowplot)
  
  load(file=paste0(inDir, "/", par, "/","posterior_table_", par, ".RData"))
  
  ## Expected vs Predicted plot
  my_breaks <- c(2.718282,2, 1)
  p   <- ggplot(posterior_table, aes(x,y))
  p2d <- p + 
         stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
         scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                              ,breaks = round(my_breaks),labels = round(my_breaks),
         ) +
         xlab(expression(paste("true", " ", log[10](italic(P)[R] * italic(P)[B])))) +
         ylab(expression(paste("predicted", " ", log[10](hat(italic(P)[R] * italic(P)[B])))))+
         annotate("text", x = -0.7, y = -5.1, label = paste("MSE =", round(1.49873130087053, 3))) + 
         annotate("text", x = -0.7, y = -5.7, label = "italic(R) ^ 2 == 0.348", parse = TRUE) +
         xlim(-6,0) +
         ylim(-6,0) +
         geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
         theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.position = "right", axis.text = element_text(size = 12),
                            axis.title=element_text(size=12))
  
  rm(my_breaks)
  rm(p)
  rm(posterior_table)
  return(p2d)
}

## DFE-gamma-mean
oob_plot_gamma <- function(indir, par)
{
  #library(ggplot2)
  #library(cowplot)
  
  load(file=paste0(inDir, "/", par, "/","posterior_table_", par, ".RData"))
  
  ## Expected vs Predicted plot
  my_breaks <- c(2.718282,2, 1)
  p   <- ggplot(posterior_table, aes(x,y))
  p2d <- p + 
         stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
         scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                              ,breaks = round(my_breaks),labels = round(my_breaks),
         ) +
         xlab(expression(paste("true", " ", log[10](italic(gamma))))) +
         ylab(expression(paste("predicted", " ", log[10](italic(hat(gamma))))))+
         annotate("text", x = -0.4, y = -2.5, label = paste("MSE =", round(0.688527535488464, 3))) + 
         annotate("text", x = -0.4, y = -2.9, label = "italic(R) ^ 2 == 0.104", parse = TRUE) +
         xlim(-3,0) +
         ylim(-3,0) +
         geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
         theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.position = "right", axis.text = element_text(size = 12),
                            axis.title=element_text(size=12))
  
  rm(my_breaks)
  rm(p)
  rm(posterior_table)
  return(p2d)
}

## Theta b
oob_plot_thetab <- function(indir, par)
{
  #library(ggplot2)
  #library(cowplot)
  
  load(file=paste0(inDir, "/", par, "/","posterior_table_", par, ".RData"))
  
  ## Expected vs Predicted plot
  my_breaks <- c(5.436564,2.718282,2, 1)
  p   <- ggplot(posterior_table, aes(x,y))
  p2d <- p + 
         stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
         scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                              ,breaks = round(my_breaks),labels = round(my_breaks)
         ) +
         xlab(expression(paste("true", " ", log[10](italic(theta)[b])))) +
         ylab(expression(paste("predicted", " ",log[10](italic(hat(theta))[b]))))+
         annotate("text", x = 3.5, y = -5.9, label = paste("MSE =", round(1.56719841608891, 3))) + 
         annotate("text", x = 3.5, y = -7.6, label = "italic(R) ^ 2 == 0.638", parse = TRUE) +
         xlim(-8,5) +
         ylim(-8,5) +
         geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
         theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.position = "right", axis.text = element_text(size = 12),
                            axis.title=element_text(size=12))
  
  rm(my_breaks)
  rm(p)
  rm(posterior_table)
  return(p2d)
}

## mu
oob_plot_mu <- function(indir, par)
{
  #library(ggplot2)
  #library(cowplot)
  
  load(file=paste0(inDir, "/", par, "/","posterior_table_", par, ".RData"))
  
  ## Expected vs Predicted plot
  my_breaks <- c(7.389056,2.718282, 1)
  p   <- ggplot(posterior_table, aes(x,y))
  p2d <- p + 
         stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
         scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                              ,breaks = round(my_breaks),labels = round(my_breaks)
         ) +
         xlab(expression(paste("true", " ", log[10](italic(mu))))) +
         ylab(expression(paste("predicted", " ",log[10](italic(hat(mu))))))+
         annotate("text", x = -7.1, y = -9.8, label = paste("MSE =", round(0.00829189633922191, 3))) + 
         annotate("text", x = -7.1, y = -10.3, label = "italic(R) ^ 2 == 0.994", parse = TRUE) +
         xlim(-10.5,-6.5) +
         ylim(-10.5,-6.5) +
         geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
         theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.position = "right", axis.text = element_text(size = 12),
                            axis.title=element_text(size=12))
  rm(my_breaks)
  rm(p)
  rm(posterior_table)
  return(p2d)
}

## rr
oob_plot_rr <- function(indir, par)
{
  #library(ggplot2)
  #library(cowplot)
  
  load(file=paste0(inDir, "/", par, "/","posterior_table_", par, ".RData"))
  
  ## Expected vs Predicted plot
  my_breaks <- c(2.718282,2, 1)
  p   <- ggplot(posterior_table, aes(x,y))
  p2d <- p + 
         stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
         scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                              ,breaks = round(my_breaks),labels = round(my_breaks)
         ) +
         xlab(expression(paste("true", " ", log[10](italic(r))))) +
         ylab(expression(paste("predicted", " ",log[10](hat(italic(r))))))+
         annotate("text", x = -6.8, y = -8.9, label = paste("MSE =", round(0.404917980580111, 3))) + 
         annotate("text", x = -6.8, y = -9.3, label = "italic(R) ^ 2 == 0.454", parse = TRUE) +
         xlim(-9.4,-6.4) +
         ylim(-9.4,-6.4) +
         geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
         theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.position = "right", axis.text = element_text(size = 12),
                            axis.title=element_text(size=12))
  
  rm(my_breaks)
  rm(p)
  rm(posterior_table)
  return(p2d)
}

## Ne
oob_plot_ne <- function(indir, par)
{
  #library(ggplot2)
  #library(cowplot)
  
  load(file=paste0(inDir, "/", par, "/","posterior_table_", par, ".RData"))
  
  ## Expected vs Predicted plot
  my_breaks <- c(54.598150, 7.389056, 1)
  p   <- ggplot(posterior_table, aes(x,y))
  p2d <- p + 
         stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
         scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                              ,breaks = round(my_breaks),labels = round(my_breaks)
         ) +
         xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
         ylab(expression(paste("predicted", " ",log[10](italic(hat(N))[e]))))+
         annotate("text", x = 3.3, y =  0.1, label = paste("MSE =", round(0.0266339174282766, 3))) + 
         annotate("text", x = 3.3, y = -0.5, label = "italic(R) ^ 2 == 0.968", parse = TRUE) +
         xlim(-0.7,3.8) +
         ylim(-0.7,3.8) +
         geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
         theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.position = "right", axis.text = element_text(size = 12),
                            axis.title=element_text(size=12))

  rm(my_breaks)
  rm(p)
  rm(posterior_table)
  return(p2d)
}

## Nc
oob_plot_nc <- function(indir, par)
{
  #library(ggplot2)
  #library(cowplot)
  
  load(file=paste0(inDir, "/", par, "/","posterior_table_", par, ".RData"))
  
  ## Expected vs Predicted plot
  my_breaks <- c(20.085537, 7.389056, 2.718282,1)
  p   <- ggplot(posterior_table, aes(x,y))
  p2d <- p + 
         stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
         scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                              ,breaks = round(my_breaks),labels = round(my_breaks)
         ) +
         xlab(expression(paste("true", " ", log[10](italic(N))))) +
         ylab(expression(paste("predicted", " ",log[10](italic(hat(N))))))+
         annotate("text", x = 3.3, y =  0.1, label = paste("MSE =", round(0.0364889783546244, 3))) + 
         annotate("text", x = 3.3, y = -0.5, label = "italic(R) ^ 2 == 0.961", parse = TRUE) +
         xlim(-0.7,3.8) +
         ylim(-0.7,3.8) +
         geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
         theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.position = "right", axis.text = element_text(size = 12),
                            axis.title=element_text(size=12))
  
  rm(my_breaks)
  rm(p)
  rm(posterior_table)
  return(p2d)
}

## Ne/Nc
oob_plot_nenc <- function(indir, par)
{
  #library(ggplot2)
  #library(cowplot)
  
  load(file=paste0(inDir, "/", par, "/","posterior_table_", par, ".RData"))
  
  ## Expected vs Predicted plot
  my_breaks <- c(54.598150, 7.389056, 1)
  p   <- ggplot(posterior_table, aes(x,y))
  p2d <- p + 
         stat_bin2d(bins=100,na.rm = T, show.legend=T) + 
         scale_fill_gradientn(colours=viridis::viridis(32), trans="log",name="counts"
                              ,breaks = round(my_breaks),labels = round(my_breaks)
         ) +
         xlab(expression(paste("true", " ", log[10](italic(N)[e]/italic(N))))) +
         ylab(expression(paste("predicted", " ",log[10](italic(hat(N))[e]/italic(N)))))+
         annotate("text", x = 0.05, y = -2.8, label = paste("MSE =", round(0.0297423278470574, 3))) + 
         annotate("text", x = 0.05, y = -3.3, label = "italic(R) ^ 2 == 0.752", parse = TRUE) +
         xlim(-3.5,0.5) +
         ylim(-3.5,0.5) +
         geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
         theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.position = "right", axis.text = element_text(size = 12),
                            axis.title=element_text(size=12))
  
  rm(my_breaks)
  rm(p)
  rm(posterior_table)
  return(p2d)
}

## pstrong
oob_plot_pstrong <- function(indir, par)
{
  #library(ggplot2)
  #library(cowplot)
  
  load(file=paste0(inDir, "/", par, "/","posterior_table_", par, ".RData"))
  
  ## Expected vs Predicted plot
  my_breaks <- c(7.389056, 2.718282, 1)
  p   <- ggplot(posterior_table, aes(x,y))
  p2d <- p + 
         stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
         scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                              ,breaks = round(my_breaks),labels = round(my_breaks)
         ) +
         xlab(expression(paste("true", " ", logit(italic(P))))) +
         ylab(expression(paste("predicted", " ", logit(italic(hat(P))))))+
         annotate("text", x = -0.4, y = -12, label = paste("MSE =", round(1.90015184160774, 3))) + 
         annotate("text", x = -0.4, y = -14, label = "italic(R) ^ 2 == 0.732", parse = TRUE) +
         xlim(-18,2) +
         ylim(-15,2) +
         geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
         theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.position = "right", axis.text = element_text(size = 12),
                            axis.title=element_text(size=12))

  rm(my_breaks)
  rm(p)
  rm(posterior_table)
  return(p2d)
}

## gamma_s
oob_plot_gammas <- function(indir, par)
{
  #library(ggplot2)
  #library(cowplot)
  
  load(file=paste0(inDir, "/", par, "/","posterior_table_", par, ".RData"))
  
  ## Expected vs Predicted plot
  my_breaks <- c(20.085537, 7.389056, 2.718282, 1)
  p   <- ggplot(posterior_table, aes(x,y))
  p2d <- p + 
         stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
         scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                              ,
                              breaks = round(my_breaks),labels = round(my_breaks),
         ) +
         xlab(expression(paste("true", " ", log[10](italic(bar(s)))))) +
         ylab(expression(paste("predicted", " ", log[10](hat(italic(bar(s)))))))+
         annotate("text", x = 0.35, y = -2.3, label = paste("MSE =", round(0.119654137148506, 3))) + 
         annotate("text", x = 0.35, y = -2.8, label = "italic(R) ^ 2 == 0.446", parse = TRUE) +
         xlim(-4.2,1) +
         ylim(-3,1) +
         geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
         theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.position = "right", axis.text = element_text(size = 12),
                            axis.title=element_text(size=12))
  
  rm(my_breaks)
  rm(p)
  rm(posterior_table)
  return(p2d)
}

## load
oob_plot_load <- function(indir, par)
{
  #library(ggplot2)
  #library(cowplot)
  
  load(file=paste0(inDir, "/", par, "/","posterior_table_", par, ".RData"))
  
  ## Expected vs Predicted plot
  my_breaks <- c(54.598150, 7.389056, 1)
  p   <- ggplot(posterior_table, aes(x,y))
  p2d <- p + 
         stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
         scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                              ,breaks = round(my_breaks),labels = round(my_breaks),
         ) +
         xlab(expression(paste("true", " ", logit(italic(L))))) +
         ylab(expression(paste("predicted", " ",logit(italic(hat(L))))))+
         annotate("text", x = 0.3, y = -16, label = paste("MSE =", round(2.67101894251427, 3))) + 
         annotate("text", x = 0.3, y = -19, label = "italic(R) ^ 2 == 0.555", parse = TRUE) +
         xlim(-38,7) +
         ylim(-20,7) +
         geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
         theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.position = "right", axis.text = element_text(size = 12),
                            axis.title=element_text(size=12))
  
  rm(my_breaks)
  rm(p)
  rm(posterior_table)
  return(p2d)
}

pred_plot_fstne <- function(indir, par)
{
  #library(ggplot2)
  #library(cowplot)
  
  load(file=paste0(inDir, "/", par, "/","pred_table_", par, ".RData"))
  
  ## Expected vs Predicted plot
  my_breaks <- c(7.389056, 2.718282, 1)
  p   <- ggplot(fstne_recomb_table, aes(x,y))
  p2d <- p + 
         stat_bin2d(bins=100, na.rm = T, show.legend=T) + 
         scale_fill_gradientn(colours=viridis::viridis(32), trans="log", name="counts"
                              ,breaks = round(my_breaks),labels = round(my_breaks)
         ) +
         xlab(expression(paste("true", " ", log[10](italic(N)[e])))) +
         ylab(expression(paste(log[10](italic(hat(N))[e]), " ","(from", " ", italic(hat(F))[ST], ")"))) +
         xlim(-0.5,3.5) +
         ylim(-0.5,3.5) +
         annotate("text", x = 3.05, y =  0.2, label = paste("MSE =", round(0.316692865221872, 3))) + 
         annotate("text", x = 3.05, y = -0.3, label = "italic(R) ^ 2 == 0.701", parse = TRUE) + 
         geom_abline(intercept = 0, slope = 1, color = "black",  linetype="dashed") +
         theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.position = "right", axis.text = element_text(size = 12),
                            axis.title=element_text(size=12))
  
  rm(my_breaks)
  rm(p)
  rm(fstne_recomb_table)
  return(p2d)
}

mse.fun <- function(data)
{
  x = data$x
  y = data$y
  
  # Root mean squared error
  mse <- mean((y - x)^2, na.rm = TRUE)
  
  return(mse)
} # end of function

rsquared.fun <- function(data)
{
  x = data$x
  y = data$y
  
  # x mean and sd
  mean_x <- mean(x, na.rm = T)
  sd_x <- sd(x, na.rm = T)
  
  # y mean and sd
  mean_y <- mean(y, na.rm = T)
  sd_y <- sd(y, na.rm = T)
  
  # R^2
  rsq <- (1/(length(x)-1) * (sum((x - mean_x)*(y - mean_y), na.rm = T)/(sd_x*sd_y)))^2
  
  return(rsq)
  
} # end of function

bias.fun <- function(data)
{
  x = data$x
  y = data$y
  
  # Mean bias
  bias <- mean(y - x, na.rm = TRUE)
  
  return(bias)
}

globalFSTNe <- function(x, tau){
  fst  <- x
  ne <- (tau*(1-fst))/(4*fst)
  return(ne)
}

