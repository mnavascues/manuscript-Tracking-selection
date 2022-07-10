makePCApredPlot <- function(pcx, pcy, predx, predy, pcx.n = 1, pcy.n = 2, alpha=0.75, name="Name")
{
  require(FactoMineR)
  require(factoextra)
  require(ggplot2)
  
  dt <- data.frame(pcx=c(pcx, predx), 
                   pcy=c(pcy, predy),
                   label=c(rep("Simulated",length(pcx)), "Observed"))
  
  p <- ggplot(data = dt, 
              aes(x = pcx, y = pcy)) +
    geom_point(aes(fill = factor(label),
                   colour = factor(label)), shape=21, size = 4) + 
    scale_fill_manual(values=c(alpha("#000000", alpha), alpha("#d9d9d9", alpha)), name=name) + 
    scale_colour_manual(values=c(alpha("#000000", alpha), alpha("#969696", alpha)), name=name) +
    xlab(paste("PC", pcx.n)) +
    ylab(paste("PC", pcy.n)) +
    geom_hline(yintercept = 0, color = "black",  linetype="dashed") +
    geom_vline(xintercept = 0,color = "black",  linetype="dashed") +
    theme_bw() + theme(panel.border = element_rect(colour = "black", fill=NA), 
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), 
                       axis.line = element_line(colour = "black"),
                       axis.text = element_text(size = 10),
                       axis.title=element_text(size=12),
                       legend.position = "right",
                       legend.text = element_text(size = 12),
                       legend.title = element_text(size = 14))
  
  return(p)
} 

modelCheckPCA <- function(simSumStats, obsSumStats, 
                          #pcs = c(1,2,3,4,5,6),
                          last.pc = 10, name="NAME", alpha=0.75)
{
  library(ggpubr)
  require(cowplot)
  
  # Check if the last PC is even, if odd add 1;
  if (last.pc %% 2 > 0)
  {
    last.pc = last.pc + 1
  } else {
    last.pc = last.pc
  }
  
  # PCA & Prediction
  PCA <- princomp(simSumStats)
  pred <- predict(PCA, obsSumStats)
  
  # For loop
  number_of_plots = (last.pc/2) # set the number of plots
  list_p <- vector(mode = "list", length = number_of_plots)
  for (i in seq(number_of_plots))
  {
    # Temporary objects PC vectors
    x = i+(i-1)
    y = 2*i
    print(paste("Plotting PCs pairs...","PC", x, "and", "PC", y))
    
    # Save plots as list elements
    list_p[[i]] <- makePCApredPlot(pcx=PCA$scores[,x], pcy=PCA$scores[,y], 
                                   predx=pred[,x], predy=pred[,y], 
                                   pcx.n = x, pcy.n = y, alpha=alpha, name=name)
    
    # clean-up for loop
    rm(x)
    rm(y)
  }# end of for loop
  
  # clean-up function
  rm(PCA)
  rm(pred)
  
  # Create ggarrange object
  n_rows=0; n_cols=0
  
  if (number_of_plots == 1)
  {
    n_cols=1
    n_rows=1
  } else if (number_of_plots %% 2 == 0){
    n_cols = 2
    n_rows = ceiling(number_of_plots/n_cols)
  } else {
    n_cols = 3
    n_rows = ceiling(number_of_plots/n_cols)
  }
  
  cp <- ggarrange(plotlist = list_p,
                  common.legend = T,
                  legend = "top",
                  font.label = list(size = 24, face = "bold"),
                  nrow=n_rows, 
                  ncol=n_cols)
  
  return(cp)
  
} #end function

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








