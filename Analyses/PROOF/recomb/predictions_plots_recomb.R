###########################
##  Script to OOB Plots  ##
###########################

rm(list=ls())
ls()

print("Preparing the environment for the analysis ...")
#setwd("./")
setwd("/Users/correapavinato.1/My_repositories/Tracking-selection/results/pipeline_v5/recomb_pods")

# Load package
library(ggplot2)
library(ggpubr)
library(cowplot)

# Load auxiliary functions
source("/Users/correapavinato.1/My_repositories/Tracking-selection/src/Analyses/PROOF/recomb/fun.R")

# Make path for the posterior tables
inDir = "posteriors"

# Make plots
print("Running PrPS ...")
par = "prps"
prps <- oob_plot_prps(indir = inDir, par = par)
rm(par)

print("Running gamma...")
par = "gamma"
gamma <- oob_plot_gamma(indir = inDir, par = par)
rm(par)

print("Running theta_b...")
par = "theta_b"
theta_b <- oob_plot_thetab(indir = inDir, par = par)
rm(par)

print("Running mu...")
par = "mu"
mu <- oob_plot_mu(indir = inDir, par = par)
rm(par)

print("Running rr...")
par = "rr"
rr <- oob_plot_rr(indir = inDir, par = par)
rm(par)

print("Running ne...")
par = "ne"
ne <- oob_plot_ne(indir = inDir, par = par)
rm(par)

print("Running nc...")
par = "nc"
nc <- oob_plot_nc(indir = inDir, par = par)
rm(par)

print("Running ne/nc...")
par = "ne_nc"
ne_nc <- oob_plot_nenc(indir = inDir, par = par)
rm(par)

print("Running pstrong...")
par = "pstrong"
pstrong <- oob_plot_pstrong(indir = inDir, par = par)
rm(par)

print("Running gamma_s...")
par = "gamma_s"
gamma_s <- oob_plot_gammas(indir = inDir, par = par)
rm(par)

print("Running load...")
par = "load"
load <- oob_plot_load(indir = inDir, par = par)
rm(par)


print("Running FST-NE...")
par="FST-NE"
fstne <- pred_plot_fstne(indir = inDir, par = par)
rm(par)


print("Exporting combined plots...")
# Make a final combined plot
posterior_plots <- cowplot::plot_grid(theta_b, nc, 
                                      ne, fstne,
                                      prps, pstrong,
                                      gamma, gamma_s,
                                      load, mu, 
                                      rr, ne_nc,
                                      nrow = 6,
                                      ncol = 2,
                                      labels=NULL
                                      #labels=c("a","b","c","d",
                                      #         "e","f","g","h",
                                      #         "i","j","k","l")
                                      #,
                                      #align = "hv"
                                      )

# Save final plot
ggsave(posterior_plots, filename = paste0(inDir, "/","posterior_plots.pdf"), device = cairo_pdf, 
       width = 9, height = 13, units = "in", dpi = "retina")
