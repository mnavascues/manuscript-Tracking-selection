###########################################
## Plot to exemplify the heterogenity in ##
## recombination rates across the genome ##
###########################################

rm(list=ls())
ls()

## DEFINE THE GENOMIC ARCHITECTURE
##----------------------------------

# Genome size
genomeS = 100e+6

# Genome fragments sizes
fragS   = 5e+4

# Number of chromosome(s)
chrN    = 1

## DEFINE THE SIMULATION PARAMETERS AS IN THE PIPELINE
##----------------------------------

# PROPORTION OF THE GENOME THAT CONTAINS BENEFICIAL MUTATIONS - G2 ELEMENTS
PrGWSel_random = TRUE
PrGWSel_value <- 0
PrGWSel_min = 0 
PrGWSel_max = 1

# PER BASE RECOMBINATION RATE
rr_random = TRUE
rr_rate <- 5 * 1e-7
rr_min  = 5 * 1e-10
rr_max  = 5 * 1e-7

## DEFINE THE PARAMETER SAMPLING AS IN THE PIPELINE
##----------------------------------

# PROPORTION OF THE GENOME THAT CONTAINS BENEFICIAL MUTATIONS - G2 ELEMENTS
if (PrGWSel_random){
  PrGWSel <- runif(n = 1, min = PrGWSel_min, max = PrGWSel_max)
} else {
  PrGWSel <-  PrGWSel_value
}

# PER BASE RECOMBINATION RATE
if (rr_random){
  rr <- 10^runif(1, min = log10(rr_min), max = log10(rr_max))
} else {
  rr <- rr_rate
}

## RANDOM VALUES DEFINING GENOMIC ELEMENTS  
##-------------------------------------------------------------------------------

# GENOME'S GENOME ELEMENT TYPE

# Set the number of the GenomicElementType
genomicElementN = as.integer((genomeS/fragS))

# Set the STARTS and the ENDS for the initializeGenomicElement
e_starts = NULL
e_ends   = NULL
for(i in seq(from = 0, to = genomicElementN-1)){
  starts    = i*fragS
  e_starts = c(e_starts, starts)
  
  ends = ((i+1)*fragS)-1
  e_ends = c(e_ends, ends)
}

# Sample the starts/ends pairs to assign to GenomicElementType G2
indexes = seq(from = 0, to = (genomicElementN-1))

g2_idx = sort(sample(indexes, as.integer(PrGWSel*genomicElementN), replace = FALSE))

# The difference are the GenomicElementType G1
g1_idx = setdiff(indexes, g2_idx)

## DEFINE RANDOM VALUES OF RECOMBINATION RATE FOR RECOMBINATION HOTSPOTS
## -- This take precedend over the rr_limits of one chromosome -- 
rr_hotspots <- 10^runif(length(e_ends), min = log10(rr)-.5, max = log10(rr)+.5)

## Update rr_rates with rr_hotspots values
rr_rates = rr_hotspots
rr_limits = e_ends

## Put the fragment ends in Mbp scale
rr_limits = rr_limits/1e6

plot(rr_rates~rr_limits, type="l")

hist(log10(rr_rates))

## Make a table
library(ggplot2)
library(ggpubr)

## Exponential scientific notation
#https://stackoverflow.com/questions/10762287/how-can-i-format-axis-labels-with-exponents-with-ggplot2-and-scales
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}


rr_plot_table <- data.frame(pos=c(rr_limits, rr_limits), 
                            rr = c(rep(rr, genomicElementN), rr_rates),
                            rr_type = c(rep("fixed_rr", genomicElementN), rep("variable_rr", genomicElementN)))


## MAKE AN EXAMPLE PLOT
p1 <- ggplot(rr_plot_table) + 
  geom_line(mapping = aes(x = pos, y = rr, col=rr_type), size=0.5) +
  #scale_x_continuous(label=scientific_10, trans="log") +
  scale_y_continuous(label=scientific_10, trans="identity") +
  scale_color_manual(values = c("#023047","#2a9d8f"),
                     name   = expression(paste("Simulation with ", italic(r))),
                     breaks = c("fixed_rr", "variable_rr"),
                     labels = c("Fixed", "Variable")) +
  geom_hline(yintercept = rr_plot_table$rr[1], color='#023047', size=2) +
  labs(x = "Positions in Mbp", y = expression(paste("Recombination rate (", italic(r), ")"))) + 
  theme_bw() + theme(panel.border = element_rect(colour = "black"), 
                     axis.line = element_line(colour = "black"),
                     axis.text = element_text(size = 14),
                     axis.title=element_text(size=20),
                     axis.text.x = element_text(angle = 0),
                     legend.position=NULL)

p1

## Save to a PDF
ggsave("recomb/example_variable_recomb.pdf",
       p1,
       device="pdf",
       width=7.1,
       height=6.7)
