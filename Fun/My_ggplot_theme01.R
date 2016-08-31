################################################################################
# Copyright statement
# This script, or parts of it, can be used or reproduced under the terms of the
# Creative Commons Attribution License which permits any use, distribution, and 
# reproduction in any medium, provided the original author(s) and the source are
# credited.

################################################################################
# Author: Ronald Petie

################################################################################
# source() and library() statements

require(ggplot2)

################################################################################
# Function definitions

################################################################################
# Executed statements

# Create custom ggplot theme
my.theme <- theme(panel.grid.major = element_line(size = 0.3, color = "grey"),
                  panel.grid.minor = element_blank(),
                  plot.background = element_rect(fill="transparent"),
                  panel.background = element_rect(fill="transparent"),
                  axis.line = element_line(size = 0.6, color = "black"),
                  axis.ticks = element_line(size = 0.6, color = "black"),
                  text = element_text(size = 9, colour="black"),
                  axis.text = element_text(size = 9, colour="black"),
                  panel.border = element_blank(),
                  # Omit titles for publication style plots
                  plot.title = element_blank(),
                  # Plot background red for visualisation of plot areas
                  #plot.background=element_rect(fill="red"),
                  plot.margin = unit(rep(1, 4), "mm"),
                  # Format legend
                  legend.title=element_blank(),
                  legend.key = element_rect(colour="transparent"),
                  legend.key.size = unit(0.06, "npc"))



