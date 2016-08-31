#!/usr/bin/env Rscript
################################################################################
# Copyright statement
# This script, or parts of it, can be used or reproduced under the terms of the
# Creative Commons Attribution License which permits any use, distribution, and 
# reproduction in any medium, provided the original author(s) and the source are
# credited.

################################################################################
# Author: Ronald Petie

################################################################################
# Purpose: Plot area of black and white for a circular stimulus at decreasing
#          distance. Area's were measured on "Field_of_view-area01.png". Green 
#          shading represents black area's in the field of view of the eye, blue
#          shading represent white area's.
#
# Args:
#   Input: "B_W_circle_areaxx.csv"
#
# Returns:
#   SVG plot

################################################################################
# source() and library() statements

# Clear workspace first
rm(list=ls())

# Attach packages:
library(ggplot2)
library(grid)

# Load personalised theme
source("My_ggplot_theme01.R")

################################################################################
# Function definitions

################################################################################
# Load data

data <- read.table(file.path("..", "Data", "B_W_circle_area01.csv"),
                   sep=";", dec=",", header=T)

# Calculate ratio between white and black in visual field
data$ratio <- data$area.white / data$area.black

################################################################################
# Plot

bw.area <- ggplot(data, aes(x=distance, y=ratio)) +
  xlab("Distance to stimulus (cm)") +
  ylab("Area white/Area black") +
  geom_point() +
  geom_line() +
  my.theme

################################################################################
# Save plot

svg(file.path("..", "Figs", "Area_black_VS_white.svg"), width=3, 
    height=2)
print(bw.area)
dev.off()
