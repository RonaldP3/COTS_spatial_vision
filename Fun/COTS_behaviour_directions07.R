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
# Purpose: Plot circular stats for crown-of-thorns starfish behaviour.
#
# Args:
#   Input: CSV file, "Data-Black circles white backgroundxx.csv" in parent dir.
#
# Returns:
#   SVG vector graphics
#
# Requires:
# - "MyCircularPlotxx.R"

################################################################################
# source() and library() statements

# Clear workspace first
rm(list=ls())

# Attach packages:
# Script for plotting circular data as SVG
source("MyCircularPlot05.R")

# Data management
library(dplyr)
# Circular stats
library(circular)

################################################################################
# Function definitions

################################################################################
# Executed statements

# Save current.directory first
current.dir <- getwd()

#-------------------------------------------------------------------------------
# User input
# Filter data on order?
filter <- "yes"
# Exclude experiments with order larger than:
max.order <- 3

#-------------------------------------------------------------------------------
# Read data
headings <- read.csv(file.path("..","Data", "COTS data AIMS04.csv"),
                     sep=";", dec=",")

# Save original data
headings.original <- headings

#-------------------------------------------------------------------------------
# Create folders and iterate through data
setwd(file.path("..", "Figs"))

# Create folder for data
dir.create("Directional plots-AIMS", showWarnings = F)
setwd("Directional plots-AIMS")
# Filter data
headings <- filter(headings, order <= max.order)

# Make one folder for each experiment types
types <- levels(headings$type)

# Collumns 2-n.cols will be used as independent variable
n.cols <- 4

# Create progress bar
pb <- txtProgressBar(min = 0, max = length(types)*n.cols , style = 3)

for(t in 1:length(types)) {
  # Create folders to store experiments
  dir.create(types[t], showWarnings = F)
  # Navigate to folder
  setwd(types[t])
  
  col.names <- colnames(headings)[2:(1+n.cols)]
  
  # Create polar plots. Use each collumn as independent variable
  for (i in 1:n.cols){
    #---------------------------------------------------------------------------
    # Reorganise data
    # Create data frame for this collumn
    current.data <- data.frame(independent = factor(headings[,i+1]),
                               initial.heading = headings$initial.heading,
                               final.heading = headings$final.heading,
                               type = headings$type)
    
    # Remove NA's
    current.data <- filter(current.data, is.na(final.heading) == F)
    # Select only current experiment
    current.data <- filter(current.data, type == types[t])
    
    # Create folders to store data
    dir.create(col.names[i], showWarnings = F)
    # Navigate to folder
    setwd(col.names[i])
    
    # Plot circular graph for each level of the independent variable
    levels <- levels(droplevels(current.data$independent))
    for (a in 1:length(levels)) {
      #-------------------------------------------------------------------------
      # Filter and plot data
      
      plot.data <- filter(current.data, independent == levels[a])
      
      # Plot initial walking direction
      p.init <- MyCircularPlot(plot.data$initial.heading,
                               paste(col.names[i], levels[a],"initial"))
      
      # Plot final walking direction
      p.fin <- MyCircularPlot(plot.data$final.heading,
                              paste(col.names[i], levels[a], "final"))
      
      #-------------------------------------------------------------------------
      # Organise data for summary
      info <- matrix(rep(c(types[t], col.names[i], levels[a]), 2),
                     nrow = 2, byrow = T)
      info <- cbind(info, matrix(c("initial", "final"), ncol = 1))
      colnames(info) <- c("type", "independent", "level", "timing")
      # Combine initial and final directions
      dir.stats <- rbind(p.init, p.fin)
      # Add info
      dir.stats <- cbind(info, dir.stats)
      
      if (t == 1 & i == 1 & a == 1) {
        # For first iteration, initiate result dfr
        results <- dir.stats
      } else {
        # else, append
        results <- rbind(results, dir.stats)
      }
    }
    # Update progress bar
    setTxtProgressBar(pb, ((t-1)*n.cols + i))
    
    # Navigate back
    setwd("..") 
  }
  # Navigate back
  setwd("..")
}

#-------------------------------------------------------------------------------
# Save diagnostics for each experiment
setwd(file.path("..", "..", "Output"))
dir.create("Directional data-AIMS", showWarnings = F)
setwd("Directional data-AIMS")
for(t in 1:length(types)) {
  # Filter data
  table.data <- filter(headings, type == types[t])
  
  # Drop unused levels
  table.data$stimulus.type <- droplevels(table.data$stimulus.type)
  
  # Save diagnostics
  sink(paste(types[t],"-diagnostics.txt", sep=""))
  print("Order:")
  print(with(table.data, table(order, useNA="no")))
  print(with(table.data, table(order, stimulus.direction, useNA="no")))
  print("")
  print("Stimulus type:")
  print(with(table.data, table(stimulus.type, useNA="no")))
  print(with(table.data, table(stimulus.type, stimulus.direction, useNA="no")))
  print("")
  print("Animal ID:")
  print(with(table.data, table(animal.id, useNA="no")))
  print(with(table.data, table(animal.id, stimulus.direction, useNA="no")))
  print("")
  print("Stimulus direction:")
  print(with(table.data, table(stimulus.direction, useNA="no")))
  sink()
}

#-------------------------------------------------------------------------------
# Save overall results
# Indicate significance
results$significant <- "no"
results$significant[results$Rayleigh <= 0.05] <- "yes"
# save results
write.table(results, "Directional Analysis results.csv", sep = ";", dec = ",",
            row.names = F)

# Navigate back

setwd(file.path("..", "..", "Fun"))

