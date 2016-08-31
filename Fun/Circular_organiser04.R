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
# Purpose: Organise plots such that circular plots are on one row
#
# Args:
#   Input: Levels: grouping variable, in desired order
#          Graph.data: Headings
#          Analysis: type of analysis. "Angles" or "Axial"
#
# Returns:
#   Plots with initial headings on top row and final headings on bottom row.

################################################################################
# source() and library() statements

# Attach packages:

################################################################################
# Function definitions

################################################################################
# Executed statements

CircularOrganiser <- function (levels, graph.data, analysis, plot=TRUE, 
                               select="stimulus") {
  
  # Analysis method (angular, axial) should be given for each stimulus (level)
  if (length(levels) != length(analysis)) {
    stop("Specify analysis method for all data levels!")
  }
  
  # Graphical parameters
  par(mfrow=c(1,length(levels)))
  par(mar = c(0,1,0,1), oma = c(1,1,1,1))
  
  # Iterate through data
  for (n in 1:length(levels)) {
    # data for current plot
    if (select=="stimulus") {
      current.plot <- filter(graph.data, stimulus.name==levels[n])
    } else {
      if (select=="order") {
        current.plot <- filter(graph.data, order==levels[n])
      } else {
        stop("Use order or stimulus only!")
      }
    }
  
  # Plots are put on first row (by mfg)
  par(mfg=c(1,n))
  stats <- JustCircles(current.plot$final.heading, analysis[n], letters[n],
                       levels[n], plot)
  stats <- cbind(stats, data.frame(timing = "Final", level = levels[n]))
  
  if (n == 1){
    output <- stats
  } else {
    output <- rbind(output, stats)
  }
}
return(output)
}
