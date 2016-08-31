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
# Purpose: Analyse size of crown-of-thorns starfish used during experiments at
#          AIMS.
#
# Args:
#   Input: "COTS data AIMSxx.csv", in fun directory
#
# Returns:
#   Plots 

################################################################################
# source() and library() statements

# Clear workspace first
rm(list=ls())

# Attach packages:
library(dplyr)
library(ggplot2)

# Source circular plot function
source("MyCircularPlot05.R")

################################################################################
# Function definitions

################################################################################
# Executed statements

current.dir <- getwd()

#-------------------------------------------------------------------------------
# Read data

# Name of CSV file with data
file.name <- "COTS data AIMS04.csv"

# Navigate to parent folder and read data file
data <- read.csv(file.path("..", "Data", file.name), sep=";", dec=",")

# Save original data
original.data <- data

# Drop unnecessary collumns
data <- select(data, type, batch, animal.diameter, no.arms, used.before)
# Drop rows without diameter measurements
data <- filter(data, animal.diameter>0)

# Data for plotting "all sizes" and "batch" groups cannot have animals that
# have been used before.
data2 <- filter(data, used.before == "no")

#-------------------------------------------------------------------------------
# Plot data

# All animals
all.sizes <- ggplot(data2, aes("All experiments", animal.diameter)) +
  #   geom_violin() + 
  geom_dotplot(binaxis="y", binwidth = 2.5) +
  ggtitle("crown-of-thorns starfish size") +
  xlab("") +
  ylab("Animal diameter (cm)") +
  theme_bw()
#print(all.sizes)

# Sizes grouped per experiment
experiments <- ggplot(data, aes(type, animal.diameter)) +
  geom_dotplot(binaxis="y", binwidth = 2.5) +
  ggtitle("crown-of-thorns starfish size\nGrouped per experiment") +
  xlab("Experiment") +
  ylab("Animal diameter (cm)") +
  theme_bw()
#print(experiments)

# Grouped per batch
batch <- ggplot(data2, aes(batch, animal.diameter)) +
  #   geom_violin() + 
  geom_dotplot(binaxis="y", binwidth = 2.5) +
  ggtitle("crown-of-thorns starfish size\nGrouped per batch") +
  xlab("Batch") +
  ylab("Animal diameter (cm)") +
  theme_bw()
#print(batch)

#-------------------------------------------------------------------------------
# Save plots

setwd(file.path("..", "Figs"))
dir.create("Size", showWarnings = F)
setwd("Size")

png("All animals.png", width = 800, height = 400)
print(all.sizes)
dev.off()

png("Grouped per experiment.png", width = 800, height = 400)
print(experiments)
dev.off()

png("Grouped per batch.png", width = 800, height = 400)
print(batch)
dev.off()

# Save in RData file
save(all.sizes, experiments, batch, file="Size.RData")

#-------------------------------------------------------------------------------
# Plot directional responses dependent on animal size.

dir.create("Size VS directionality", showWarnings=F)
setwd("Size VS directionality")
dir.data <- original.data
dir.data <- select(dir.data, type, animal.id, initial.heading, final.heading,
                   animal.diameter)

# Fill out empty "animal.diameter" fields
# Unique animal IDs
dir.data$animal.id <- factor(dir.data$animal.id)
id <- levels(dir.data$animal.id)
# Iterate through data
for (i in 1:length(id)){
  dir.data$animal.diameter[dir.data$animal.id==id[i]] <- 
    dir.data$animal.diameter[dir.data$animal.id==id[i]][1]
}

max.diameter <- max(dir.data$animal.diameter, na.rm=T)
# Size of interval in cm
step <- 10
intervals <- ceiling(max.diameter/step)

for (i in 1:intervals){
  # Filter data
  min <- (i-1)*step
  max <- i*step
  curr.data <- filter(dir.data, animal.diameter >= min,
                      animal.diameter < max)
  
  # Plot final headings
  plot.name <- paste("Size ", toString(min), "-",
                     toString(max), " final",sep="")
  final <- MyCircularPlot(curr.data$final.heading, plot.name)
  
  # Plot initial headings
  plot.name <- paste("Size ", toString(min), "-",
                     toString(max), " initial",sep="")
  initial <- MyCircularPlot(curr.data$initial.heading, plot.name)
  
  # Create data frame with final headings for later analysis
  if (i == 1) {
    heading.vs.size <- data.frame(
      size.group = rep(paste(toString(min), "-", toString(max), sep=""),
                       length(curr.data$final.heading)),
      final.heading = curr.data$final.heading)
  } else {
    temp <- data.frame(
      size.group = rep(paste(toString(min), "-", toString(max), sep=""),
                       length(curr.data$final.heading)),
      final.heading = curr.data$final.heading)
    
    heading.vs.size <- rbind(heading.vs.size, temp)
  }
}

#------------------------------------------------------------------------------
# Plot distribution of headings per experiment

setwd("..")

# Plot distribution of headings per size class
png("Headings_per_size_group.png", width = 800, height = 600)
plot(final.heading~size.group, data = heading.vs.size)
dev.off()


setwd(current.dir)

################################################################################
# Save stats and data

setwd(file.path("..", "Output"))
dir.create("Sizes", showWarnings = F)
setwd("Sizes")

# ANOVA
aov.animal.diameter <- aov(animal.diameter ~ type, data=data)

# Kruskall Wallis test
kw.animal.diameter <- with(data, kruskal.test(animal.diameter, type))

# Save
sink("ANOVA and Kruskall-Wallis.txt")
print("ANOVA")
print(summary(aov.animal.diameter))
print("")
print("Kruskall-Wallis")
print(kw.animal.diameter)
sink()

# Table headings VS size
write.table(heading.vs.size, file = "heading_VS_size.csv", sep=";", dec=",",
            row.names = F)

circular.anova <- aov.circular(
  as.circular(heading.vs.size$final.heading,
              type="angles", units="degrees",
              rotation="clock",
              zero=pi/2, 
              template="none",
              modulo="asis"),
  heading.vs.size$size.group, method = "F.test")

# Circular anova on headings VS size
sink("Circular ANOVA headings VS size.txt")
print(circular.anova)
sink()

# Save important Data frames and tests as RData file
save(aov.animal.diameter, heading.vs.size, circular.anova, dir.data,
     aov.animal.diameter, file = "Size_data.RData")

setwd(current.dir)
