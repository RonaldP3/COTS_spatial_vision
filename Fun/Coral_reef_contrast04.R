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
# Purpose: Plot relation between distance and coral reef contrast
#
# Args:
#   Input: Coral reef contrastxx.csv file
#
# Returns:
#   Plots and RData files

################################################################################
# source() and library() statements

# Clear workspace first
rm(list=ls())

# Attach packages:
library(ggplot2)
library(dplyr)
library(tidyr)

################################################################################
# Function definitions

################################################################################
# Executed statements

current.dir <- getwd()
#-------------------------------------------------------------------------------
# Read data
original <- read.csv(file.path("..", "Data", "Coral reef contrast02.csv"),
                     sep=";", dec=",")

# Select collumns to use
data <- select(original, date, description, sun, rep, distance, scene, mean)

# make separate collumns for foreground and background
data <- spread(data, scene, mean)

# Calculate Weber's contrast
data$contrast <- (data$background-data$foreground)/data$background

# Summarise data
final.contrasts <- summarise(group_by(data, description, sun, distance),
                             contrast = mean(contrast))

final.contrasts <- as.data.frame(final.contrasts)

mean.contrasts <- summarise(group_by(final.contrasts, distance),
                            N = length(contrast),
                            mean.contrast = mean(contrast),
                            SD = sd(contrast))
# append means
mean.contrasts2 <- data.frame(description = rep("mean", times =10),
                              sun = rep("NA", times = 10),
                              distance = mean.contrasts$distance,
                              contrast = mean.contrasts$mean.contrast)
final.contrasts <- rbind(final.contrasts, mean.contrasts2)

#-------------------------------------------------------------------------------
# Plot
contrast <- ggplot(final.contrasts, aes(x=distance, y=contrast, 
                                        col=description,
                                        linetype=description)) +
  xlab("distance (m)") +
  geom_line(aes(linetype = description)) +
  scale_colour_manual(values = c(rep("black", 5), "red")) +
  scale_linetype_manual(values = 1:6) +
  theme_bw()

#-------------------------------------------------------------------------------
# Save

# Change dir
setwd(file.path("..", "Figs"))
dir.create("Contrast", showWarnings = F)
setwd("Contrast")

# PNG image
png("Contrast.png", width = 800, height = 400)
print(contrast)
dev.off()

# RData file
save(contrast, final.contrasts, mean.contrasts, file="Contrast.RData")

# Navigate back
setwd(current.dir)

#-------------------------------------------------------------------------------
# CSV file
setwd(file.path("..", "Output"))
dir.create("Reef experiment", showWarnings = F)
setwd("Reef experiment")
dir.create("Contrast", showWarnings = F)
setwd("Contrast")

write.table(final.contrasts, file = "Reef_contrast.csv", sep = ";", dec = ",",
            row.names = F)
write.table(mean.contrasts, file = "Reef_contrast_mean.csv", sep = ";",
            dec = ",", row.names = F)

# Navigate back
setwd(current.dir)