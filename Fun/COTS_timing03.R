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
#          AIMS
#
# Args:
#   Input: "COTS data AIMSxx.csv", in directory above this one
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

################################################################################
# Function definitions

################################################################################
# Executed statements

# Save current directory
current.dir <- getwd()

# Time interval used for image sequences
interval <- 5

#-------------------------------------------------------------------------------
# Read data

# Name of CSV file with data
file.name <- "COTS data AIMS04.csv"

# Navigate to parent folder and read data file
data <- read.csv(file.path("..", "Data", file.name), sep=";", dec=",")

# Save original data
original.data <- data

# Select collumns
data <- select(data, type, stimulus.type, animal.diameter, start.frame,
               stop.frame)

# Calculate timing in minutes
data$start.time <- data$start.frame * interval / 60
data$stop.time <- data$stop.frame * interval / 60
data$elapsed.time <- data$stop.time - data$start.time

# Create unique naming for experiments
data$combined.type <- with(data, paste(type, stimulus.type, sep = "-"))

################################################################################
# Create plots

###### 1) All experiments combined----------------------------------------------
start.all <- ggplot(data, aes(x="All experiments", y=start.time)) +
  #   geom_violin() + 
  geom_dotplot(binaxis = "y", binwidth = 0.5) +
  ggtitle("Time untill start walking\nAll experiments") +
  xlab("") +
  ylab("Time (min)") +
  theme_bw()
#print(start.all)

elapsed.all <- ggplot(data, aes(x="All experiments", y=elapsed.time)) +
  #   geom_violin() + 
  geom_dotplot(binaxis = "y", binwidth = 0.5) +
  ggtitle(
    "Time from onset of walking untill reaching target\nAll experiments") +
  xlab("") +
  ylab("Time (min)") +
  theme_bw()
#print(elapsed.all)

###### 2) Grouped per experiment------------------------------------------------
start.exp <- ggplot(data, aes(x=type, y=start.time)) +
  #   geom_violin() + 
  geom_dotplot(binaxis = "y", binwidth = 0.5) +
  ggtitle("Time untill start walking\nGrouped per experiment") +
  xlab("") +
  ylab("Time (min)") +
  theme_bw()
#print(start.exp)

# calculate mean
elapsed.mean <- summarise(group_by(data, type),
                          mean=mean(elapsed.time, na.rm=T),
                          sd=sd(elapsed.time, na.rm=T))
# Create plot
elapsed.exp <- ggplot(data, aes(x=type, y=elapsed.time)) +
  geom_pointrange(data = elapsed.mean, aes(x=type, y=mean,
                                           ymax=mean+sd,
                                           ymin=mean-sd,
                                           colour="red")) +
  geom_dotplot(binaxis = "y", binwidth = 0.5, dotsize=0.5) +
  ggtitle(
    "Time from onset of walking untill reaching target\nGrouped per experiment") +
  xlab("") +
  ylab("Time (min)") +
  theme_bw()
#print(elapsed.exp)

###### 3) Grouped per stimulus--------------------------------------------------
start.stim <- ggplot(data, aes(x=combined.type, y=start.time)) +
  #   geom_violin() + 
  geom_dotplot(binaxis = "y", binwidth = 0.5) +
  ggtitle("Time untill start walking\nGrouped per stimulus") +
  xlab("") +
  ylab("Time (min)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90))
#print(start.stim)

elapsed.stim <- ggplot(data, aes(x=combined.type, y=elapsed.time)) +
  #   geom_violin() + 
  geom_dotplot(binaxis = "y", binwidth = 0.5) +
  ggtitle(
    "Time from onset of walking untill reaching target\nGrouped per stimulus") +
  xlab("") +
  ylab("Time (min)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90))
#print(elapsed.stim)

###### 4) Elapsed time as a function of animal diameter-------------------------

# First remove outliers, t > 9 min
data2 <- data
#data2 <- filter(data2, elapsed.time <= 9)

# Then fit linear function
fit <- lm(elapsed.time ~ animal.diameter, data=data2)
coef <- coef(fit)

size <- ggplot(data2, aes(x=animal.diameter, y=elapsed.time)) +
  #   geom_abline(intercept = coef[1], slope = coef[2], colour = "red") +
  geom_smooth() +
  geom_point() +
  ggtitle("Time untill reaching target as a function of animal diameter") +
  xlab("Animal diameter (cm)") +
  ylab("Time (min)") +
  theme_bw()
#print(size)

###### 5) Time to onset time as a function of animal diameter-------------------

# Remove outliers
data3 <- data
#data3 <- filter(data3, start.time <= 20)

# Then fit linear function
fit2 <- lm(start.time ~ animal.diameter, data=data3)
coef2 <- coef(fit2)

start <- ggplot(data3, aes(x=animal.diameter, y=start.time)) +
  #   geom_abline(intercept = coef2[1], slope = coef2[2], colour = "red") +
  geom_smooth() +
  geom_point() +
  ggtitle("Time untill start walking as a function of animal diameter") +
  xlab("Animal diameter (cm)") +
  ylab("Time (min)") +
  theme_bw()
#print(start)

################################################################################
# Save plots

# Make dir
setwd(file.path("..", "Figs"))
dir.create("Timing", showWarnings = F)
setwd("Timing")

# Save figures
png("All-onset.png", width = 800, height = 400)
print(start.all)
dev.off()

png("All-elapsed.png", width = 800, height = 400)
print(elapsed.all)
dev.off()

png("Experiment-onset.png", width = 800, height = 400)
print(start.exp)
dev.off()

png("Experiment-elapsed.png", width = 800, height = 400)
print(elapsed.exp)
dev.off()

png("Stimulus-onset.png", width = 800, height = 400)
print(start.stim)
dev.off()

png("Stimulus-elapsed.png", width = 800, height = 400)
print(elapsed.stim)
dev.off()

png("Elapsed time VS diameter.png", width =800, height = 400)
print(size)
dev.off()

png("Start time VS diameter.png", width =800, height = 400)
print(start)
dev.off()

# Save RData file
save(start.all, elapsed.all, start.exp, elapsed.exp, start.stim,
     elapsed.stim, size, start, file="Timing.RData")

# Return to start directory
setwd(current.dir)

################################################################################
# Statistics

# Run anova
elapsed.aov <- aov(elapsed.time ~ type, data = data)
# Posthoc test
elapsed.posthoc <- TukeyHSD(elapsed.aov)

# Change directory
setwd(file.path("..","Output"))
dir.create("Timing", showWarnings = F)
setwd("Timing")

# and save tests
sink("Anova timing per experiment.txt")
print("Analysis of variance")
print(summary(elapsed.aov))
print("")
print("Post hoc test")
print(elapsed.posthoc)
sink()

# Return to base directory
setwd(current.dir)
