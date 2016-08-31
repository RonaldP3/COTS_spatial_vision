#!/usr/bin/env Rscript
################################################################################
# Author: Ronald Petie

################################################################################
# Purpose:
# 
# Calculate arm count statistics

################################################################################
# source() and library() statements

# Clear workspace first
rm(list=ls())

# Attach packages:
library(dplyr)

################################################################################
# Function definitions

################################################################################
# Executed statements

#------------------------------------------------------------------------------
# Read data
original <- read.csv(file.path("..", "Data", "COTS data AIMS04.csv"),
                     sep=";", dec=",")

# Select arm count data
arm.data <- select(original, type, order, no.arms)

# First measurement (order=1) contains arm count for each animal
arm.data <- filter(arm.data, order == 1)

#------------------------------------------------------------------------------
# Tests

# ANOVA
arm.aov <- summary(aov(no.arms ~ type, data = arm.data))

# Kruskall Wallis test
arm.kw <- with(arm.data, kruskal.test(no.arms, type))

#------------------------------------------------------------------------------
# Save tests and workspace

# Save test results in text file
sink(file.path("..", "Output", "Arm_count_statistics.txt"))
print("ANOVA")
print(arm.aov)
print("")
print("Kruskall Wallis test")
print(arm.kw)
sink()

# Save workspace as RData file
save.image(file.path("..", "Output", "Arm_count_statistics.RData"))
