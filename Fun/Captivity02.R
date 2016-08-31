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
# Purpose: Plot influence of time in captivity on behaviour
#
# Args:
#   Input: CSV file in parent directory
#
# Returns:
#   A pdf with summary stats, via Rmarkdown

################################################################################
# source() and library() statements

# Clear workspace first
rm(list=ls())

# Attach packages:
library("dplyr")
library("ggplot2")
library("tidyr")

################################################################################
# Function definitions

################################################################################
# Executed statements

inch <- 2.54 # cm

#-------------------------------------------------------------------------------
# Read and shape data

# Read data file.
data <- read.csv(file.path("..", "Data", "COTS data AIMS04.csv"),
                 sep=";", dec=",")

# Filter data
data <- select(data, type, order, animal.id, animal.diameter, date, batch,
               start.frame, stop.frame, used.before, stimulus.name)

# Make dates as dates
data$date <- as.Date(data$date)

# Fill out empty "batch" fields
# Unique animal IDs
data$animal.id <- factor(data$animal.id)
id <- levels(data$animal.id)
# Iterate through data
for (i in 1:length(id)){
  data$batch[data$animal.id==id[i]] <- 
    data$batch[data$animal.id==id[i]][1]
}

# Fill out empty "date" fields
# Iterate through data
for (i in 1:length(id)){
  data$date[data$animal.id==id[i]] <- 
    data$date[data$animal.id==id[i]][1]
}


# Collection date, called "batch", is in weeks. Set to date.
# First get week number
nchar <- nchar(as.character(data$batch))
# Collection week is always in last two characters
week.nr <- as.numeric(substr(data$batch, nchar-2, nchar))
# Turn week number into date. %w 5, sets delevery day to Thursday, %W sets week.
# Somehow the week nr needs to be subtracted with one to get right week 
# according to my calender.
# Set delivery date.
data$delivery.date <- as.Date(paste(4, week.nr-1, 2015, sep = "-"), 
                                format = "%w-%W-%Y")

# Remove orders above 3, NA's and unused stimuli.
data <- filter(data, order <= 3, stimulus.name != "remove")

#-------------------------------------------------------------------------------
# Timing

# Calculate days in captivity
data$captivity.days <- as.numeric(data$date - data$delivery.date)

# Timing in minutes. (5 second interval between frames)
data$start.time <- data$start.frame*5/60
data$stop.time <- data$stop.frame*5/60
data$elapsed.time <- data$stop.time - data$start.time

#-------------------------------------------------------------------------------
# Plots
elapsed <- ggplot(data, aes(x=captivity.days, y=elapsed.time)) +
  xlab("Days in captivity") +
  ylab("Elapsed time (min)") +
  geom_point() +
  geom_smooth() +
  theme_bw()
# print(elapsed)

start <- ggplot(data, aes(x=captivity.days, y=start.time)) +
  xlab("Days in captivity") +
  ylab("Time to start (min)") +
  geom_point() +
  geom_smooth() +
  theme_bw()
# print(start)

distribution <- ggplot(data, aes(x="All experiments", y=captivity.days)) +
  geom_dotplot(binaxis="y", binwidth = 1) +
  ylab("Days in captivity") +
#   ggtitle("Days in captivity when used") +
  theme_bw()
# print(distribution)

size <- ggplot(data, aes(x=captivity.days, y=animal.diameter)) +
  xlab("Days in captivity") +
  ylab("Animal diameter") +
  geom_point() +
  geom_smooth() +
  theme_bw()
# print(size)

#-------------------------------------------------------------------------------
# Save plots

# Make dir
setwd(file.path("..", "Figs"))
dir.create("Captivity", showWarnings = F)
setwd("Captivity")

# Save figures
png("Elapsed VS days in captivity.png", width = 800, height = 400)
print(elapsed)
dev.off()

# Save figures
png("Onset VS days in captivity.png", width = 800, height = 400)
print(start)
dev.off()

# Save figures
png("Distribution.png", width = 800, height = 400)
print(distribution)
dev.off()

# Save figures
png("Size VS days in captivity.png", width = 800, height = 400)
print(size)
dev.off()

setwd(file.path("..", "..", "Fun"))

#------------------------------------------------------------------------------
# Create and save summary table

# Generate summary table captivity days
captivity.summary <- filter(data, is.na(animal.diameter) == F) %>%
  select(type, animal.id, captivity.days) %>%
  group_by(type) %>%
  summarise(min = quantile(captivity.days)[1],
            first.quantile = quantile(captivity.days)[2],
            median = quantile(captivity.days)[3],
            third.quantile = quantile(captivity.days)[4],
            max = quantile(captivity.days)[5])

# Generate summary table first and second use
usage.table <- filter(data, is.na(animal.diameter) == F) %>%
  select(type, animal.id, used.before) %>%
  group_by(type, used.before) %>%
  summarise(first.use = length(used.before))
# Reshape data
usage.table <- spread(usage.table, used.before, first.use)
# Rename collumns
usage.table <- select(usage.table,type, first.use=no, second.use=yes)
# Replace NA's with zero
usage.table$second.use[is.na(usage.table$second.use)] <- 0

# Combine captivity days table with usage table
combined.captivity.data <- left_join(captivity.summary,
                            usage.table,
                            by = "type")

# Export table
write.csv(combined.captivity.data,
          file = file.path("..", "Output", "Captivity_summary.csv"),
          row.names = F)

#------------------------------------------------------------------------------
# Save RDATA

# Clean first
rm(i, id, inch, nchar)

# Save RData file
captivity <- data
save.image(file= file.path("..", "Figs", "Captivity", "Captivity.RData"))
