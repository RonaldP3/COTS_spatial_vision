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
# Purpose: Plot example tracks for experiments done at AIMS.
#
# Args:
#   Input: "Trackingxx.csv" file in Data folder
#
# Returns:
#   Track plots

################################################################################
# source() and library() statements

# Clear workspace first
rm(list=ls())

# Attach packages:
library(ggplot2)
library(dplyr)
library(circular)

################################################################################
# Function definitions

polar2cart<-function(x,y,r,theta,as.deg=FALSE){
  ## Translate Polar coordinates into Cartesian coordinates
  ## based on starting location (x,y), distance (r), and bearing (theta)
  ## as.deg indicates if the bearing is in degrees (T) or radians (F)
  
  if(as.deg){
    ##if bearing is in degrees, convert to radians
    theta=theta*pi/180
  }
  
  newx<-x+r*cos(theta)  ##X
  newy<-y+r*sin(theta)  ##Y
  return(data.frame("x.cart"=newx,"y.cart"=newy))
}

################################################################################
# Read data

# Save current dir
current.dir <- getwd()

# Import data
file.name <- "Tracking03.csv"
# file.name <- "Test track.csv"
data <- read.csv(file.path("..","Data", file.name), sep=";", dec=",")

# correct coordinates
data$x <-data$x + data$x.cor 
data$y <-data$y + data$y.cor

# In ImageJ the upperleft corner is the origin, in R the lower left. correct.
# NB. All images have final dimentions 1500x1125 (xmax, ymax)
data$y <- 1225 - data$y

# Stimuli info
stim.info <- filter(data, type != "track")

# Tracks
tracks <- filter(data, type == "track") %>%
  select(-type)
# remove correction factor from df
tracks <- tracks %>% select(-x.cor, -y.cor)

################################################################################
# Determine polar coordinates of stimuli

# Get dx and dy
stim.info$dx <- c(NA, diff(stim.info$x))
stim.info$dy <- c(NA, diff(stim.info$y))

# Polar coordinates:
# Angle
stim.info$theta <- with(stim.info, coord2rad(dx, dy))
# length
stim.info$r <- with(stim.info, sqrt(dx^2 + dy^2))

# Polar coordinates are only correctly calculated for "stimulus" rows. Mark
# other with NA
stim.info$theta[stim.info$type=="centre"] <- NA
stim.info$r[stim.info$type=="centre"] <- NA
stim.info$dx[stim.info$type=="centre"] <- NA
stim.info$dy[stim.info$type=="centre"] <- NA

################################################################################
# Set up data frames with tracks

#-------------------------------------------------------------------------------
# Center coordinate system on centre of arena

# Iterate through entries to add centre coordinates and stimulus info.
for (i in 1:length(tracks$x)) {
  # Extract values from stimulus info
  coord <- filter(stim.info, type=="centre",
                  stimulus==tracks$stimulus[i],
                  animal==tracks$animal[i]) %>%
    select(x, y)
  
  # extract arena radius
  stimulus <- filter(stim.info, type=="stimulus",
                     stimulus==tracks$stimulus[i],
                     animal==tracks$animal[i]) %>%
    select(theta, r)
  
  # Append data
  if (i==1) {
    output <- cbind(coord, stimulus)
  } else {
    output <- rbind(output, cbind(coord, stimulus))
  }
}

# Add centre info, but first rename collumns
colnames(output) <- c("x.centre", "y.centre", "stim.heading", "radius")
# Add centre info to tracks data frame
tracks <- cbind(tracks, output)
# cleanup
rm(coord, output, stimulus, i)

# Calculate new coordinates
tracks$xnew <- with(tracks, (x-x.centre))
tracks$ynew <- with(tracks, (y-y.centre))

#-------------------------------------------------------------------------------
# Make polar coordinates:

# Angle
tracks$theta <- with(tracks, coord2rad(xnew,ynew))
# Length
tracks$r <- with(tracks, sqrt(xnew^2 + ynew^2))

# Make radius relative. Arena edge is one, centre is zero
tracks$r.rel <- tracks$r / tracks$radius

#-------------------------------------------------------------------------------
# Rotate

# Stimulus can be located at five different headings. Correct found theta for
# stimulus location
tracks$theta.corr <- tracks$theta -tracks$stim.heading + pi/2

# Make tidier version of "tracks"
tracks.original <- tracks
tracks <- select(tracks, animal, stimulus, t, r.rel, theta.corr)

#-------------------------------------------------------------------------------
# Convert back to cartesian

# Calculate cartesian coordinates
cart <- polar2cart(0, 0, tracks$r.rel, tracks$theta.corr, as.deg = F)

# Add to tracks data frame
tracks <- cbind(tracks, cart)
rm(cart)

################################################################################
# Calculate speeds

# X and Y displacement
tracks$dx <- c(0, diff(tracks$x.cart))
tracks$dy <- c(0, diff(tracks$y.cart))

# Set displacement at t=0 to 0
tracks$dx[tracks$t == 0] <- 0
tracks$dy[tracks$t == 0] <- 0

# Instantaneous speed in normalised units/sec
tracks$speed <- sqrt(tracks$dx^2 + tracks$dy^2) / 5
# Speed in units/min
tracks$speed <- tracks$speed*60
# All tracks are normalised where the coordinate system is centred on the centre
# of the arena and the distance to the arena wall is 1. In real life the radius
# of the arena is 80 cm
tracks$speed <- tracks$speed*80

################################################################################
# Plot data

#-------------------------------------------------------------------------------
# Prepare data

# Make animal factor
tracks$animal <- factor(tracks$animal)

# Now all data is in one data frame. Create separate data frames for experiments
control.tracks <- filter(tracks, stimulus == "control") %>%
  select(-stimulus)
normal.tracks <- filter(tracks, stimulus == "37°") %>%
  select(-stimulus)

# Variables needed for circle around origin
angle <- seq(-pi, pi, length = 200)
df <- data.frame(x = sin(angle), y = cos(angle))

#-------------------------------------------------------------------------------
# Set up skeleton plot
base.plot <- ggplot() +
  # Axis limits
  xlim(-1.3, 1.3) +
  ylim(-1.3, 1.3) +
  # Circle of radius 1
  geom_polygon(data = df, aes(x, y),inherit.aes = F,
               fill = "white", colour =  "black") +
  # Marker for circle centre
  geom_point(data = data.frame(x=0, y=0), aes(x,y), shape = 4, size = 7) +
  # Labels
  annotate("text", x = 0, y = 1.2, label = "0°") +
  annotate("text", x = 1.2, y = 0, label = "90°") +
  annotate("text", x = 0, y = -1.2, label = "180°") +
  annotate("text", x = -1.2, y = 0, label = "270°") +
  # Equal aspect ratio
  coord_fixed(ratio = 1) +  
  # Completely blank background
  theme(line = element_blank(),
        rect = element_blank(),
        text = element_blank())

#-------------------------------------------------------------------------------
# Create plots

# 1. Plot control data
control.plot <- base.plot +
  ggtitle("Control") +
  geom_path(data=control.tracks,
            aes(x=x.cart, y=y.cart, group=animal, colour=animal),
            show.legend = FALSE) +
  scale_colour_manual(values=rainbow(9))
#   scale_colour_grey(start = 0, end = 0.8)
#print(control.plot)

# 2. Plot normal data
stimulus.plot <- base.plot +
  ggtitle("37°") +
  geom_path(data=normal.tracks,
            aes(x=x.cart, y=y.cart, group=animal, colour=animal),
            show.legend = FALSE) +
  scale_colour_manual(values=rainbow(10))
#print(stimulus.plot)

# 3. Save SVG's
# Create dir
setwd(file.path("..", "Figs"))
dir.create("Directional plots-AIMS", showWarnings = F)
setwd("Directional plots-AIMS")
dir.create("Tracks", showWarnings = F)
setwd("Tracks")

# Save file
svg("Control_tracks.svg", width=2.64, height=2.64, pointsize=8)
#print(control.plot)
dev.off()

svg("Stimulus_tracks.svg", width=2.64, height=2.64, pointsize=8)
print(stimulus.plot)
dev.off()

setwd(current.dir)

################################################################################
# Create summaries

# Summarise data per animal
speed.animal <- summarise(group_by(tracks, stimulus, animal),
                          n = length(speed),
                          mean.speed = mean(speed),
                          sd.speed = sd(speed))

# Summarise per experiment
speed.experiment <- summarise(group_by(speed.animal, stimulus),
                              n = length(mean.speed),
                              mean = mean(mean.speed),
                              sd = sd(mean.speed))

################################################################################
# Calculate statistics

# organise data for t test
test <- filter(speed.animal, stimulus == "37°") %>%
  select(mean.speed)
control <- filter(speed.animal, stimulus == "control") %>%
  select(mean.speed)

# t-test
speed.test.tank <- t.test(test$mean.speed, control$mean.speed,
                          alternative = "two.sided",
                          paired = F)

# Mann-Whitney
speed.test.tank.wilcox <- wilcox.test(test$mean.speed, control$mean.speed,
                                      alternative = "two.sided",
                                      paired = F)

# Save stats
sink(file.path("..", "Output", "Directional data-AIMS", 
               "Speed - statistics.txt"))
print("T-test")
print(speed.test.tank)
print("Mann-Whitney")
print(speed.test.tank.wilcox)
sink()


################################################################################
# Save summaries and stats

# Set working directory
setwd(file.path("..", "Output"))
dir.create("Directional data-AIMS", showWarnings = F)
setwd("Directional data-AIMS")

# Save summaries as CSV files
write.table(speed.animal, file = "Speed_animal.csv", sep=";", dec=",",
            row.names = F)
write.table(speed.experiment, file = "Speed_experiment.csv", sep=";", dec=",",
            row.names = F)

# Save stats and summaries as RData file
save(speed.animal, speed.experiment, speed.test.tank, file = "Arena_speeds.RData")


# Navigate back
setwd(current.dir)

# Cleanup
rm(angle, df, control.tracks, normal.tracks)
