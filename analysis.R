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
# Purpose: Master script to run data analysis and report generation.
#
# Args:
#   Input: none
#
# Returns:
#   none

################################################################################
# source() and library() statements

# Clear workspace first
rm(list=ls())

# Uncomment to install packages. Packages are attached in function scripts
# install.packages("ggplot2")
# install.packages("grid")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("circular")
# install.packages("knitr")
# install.packages("bibtex")
# install.packages("rmarkdown")

################################################################################
# Function definitions

################################################################################
# Executed statements

# 0: Make sure output folders exist
# Graphical output
dir.create("Figs", showWarnings = F)
# Numeric output
dir.create("Output", showWarnings = F)

#-------------------------------------------------------------------------------
# Analysis

# First run analysis of Directional data for AIMS experiments. This is time
# consuming: optional.
message("Running directional analysis...")
source(file.path("Fun", "COTS_behaviour_directions07.R"), chdir=T, local=T)
message("Done")
#rm(list=ls())

# Then plot the effects of animal size
message("Running animal size analysis...")
source(file.path("Fun", "COTS_sizes04.R"), chdir=T, local=F)
message("Done")
rm(list=ls())

# And the timing of the behaviours
message("Running timing analysis...")
source(file.path("Fun", "COTS_timing03.R"), chdir=T, local=F)
message("Done")
rm(list=ls())

# The effect of time in captivity
message("Assessing effect of time in captivity...")
source(file.path("Fun", "Captivity02.R"), chdir=T, local=F)
message("Done")
rm(list=ls())

# Coral reef contrast
message("Calculating coral reef contrasts...")
source(file.path("Fun", "Coral_reef_contrast04.R"), chdir=T, local=F)
message("Done")
rm(list=ls())

# Tracking data AIMS
message("Analysing tracking data AIMS...")
source(file.path("Fun", "Tracking04.R"), chdir=T, local=F)
message("Done")
message("")
rm(list=ls())

# Black VS white area when approaching circular stimulus
message("Processing black and white area comparison...")
source(file.path("Fun", "B_W_area_comparison01.R"), chdir=T, local=F)
message("Done")
message("")
rm(list=ls())

# Arms (eyes) per experiment
message("Running arm count analysis...")
source(file.path("Fun", "Arm_count_statistics.R"), chdir=T, local=F)
message("Done")
rm(list=ls())

#-------------------------------------------------------------------------------
# Report

# Render report
message("Rendering PDF and DOCX files for main document")
message("PDF file...")
setwd("Doc")
rmarkdown::render("COTS_behaviour09.Rmd", output_format = "pdf_document")
message("Done")
message("DOCX file...")
rmarkdown::render("COTS_behaviour09.Rmd", output_format = "word_document")
setwd("..")

message("Done")
message("")

#------------------------------------------------------------------------------
# Supplementary tables

# Render supplementary tables
message("Rendering PDF and DOCX files for supplementary tables")
message("PDF file...")
setwd("Doc")
rmarkdown::render("Supplementary_tables.Rmd", output_format = "pdf_document")
message("Done")
message("DOCX file...")
rmarkdown::render("Supplementary_tables.Rmd", output_format = "word_document")
setwd("..")

message("Done")
message("")

#------------------------------------------------------------------------------
# Session info

message("Saving session info...")
sink("session_info.txt")
print(sessionInfo())
sink()
message("Done")
