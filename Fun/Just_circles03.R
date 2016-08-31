################################################################################
# Copyright statement
# This script, or parts of it, can be used or reproduced under the terms of the
# Creative Commons Attribution License which permits any use, distribution, and 
# reproduction in any medium, provided the original author(s) and the source are
# credited.

################################################################################
# Author: Ronald Petie

################################################################################
# source() and library() statements
library("circular")

################################################################################
# Function definitions

################################################################################
# Executed statements

JustCircles <- function(data, type, annotation, title, plot=TRUE) { 
  # Purpose: Plot custom circular plots. Confidence intervals and Rayleigh test
  # are shown.
  #
  # Args:
  #   data: - data in degrees
  #         - title
  #         - analysis type, angles or axial
  # 
  # Returns:
  #   Plain plots
  
  #-----------------------------------------------------------------------------
  # Create circular data
  circular.data <- circular(data, units="degrees", rotation="clock", zero=pi/2)
  
  if (type=="angular") {
    # Calculate statistics.
    # Rayleigh test
    p.Rayleigh <- rayleigh.test(circular.data)$p.value 
    # Mean heading
    mean.heading <- mean(circular.data, na.rm=TRUE)
    # Variance
    rho.heading <- rho.circular(circular.data, na.rm=TRUE)
    # Confidence interval
    confidence<-(mle.vonmises.bootstrap.ci(circular.data, alpha=.05))$mu.ci
  }
  
  if (type=="axial"){
    # Axial data. First double angles:
    axial.data <- circular.data*2
    # Then calculate statistics
    p.Rayleigh.ax <- rayleigh.test(axial.data)$p.value 
    # Mean heading. OBS. devide by two!
    mean.heading.ax <- mean(axial.data, na.rm=TRUE)/2
    # Variance
    rho.heading.ax <- rho.circular(axial.data, na.rm=TRUE)
    # Confidence interval, also devide by two
    confidence.ax<-(mle.vonmises.bootstrap.ci(axial.data, alpha=.05))$mu.ci/2
  }
  
  #-----------------------------------------------------------------------------
  # Plotting parameters
  # Character expantion value
  cex.value<-1
  # Inch
  inch <- 2.54
  # fonts: 1, plain; 2, bold; 3,italic; 4, bold-italic
  font <- 1
  # font family
  family <- "sans"
  # Define tickmark location for graphs
  mark.loc<-circular(c(0, 45, 90, 135, 180, 225, 270, 315),units="degrees",
                     rotation="clock", zero=pi/2)
  if (plot) {
    #-----------------------------------------------------------------------------
    # 1. Save angular plot
    
    # Make plot, use 360 bins, expand the text to produce a nice image when 
    # exported to svg (cex), specify tickmark lenght (tcl) and white space at 
    # margins (tol). Stack observation (stack) with (sep) spacing between points.
    if (type=="angular"){
      plot(circular.data, stack=TRUE,
           bins=360/5, cex.main=1, cex=cex.value, sep=0.1, 
           tcl.text=0.3, tol=0.07, control.circle=circle.control(lwd=1))
      # Add mean vector
      arrows.circular(mean.heading, rho.heading, lwd=1, length=0.2/inch)
      # Title
      text(0, 1.6, title, cex=cex.value, font=font, family=family)
      # Annotation. 
      text(-1, 1.6, annotation, cex=cex.value, font=font, family=family)
      
      # Add tickmarks
      ticks.circular(mark.loc, zero=pi/2, rotation="clock", tcl=0.1, lwd=1)
      
      # show 95% confidence intervals when p.Rayleigh is smaller then 5%
      if(is.na(p.Rayleigh) == FALSE & p.Rayleigh <= 0.05)
      {arrows.circular(confidence, c(1,1), lwd=1, lty=2, length=0)}
    }
    
    #-----------------------------------------------------------------------------
    # 2. Save axial plot
    
    if (type=="axial") {
      # Make plot, use original undoubled data for plotting. Use stats from
      # doubled data.
      plot(circular.data, stack=TRUE,
           bins=360/5, cex.main=1, cex=cex.value, sep=0.1, 
           tcl.text=0.3, tol=0.07, control.circle=circle.control(lwd=1))
      # Add axial direction
      arrows.circular(mean.heading.ax, rho.heading.ax, lwd=1, length=0.2/inch)
      arrows.circular((mean.heading.ax+180), rho.heading.ax, lwd=1, 
                      length=0.2/inch)
      # Title
      text(0, 1.6, title, cex=cex.value, font=font, family=family)
      
      # Annotation
      text(-1, 1.6, annotation, cex=cex.value, font=font, family=family)
      
      # Add tickmarks
      ticks.circular(mark.loc, zero=pi/2, rotation="clock", tcl=0.1, lwd=1)
      
      # show 95% confidence intervals when p.Rayleigh is smaller then 5%
      if(is.na(p.Rayleigh.ax) == FALSE & p.Rayleigh.ax <= 0.05) {
        arrows.circular(confidence.ax, c(1,1), lwd=1, lty=2, length=0)
        arrows.circular(confidence.ax+180, c(1,1), lwd=1, lty=2, length=0)
      }
    }
  }
  
  #-----------------------------------------------------------------------------
  # Return average heading, confidence interval and p-value
  # Make dataframe
  if (type=="angular"){
    summary <- data.frame(test="angular",
                          n=length(data),
                          mean=as.numeric(mean.heading),
                          rho=rho.heading, 
                          confidence.1=as.numeric(confidence[1]),
                          confidence.2=as.numeric(confidence[2]), 
                          Rayleigh=p.Rayleigh)
  }
  
  if (type=="axial"){
    summary <- data.frame(test="axial",
                          n=length(axial.data),
                          mean=as.numeric(mean.heading.ax),
                          rho=c(rho.heading.ax), 
                          confidence.1=as.numeric(confidence.ax[1]),
                          confidence.2=as.numeric(confidence.ax[2]), 
                          Rayleigh=p.Rayleigh.ax)
  }
  # Return data
  return(summary)
}
