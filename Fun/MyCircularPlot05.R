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

MyCircularPlot <- function(data, title) { 
  # Purpose: Plot custom circular plots. Confidence intervals and Rayleigh test
  # are shown. Optimised for saving plots as an .svg file.
  #
  # Args:
  #   data: - data in degrees
  #         - title
  # 
  # Returns:
  #   Plots the data and saves plot as .svg and returns:
  #   - average heading, confidence interval, p-value (Rayleigh test)
  
  #-----------------------------------------------------------------------------
  # Create circular data
  circular.data <- circular(data, units="degrees", rotation="clock", zero=pi/2)
  
  # Calculate statistics.
  # Rayleigh test
  p.Rayleigh <- rayleigh.test(circular.data)$p.value 
  # Mean heading
  mean.heading <- mean(circular.data, na.rm=TRUE)
  # Variance
  rho.heading <- rho.circular(circular.data, na.rm=TRUE)
  # Confidence interval
  confidence<-(mle.vonmises.bootstrap.ci(circular.data, alpha=.05))$mu.ci
  
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
  
  #-----------------------------------------------------------------------------
  # Plotting parameters
  inch <- 2.54
  bg <- "transparent"
  width <- 6 / inch
  height <- 9 / inch
  family <- "Arial"
  pointsize <- 9
  # Character expantion value
  cex.value<-1
  
  # The final pointsize is a little too large. Correct for this.
  pointsize <- pointsize * 0.80
  
  # Define tickmark location for graphs
  mark.loc<-circular(c(0, 45, 90, 135, 180, 225, 270, 315),units="degrees",
                     rotation="clock", zero=pi/2)
  
  #-----------------------------------------------------------------------------
  # 1. Save as svg, angular data
  svg(paste(title, "-angular.svg", sep=""),
      width = width,
      height = height,
      pointsize = pointsize,
      family = family,
      bg = bg)
  
  # Make plot, use 360 bins, expand the text to produce a nice image when 
  # exported to svg (cex), specify tickmark lenght (tcl) and white space at 
  # margins (tol). Stack observation (stack) with (sep) spacing between points.
  plot(circular.data, stack=TRUE,
       bins=360/5, shrink=1.8195, cex.main=1, cex=cex.value, sep=0.1, 
       tcl.text=0.3, tol=0.07, control.circle=circle.control(lwd=1))
  # Add mean vector
  arrows.circular(mean.heading, rho.heading, lwd=1, length=0.2/inch)
  # Title
  text(0, 1.4, paste(title, "-angular", sep=""), cex=cex.value)
  
  # Print mean direction as a number between 0 and 360 degrees
  if (mean.heading<0) {
    meanDir <- 360 + mean.heading
    text(0, -1.25, paste("Mean = ", sprintf("%.1f", meanDir), "°", sep=""),
         cex=cex.value)
  } else {
    meanDir <- mean.heading
    text(0, -1.25, paste("Mean = ", sprintf("%.1f", meanDir), "°", sep=""),
         cex=cex.value)
  }
  
  # Print vector length
  text(0, -1.5, paste("r = ", toString(signif(rho.heading, digits=2))))
  
  # Print N
  text(0, -1.75, paste("N = ", toString(signif(length(axial.data)))))
  
  # Print p-value (R-test)
  if (is.na(p.Rayleigh) == FALSE & p.Rayleigh < 0.001)
  {text(0, -2, "p < 0.001", cex=cex.value)
  } else {
    text(0, -2, paste("p = ", sprintf("%.3f",p.Rayleigh)), cex=cex.value)
  }
  
  # Add tickmarks
  ticks.circular(mark.loc, zero=pi/2, rotation="clock", tcl=0.1, lwd=1)
  
  # show 95% confidence intervals when p.Rayleigh is smaller then 5%
  if(is.na(p.Rayleigh) == FALSE & p.Rayleigh <= 0.05)
  {arrows.circular(confidence, c(1,1), lwd=1, lty=2, length=0)}
  
  # Close graphical device
  dev.off() 
  
  #-----------------------------------------------------------------------------
  # 2. Save as svg, axial data
  svg(paste(title, "-axial.svg", sep=""),
      width = width,
      height = height,
      pointsize = pointsize,
      family = family,
      bg = bg)
  
  # Make plot, use original undoubled data for plotting. Use stats from doubled
  # data.
  plot(circular.data, stack=TRUE,
       bins=360/5, shrink=1.8195, cex.main=1, cex=cex.value, sep=0.1, 
       tcl.text=0.3, tol=0.07, control.circle=circle.control(lwd=1))
  # Add axial direction
  arrows.circular(mean.heading.ax, rho.heading.ax, lwd=1, length=0.2/inch)
  arrows.circular((mean.heading.ax+180), rho.heading.ax, lwd=1, length=0.2/inch)
  # Title
  text(0, 1.4, paste(title, "-axial", sep=""), cex=cex.value)
  
  # Print mean direction as a number between 0 and 360 degrees
  if (mean.heading<0) {
    meanDir <- 360 + mean.heading.ax
    text(0, -1.25, paste("Mean = ", sprintf("%.1f", meanDir), "°", sep=""),
         cex=cex.value)
  } else {
    meanDir <- mean.heading.ax
    text(0, -1.25, paste("Mean = ", sprintf("%.1f", meanDir), "°", sep=""),
         cex=cex.value)
  }
  
  # Print vector length
  text(0, -1.5, paste("r = ", toString(signif(rho.heading.ax, digits=2))))
  
  # Print N
  text(0, -1.75, paste("N = ", toString(signif(length(circular.data)))))
  
  # Print p-value (R-test)
  if (is.na(p.Rayleigh.ax) == FALSE & p.Rayleigh.ax < 0.001)
  {text(0, -2, "p < 0.001", cex=cex.value)
  } else {
    text(0, -2, paste("p = ", sprintf("%.3f",p.Rayleigh.ax)), cex=cex.value)
  }
  
  # Add tickmarks
  ticks.circular(mark.loc, zero=pi/2, rotation="clock", tcl=0.1, lwd=1)
  
  # show 95% confidence intervals when p.Rayleigh is smaller then 5%
  if(is.na(p.Rayleigh.ax) == FALSE & p.Rayleigh.ax <= 0.05) {
    arrows.circular(confidence.ax, c(1,1), lwd=1, lty=2, length=0)
    arrows.circular(confidence.ax+180, c(1,1), lwd=1, lty=2, length=0)
  }
  
  # Close graphical device
  dev.off() 
  
  #-----------------------------------------------------------------------------
  # Return average heading, confidence interval and p-value
  # Make dataframe
  summary <- data.frame(test=c("angular", "axial"),
                        n=rep(length(data),2),
                        mean=as.numeric(c(mean.heading, mean.heading.ax)),
                        rho=c(rho.heading, rho.heading.ax), 
                        confidence.1=as.numeric(c(confidence[1],
                                                  confidence[1]+180)),
                        confidence.2=as.numeric(c(confidence[2],
                                                  confidence[2]+180)), 
                        Rayleigh=c(p.Rayleigh, p.Rayleigh.ax))
  
  # Return data
  return(summary)
}
