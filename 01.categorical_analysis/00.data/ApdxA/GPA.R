###########################################################################
# NAME:  Chris Bilder                                                     #
# DATE:  3-11-14                                                          #
# PURPOSE: Regression model for GPA data                                  #
# NOTES:                                                                  #
###########################################################################
    
    
# Read in the data - change file location for your own computer
gpa<-read.table(file = "C:\\data\\GPA.txt", header = TRUE, sep = "")

# Alternatively, one can use the code below to set the folder where the data is stored
setwd(dir = "C:\\data")
gpa2<-read.table(file = "GPA.txt", header = TRUE, sep = "")
 
# Print data set
gpa           

# Comma delimited file
gpa3<-read.table(file = "GPA.csv", header = TRUE, sep = ",")
gpa4<-read.csv(file = "GPA.csv")

# Read in an Excel .xls version of the data - works with 32-bit R and 32-bit Excel
# Notes: The RODBC package needs to be installed first
#   The odbcConnectExcel() function creates a link between R and the data file.
#   The sqlFetch() function uses this link to read in the data located in “sheet1”
#     of the Excel file.
#   The connection to the Excel file is closed using the close() function.
library(package = RODBC)
z<-odbcConnectExcel(xls.file = "C:\\data\\GPA.xls")
gpa.excel<-sqlFetch(channel = z, sqtable = "sheet1")
close(z)
# Further notes:
#   Use odbcConnectExcel2007() for .xlsx files.
#   Other packages, such as xlsx package and xlsReadWrite package, also contain functions to read in Excel files.
#   Not all packages work with 64-bit Excel.

# Summary statistics for variables
summary(object = gpa) # summary(gpa) works as well

# Simple plot
plot(x = gpa$HS.GPA, y = gpa$College.GPA)

# Better plot
# pdf(file = "c:\\figures\\FigureA.5color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA",
     xlim = c(0,4.5), ylim = c(0,4.0), col = "red", pch = 1, cex = 1.0, panel.first = grid(col = "gray", lty = "dotted"))
# dev.off()  # Create plot for book

# Black-and-white version of plot
# pdf(file = "c:\\figures\\FigureA.5BW.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA",
     xlim = c(0,4.5), ylim = c(0,4.0), col = "black", pch = 1, cex = 1.0, panel.first = grid(col = "gray", lty = "dotted"))
# dev.off()  # Create plot for book


# Print just one variable
options(width = 60) # Used in book to limit width of line in R Console window
gpa$HS.GPA
options(width = 117) # Set back to default
gpa[,1]


########################################################################
# Find estimated simple linear regression model
     
  # Fit the simple linear regression model and save the results in mod.fit
  mod.fit<-lm(formula = College.GPA ~ HS.GPA, data = gpa)

  # A very brief look of what is inside of mod.fit - see the summary function for a better way
  mod.fit

  # See the names of all of the object components
  names(mod.fit)
  mod.fit$coefficients 
  round(mod.fit$residuals[1:5],2)

  # Put some of the components into a data.frame object
  save.fit<-data.frame(gpa, C.GPA.hat = round(mod.fit$fitted.values,2), residuals = round(mod.fit$residuals,2))
  
  # Print contents save.fit 
  head(save.fit)
  save.fit

  # Summarize the information stored in mod.fit
  summary(object = mod.fit)    

  # Class of objects
  class(mod.fit)
  class(gpa)
   
  # Method functions for a class of type lm
  # options(width = 70) # Used for book to control width displayed
  methods(class = lm)

  
  # Method functions for the summary generic function
  methods(generic.function = summary)
  # options(width = 121) # Set back to default
  
########################################################################
# Put regression line on plot

  # Open a new graphics window - there are a number ways
  # win.graph(width = 8, height = 6, pointsize = 10)  # Windows
  # windows(width = 8, height = 6, pointsize = 10)  # Windows
  # quartz(width = 8, height = 6, pointsize = 10) # Mac
  x11(width = 8, height = 6, pointsize = 10) # Originally for Linux, but works in Windows
  # pdf(file = "c:\\figures\\FigureA.6color.pdf", width = 8, height = 6, colormodel = "cmyk", pointsize = 10)   # Create plot for book in a PDF file

  
  # 1 row and 2 columns of plots
  par(mfrow = c(1,2))

  # Same scatter plot as before
  plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA", 
     xlim = c(0,4.5), ylim = c(0,4.0), panel.first=grid(col = "gray", lty = "dotted"))
  
  # Puts the line y = a + bx on the plot
  abline(a = mod.fit$coefficients[1], b = mod.fit$coefficients[2], lty = "solid", col = "blue", lwd = 2)

  # Same scatter plot as before
  plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA", 
    xlim = c(0,4.5), ylim = c(0,4.0), panel.first=grid(col = "gray", lty = "dotted"))

  # Add line
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x, xlim = c(min(gpa$HS.GPA),max(gpa$HS.GPA)),
    col= "blue", add = TRUE, lwd = 2)
  # dev.off()  # End creating plot for book in a PDF file

  # Draw a line from (x0, y0) to (x1, y1)
  # segments(x0 = min(gpa$HS.GPA), y0 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*min(gpa$HS.GPA),
  #          x1 = max(gpa$HS.GPA), y1 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*max(gpa$HS.GPA),
  #          lty = 1, col = "blue", lwd = 2)


   # pdf(file = "c:\\figures\\FigureA.6color.pdf", width = 8, height = 6, colormodel = "cmyk", pointsize = 10)   # Create plot for book in a PDF file


  # Black-and-white version of plot
  # pdf(file = "c:\\figures\\FigureA.6BW.pdf", width = 8, height = 6, colormodel = "cmyk", pointsize = 10)   # Create plot for book
  par(mfrow = c(1,2))
  plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA",
     xlim = c(0,4.5), ylim = c(0,4.0), panel.first=grid(col = "gray", lty = "dotted"))
  abline(a = mod.fit$coefficients[1], b = mod.fit$coefficients[2], lty = "solid", col = "black", lwd = 2)

  plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA",
    xlim = c(0,4.5), ylim = c(0,4.0), panel.first=grid(col = "gray", lty = "dotted"))
  curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x, xlim = c(min(gpa$HS.GPA),max(gpa$HS.GPA)),
    col= "black", add = TRUE, lwd = 2)
  # dev.off()  # End creating plot for book in a PDF file

############################################################################################################
# Create a function to find the estimated simple linear regression model and put the line on a scatter plot

  my.reg.func<-function(x, y, data) {
  
    # Fit simple linear regression model and save results in mod.fit
    mod.fit<-lm(formula = y ~ x, data = data)
  
    # Open a new graphics window - do not need to
    x11(width = 6, height = 6, pointsize = 10)
 
    # Same scatter plot as before
    plot(x = x, y = y, xlab = "x", ylab = "y", main = "y vs. x",
      panel.first = grid(col = "gray", lty = "dotted"))

    # Include regression model
    curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2]*x,
      xlim = c(min(x), max(x)), col = "blue", add = TRUE, lwd = 2)

    # This is the object returned
    mod.fit           
  }

  save.it<-my.reg.func(x = gpa$HS.GPA, y = gpa$College.GPA, data = gpa)  
  names(save.it)
  summary(save.it)
  
  
#########################################################################
# Specific x-axis values 

  # Note that xaxt = "n" tells R to not give any labels on the x-axis (yaxt = "n" works for y-axis)
  plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", main = "College GPA vs. HS GPA", 
       xaxt = "n", xlim = c(0, 4.5), ylim = c(0,4.5), col = "red", pch = 1, cex = 1.0)
  axis(side = 1, at = seq(from = 0, to = 4.5, by = 0.5)) # Major tick marks
  axis(side = 1, at = seq(from = 0, to = 4.5, by = 0.1), tck = 0.01, labels = FALSE) # Minor tick marks


########################################################################
# Example of getting mathematical characters on a plot

  plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", 
      main = expression(hat(Y) == hat(beta)[0] + hat(beta)[1]*x),
      xlim = c(0,4.5), ylim = c(0,4.5), col = "red", pch = 1, cex = 1.0, panel.first=grid(col = "gray", lty = "dotted"))
  # Draw a line from (x0, y0) to (x1, y1)
  segments(x0 = min(gpa$HS.GPA), y0 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*min(gpa$HS.GPA), 
           x1 = max(gpa$HS.GPA), y1 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*max(gpa$HS.GPA),
           lty = 1, col = "blue", lwd = 2)
 
  plot(x = gpa$HS.GPA, y = gpa$College.GPA, xlab = "HS GPA", ylab = "College GPA", 
      main = expression(paste("College GPA vs. HS GPA and ", widehat(College.GPA) == hat(beta)[0] + hat(beta)[1]*HS.GPA)),
      xlim = c(0,4.5), ylim = c(0,4.5), col = "red", pch = 1, cex = 1.0, panel.first=grid(col = "gray", lty = "dotted"))
  # Draw a line from (x0, y0) to (x1, y1)
  segments(x0 = min(gpa$HS.GPA), y0 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*min(gpa$HS.GPA), 
           x1 = max(gpa$HS.GPA), y1 = mod.fit$coefficients[1] + mod.fit$coefficients[2]*max(gpa$HS.GPA),
           lty = 1, col = "blue", lwd = 2)
           
  demo(plotmath) # Run this to see examples


########################################################################
# Open R Commander

  library(package = Rcmdr)

#
