

#' @importFrom stats quantile
perc25 = function(x) {
  return(quantile(x, probs = (0.25), na.rm = T))
}

#' @importFrom stats quantile
perc50 = function(x) {
  return(quantile(x, probs = (0.50), na.rm = T))
}

#' @importFrom stats quantile
perc75 = function(x) {
  return(quantile(x, probs = (0.75), na.rm = T))
}

#' @importFrom stats quantile
perc100 = function(x) {
  return(quantile(x, probs = (1), na.rm = T))
}


#' @importFrom stats quantile
perc0 = function(x) {
  return(quantile(x, probs = (0), na.rm = T))
}


# It adds radar polygons based on (x1,y1)...(x4, y4) pairs of points
# to build up the polygons for Radar-boxplot
# (xOut, yOut) are outliers points
# (medX, medY) are the median
#' @importFrom graphics polygon lines points
#' @importFrom grDevices rgb
addRadarPolygons = function(x1, x2, x3, x4, y1, y2, y3, y4, xOut, yOut, medX="", medY="", plot.median=F) {

  polygon(c(x3, x2), c(y3, y2), col=rgb(1,0.5,0.5,1), border=NA, fillOddEven = TRUE)
  polygon(c(x4, x3), c(y4, y3), col=rgb(0,0,1,.4), border=NA, fillOddEven = TRUE)
  polygon(c(x1, x2), c(y1, y2), col=rgb(0,0,1,.4), border=NA, fillOddEven = TRUE)
  polygon(x3, y3, col=rgb(0,0,0,0), lwd=1, border='red', fillOddEven = TRUE)
  polygon(x2, y2, col=rgb(0,0,0,0), lwd=1, border='red', fillOddEven = TRUE)
  points(xOut, yOut)
  if (plot.median)
    lines(medX, medY, col=rgb(0,0,0,1))
}

#' @importFrom utils installed.packages
is.installed = function(mypkg) is.element(mypkg, installed.packages()[,1])
