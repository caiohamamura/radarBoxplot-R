

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
addRadarPolygons = function(x1, x2, x3, x4, y1, y2, y3, y4, xOut, yOut, medX="", medY="", plot.median=F, col=c('red', 'blue')) {

  polygon(c(x3, x2), c(y3, y2), col=col[1], border=NA, fillOddEven = TRUE)
  polygon(c(x4, x3), c(y4, y3), col=col[2], border=NA, fillOddEven = TRUE)
  polygon(c(x1, x2), c(y1, y2), col=col[2], border=NA, fillOddEven = TRUE)
  polygon(x3, y3, col=rgb(0,0,0,0), lwd=1, border=col[3], fillOddEven = TRUE)
  polygon(x2, y2, col=rgb(0,0,0,0), lwd=1, border=col[3], fillOddEven = TRUE)
  points(xOut, yOut)
  if (plot.median)
    lines(medX, medY, col=rgb(0,0,0,1))
}

parseFormula = function(x, data) {
  ### formula interface for radarBoxplot
  ### code gratefully stolen from randomForest.formula (package randomForest).
  ###
  if (!inherits(x, "formula"))
    stop("method is only for formula objects")
  m <- match.call(expand.dots = FALSE)
  ## Catch xtest and ytest in arguments.
  if (any(c("xtest", "ytest") %in% names(m)))
    stop("xtest/ytest not supported through the formula interface")
  names(m)[2] <- "formula"
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval(m, parent.frame())

  y <- model.response(m)
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0
  ## Drop any "negative" terms in the formula.
  m <- model.frame(terms(reformulate(attributes(Terms)$term.labels)),
                   data.frame(m))
  ## if (!is.null(y)) m <- m[, -1, drop=FALSE]
  for (i in seq(along=m)) {
    if (is.ordered(m[[i]])) m[[i]] <- as.numeric(m[[i]])
  }
  return (list(m, y))
}

standardizeData = function(x) {
  standardizedData=x
  mins=apply(standardizedData, 2, min)
  maxs=apply(standardizedData, 2, max)
  standardizedData=t((t(standardizedData)-mins)/(maxs-mins))*0.9+0.1
  return (standardizedData)
}

drawRing = function(xOut, yOut, xIn, yIn, ...) {
  args = list(...)
  defaultArgs = list(border = NA)
  defaultArgs[names(args)] = args

  numPoints = length(xOut)
  halfNum = floor(numPoints/2)
  firstHalf = 1:halfNum
  secondHalf = halfNum:numPoints

  defaultArgs["x"] = list(c(xOut[firstHalf], xIn[rev(firstHalf)]))
  defaultArgs["y"] = list(c(yOut[firstHalf], yIn[rev(firstHalf)]))
  do.call(polygon, defaultArgs)

  defaultArgs["x"] = list(c(xOut[secondHalf], xIn[rev(secondHalf)]))
  defaultArgs["y"] = list(c(yOut[secondHalf], yIn[rev(secondHalf)]))
  do.call(polygon, defaultArgs)
}

# Merge a list of arguments arguments provided in ellipsis
mergeListArgs = function(args, ...) {
  toMerge = list(...)
  args[names(toMerge)] = toMerge
  return(args)
}

# Creates an empty radar chart
emptyRadarPlot = function(nAxis, axisNames,
                          angleOffset=0,
                          nTicks = 4,
                          ticksArgs = list(),
                          axisArgs = list(),
                          labelsArgs = list()){

  # Load default settings and replace
  defaultTicks = list(
    col="#DDDDDD"
  )
  if (is.list(ticksArgs)) {
    defaultTicks[names(ticksArgs)] = ticksArgs
  }

  defaultAxis = list(
    col="#DDDDDD",
    lty='dashed'
  )
  if (is.list(axisArgs)) {
    defaultAxis[names(axisArgs)] = axisArgs
  }

  defaultLabels = list(
    col="#444444",
    lty='dashed'
  )
  if (is.list(labelsArgs)) {
    defaultLabels[names(labelsArgs)] = labelsArgs
  }


  # Outer radius for the plot
  rOuter = 1.2

  # Initialize empty plot
  plot(c(-rOuter, rOuter), c(-rOuter, rOuter), type="n", frame.plot=FALSE, axes=FALSE,
       xlab="", ylab="", asp=1)

  # Center of plot
  center = c(0,0)

  # Radius for the radar
  r = 1
  # Radius of the ticks
  rs = t(seq(0, r, length.out = nTicks + 1)[-1])

  # Create angles sequence for tick lines
  angleStep = pi/180/6
  maxAngle = pi*360/180
  angles = seq(0, maxAngle, angleStep)

  # Create x y points for tick lines
  y = apply(rs, 2, crossprod, y=sin(angles))
  x = apply(rs, 2, crossprod, y=cos(angles))
  for (i in 1:nTicks) {
    do.call(lines,mergeListArgs(defaultTicks, x=x[,i], y=y[,i]))
  }

  # Axis lines
  angles = seq(maxAngle, 0, length.out = nAxis+1)[-nAxis-1]
  offset = (90*pi/180) - angleOffset
  angles = angles + offset
  y = (sin((angles))*r)
  x = (cos((angles))*r)
  for (i in 1:nAxis) {
    do.call(lines, mergeListArgs(defaultAxis, x=c(0,x[i]), y=c(0,y[i])))
  }

  # Labels
  y = (sin((angles))*rOuter)
  x = (cos((angles))*rOuter)

  for (i in 1:nAxis) {
    do.call(text, mergeListArgs(defaultLabels, x=x[i], y=y[i], labels=axisNames[i]))
  }
}
