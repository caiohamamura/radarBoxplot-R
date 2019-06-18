#'  Function to plot the radar-boxplot
#'
#' @aliases radarBoxplot.default radarBoxplot.formula
#'
#' @rdname radarBoxplot
#'
#' @param x a data frame or matrix of attributes or a formula describing the
#' attributes for the class
#' @param y a response vector
#' @param data dataset for fomula variant for which formula was defined
#' @param plot.median boolean value to flag if median should be plotted: Default FALSE
#' @param use.ggplot2 if ggplot2 are available it will use ggplot for plotting: Default FALSE
#' @param mfrow mfrow argument for defining the subplots nrows and ncols: Default will calculate the minimum square
#' @param oma outer margins of the subplots: Default c(5,4,0,0) + 0.1
#' @param mar margins of the subplots: Default c(0,0,1,1) + 0.1
#' @param innerPolygon a list of optional arguments to override Q2-Q3 `graphics::polygon()` style: Default list()
#' @param outerPolygon a list of optional arguments to override the outer (range) `graphics::polygon()` default style: Default list()
#' @param innerBorder a list of optional arguments to override the inner border `graphics::lines()` default style: Default list()
#' @param outerBorder a list of optional arguments to override the outer border `graphics::lines()` default style: Default list()
#' @param medianLine a list of optional arguments to override the median line `graphics::lines()` default style: Default list()
#' @param outlierPoints a list of optional arguments to override the outliers `graphics::points()` default style: Default list()
#' @param angleOffset offset for rotating the plots: Default will let the top free of axis to avoid its label overlapping the title
#' @param nTicks number of ticks for the radar chart: Default 4
#' @param ticksArgs a list of optional arguments to override radar ticks `graphics::lines()` default style: Default list()
#' @param axisArgs a list of optional arguments to override radar axis `graphics::lines()` default style: Default list()
#' @param labelsArgs a list of optional arguments to override labels `graphics::text()` default style: Default list()
#' @param ... parameter to allow the usage of S3 methods
#'
#' @examples
#' library(radarBoxplot)
#' data("winequality_red")
#'
#' # Regular
#' radarBoxplot(quality ~ ., winequality_red)
#'
#' # Orange and green pattern with grey median
#' radarBoxplot(quality ~ ., winequality_red,
#'              use.ggplot2=FALSE, medianLine=list(col="grey"),
#'              innerPolygon=list(col="#FFA500CC"),
#'              outerPolygon=list(col=rgb(0,.7,0,0.6)))
#'
#' # Plot in 2 rows and 3 columns
#' # change columns order (counter clockwise)
#' radarBoxplot(quality ~ volatile.acidity + citric.acid +
#'              residual.sugar + fixed.acidity + chlorides +
#'              free.sulfur.dioxide + total.sulfur.dioxide +
#'              density + pH + sulphates + alcohol,
#'              data = winequality_red,
#'              mfrow=c(2,3))
#'
#' @export
`radarBoxplot` = function(x, ...) {
  UseMethod("radarBoxplot")
}

#' @rdname radarBoxplot
#' @export
"radarBoxplot.formula" = function(x, data, ...) {
  results = parseFormula(x, data)
  radarBoxplot.default(results[[1]], results[[2]], ...)
}

#' @import graphics grDevices stats
#' @rdname radarBoxplot
#' @export
`radarBoxplot.default` = function(x, y, plot.median=FALSE,
                                  use.ggplot2=FALSE, mfrow=NA,
                                  oma = c(5,4,0,0) + 0.1,
                                  mar=c(0,0,1,1) + 0.1,
                                  innerPolygon = list(),
                                  outerPolygon = list(),
                                  innerBorder = list(),
                                  outerBorder = list(),
                                  medianLine = list(),
                                  outlierPoints = list(),
                                  nTicks = 4,
                                  ticksArgs = list(),
                                  axisArgs = list(),
                                  labelsArgs = list(),
                                  angleOffset = NA,
                                  ...) {

  TAIL_THRESHOLD = 0.25

  # Standardize data to range between 0.1 to 1
  standardizedData=standardizeData(x)

  #Get unique classes
  factorY = factor(y)
  classes = levels(factorY)

  #Calculate appropriate plot matrix size
  if (anyNA(mfrow)) {
    classLength = length(classes)
    sqSize = ceiling(sqrt(classLength))

    if (sqSize*(sqSize-1) >= classLength)
      mfrow = c(sqSize-1, sqSize)
    else
      mfrow = c(sqSize, sqSize)
  }

  nCols = dim(x)[2]+1

  # Calculate q25, q50 and q75
  q25 = aggregate(standardizedData, list(classes=y), quantile, .25)[,2:nCols]
  q75 = aggregate(standardizedData, list(classes=y), quantile, .75)[,2:nCols]
  q50 = aggregate(standardizedData, list(classes=y), quantile, .50)[,2:nCols]


  #Remove outlier to calculate q0 and q100
  iqr = q75-q25
  outlier_min = q25 - 1.5*iqr
  outlier_max = q75 + 1.5*iqr

  reps_min = data.frame(classes=y, outlier_min[as.numeric(factorY),])
  reps_max = data.frame(classes=y, outlier_max[as.numeric(factorY),])
  mask = standardizedData<reps_min[,2:nCols] | standardizedData>reps_max[,2:nCols]
  standardizedData[!mask]
  masked = standardizedData
  masked[mask] = NA
  q0=aggregate(masked, list(classes=y), min, na.rm=TRUE)[, 2:nCols]
  q100=aggregate(masked, list(classes=y), max, na.rm=TRUE)[, 2:nCols]


  # Calculate angle alpha for transforming values
  # to coordinates for drawing lines and polygons
  nCols = dim(x)[2]
  attributes = colnames(x)
  nClasses = length(classes)
  if (is.na(angleOffset)) {
    angleOffset = pi/nCols
  }
  alpha = ((pi/2) - pi*2*0:(nCols-1)/nCols) - angleOffset
  alphaClasses = (rep((pi/2) - pi*2*0:(nCols-1)/nCols, each=nClasses)) - angleOffset

  # Get points for outliers
  nRows = dim(x)[1]
  indices=0:length(mask)
  maskedIndices=indices[as.vector(mask)]
  colsOutliers = floor(maskedIndices / nRows) + 1
  rowsOutliers = (maskedIndices %% nRows) + 1

  outliersAlpha=alpha[colsOutliers]
  dataOutliers = standardizedData[mask]
  xOut = cos(outliersAlpha)*dataOutliers
  yOut = sin(outliersAlpha)*dataOutliers
  outliers = data.frame(classes=y[rowsOutliers], x=xOut, y=yOut)

  if (use.ggplot2) {
    res = system.file(package="ggplot2") != ""
    if (res) {
      nrows = NA
      if (length(mfrow) == 2) {
        nrows = mfrow[1]
      }

      outliers$variable = attributes[colsOutliers]
      outliers$value = dataOutliers

      return(radarGgplot2(x, y, outliers, q0, q25, q50, q75, q100,
                          nrows = nrows,
                          innerPolygon = innerPolygon,
                          outerPolygon = outerPolygon,
                          innerBorder = innerBorder,
                          outerBorder = outerBorder,
                          medianLine = medianLine,
                          outlierPoints = outlierPoints,
                          angleOffset = angleOffset))
    }
  }

  # Save previous values to restore afterwards
  prevPars = par(c("mfrow", "oma", "mar"))
  par(mfrow=mfrow,
      oma = oma,
      mar = mar)

  # Transform values into coordinates
  x1 = cos(alphaClasses)*q0
  y1 = sin(alphaClasses)*q0
  x2 = cos(alphaClasses)*q25
  y2 = sin(alphaClasses)*q25
  x3 = cos(alphaClasses)*q75
  y3 = sin(alphaClasses)*q75
  x4 = cos(alphaClasses)*q100
  y4 = sin(alphaClasses)*q100

  # Replicate last coordinate to close polygons
  x1 = cbind(x1,x1[,1])
  y1 = cbind(y1,y1[,1])
  x2 = cbind(x2, x2[,1])
  y2 = cbind(y2, y2[,1])
  x3 = cbind(x3,x3[,1])
  y3 = cbind(y3,y3[,1])
  x4 = cbind(x4,x4[,1])
  y4 = cbind(y4,y4[,1])

  medX = cos(alphaClasses)*q50
  medY = sin(alphaClasses)*q50
  medX = cbind(medX,medX[,1])
  medY = cbind(medY,medY[,1])

  data2=x[1:2,]
  data2[1,]=0
  data2[2,]=1
  for (classes_i in 1:length(classes)) {
    # Plot empty radarchart
    emptyRadarPlot(nCols, attributes, angleOffset = angleOffset,
                   ticksArgs = ticksArgs,
                   axisArgs = axisArgs,
                   labelsArgs = labelsArgs,
                   nTicks = nTicks)
    title(main = classes[classes_i])

    # Default args for polygons
    defaultInnerPolygonArgs = list(
      xOut = x3[classes_i,],
      xIn =  x2[classes_i,],
      yOut = y3[classes_i,],
      yIn = y2[classes_i,],
      col=grDevices::rgb(1,0,0,0.6),
      border=NA,
      fillOddEven = TRUE
    )

    defaultOuterPolygonArgs = list(
      xOut = x4[classes_i,],
      xIn = x3[classes_i,],
      yOut = y4[classes_i,],
      yIn = y3[classes_i,],
      col = grDevices::rgb(0,0,1,0.6),
      border = NA
    )

    # Replace with provided args
    if (is.list(innerPolygon)) {
      defaultInnerPolygonArgs[names(innerPolygon)] = innerPolygon
    }
    if (is.list(outerPolygon)) {
      defaultOuterPolygonArgs[names(outerPolygon)] = outerPolygon
    }

    defaultInnerBorderArgs = list(
      x = x3[classes_i,],
      y = y3[classes_i,],
      col = paste(substr(defaultInnerPolygonArgs$col, 1, 7), "FF", sep=""),
      lwd = 1.8
    )
    if (is.list(innerBorder)) {
      defaultInnerBorderArgs[names(innerBorder)] = innerBorder
    }


    # Plot polygons
    do.call(drawRing, defaultInnerPolygonArgs)
    do.call(drawRing, defaultOuterPolygonArgs)
    defaultOuterPolygonArgs[c("xIn","xOut","yIn", "yOut")] = list(
      x1[classes_i,],
      x2[classes_i,],
      y1[classes_i,],
      y2[classes_i,])
    do.call(drawRing, defaultOuterPolygonArgs)

    # Plot inner borders
    do.call(lines, defaultInnerBorderArgs)
    defaultInnerBorderArgs[c("x", "y")] = list(x2[classes_i,], y2[classes_i,])
    do.call(lines, defaultInnerBorderArgs)

    # Plot outer borders
    defaultOuterBorderArgs = list(
      x = x1[classes_i,],
      y = y1[classes_i,],
      col = NA
    )
    if (is.list(outerBorder)) {
      defaultOuterBorderArgs[names(outerBorder)] = outerBorder

      do.call(lines, defaultOuterBorderArgs)
      defaultOuterBorderArgs[c("x", "y")] = list(x4[classes_i,], y4[classes_i,])
      do.call(lines, defaultOuterBorderArgs)
    }

    # Default args
    defaultMedianLineArgs = list(
      x = medX[classes_i,],
      y = medY[classes_i,],
      col="white",
      lty="dashed"
    )
    # Replace with defined args
    if (is.list(medianLine)) {
      defaultMedianLineArgs[names(medianLine)] = medianLine
    }

    # Plot median lines
    do.call(lines, defaultMedianLineArgs)

    class_name = classes[classes_i]
    outlierClassMask = outliers$classes==class_name
    defaultPointArgs = list(
      x = outliers[outlierClassMask,"x"],
      y = outliers[outlierClassMask,"y"]
    )
    if (is.list(outlierPoints)) {
      defaultPointArgs[names(outlierPoints)] = outlierPoints
    }

    do.call(points, defaultPointArgs)
  }

  par(prevPars)
}

"radarGgplot2" = function(x, y, outliers, q0, q25, q50,
                          q75, q100,
                          nrows=NA,
                          innerPolygon = list(),
                          outerPolygon = list(),
                          innerBorder = list(),
                          outerBorder = list(),
                          medianLine = list(),
                          outlierPoints = list(),
                          angleOffset = 0) {

    variable = value = pol = NA

  if (!requireNamespace("ggplot2")) {
    return()
  }

  if (is.na(nrows)) {
    nrows = ceiling(sqrt(length(classes)))
  }

  classes = levels(as.factor(y))
  attributes = names(x)
  nAttributes = length(attributes)
  halfAttr = ceiling(nAttributes/2)
  firstHalfAttr = attributes[1:halfAttr]
  secondHalfAttr = c(attributes[halfAttr:nAttributes])
  angleOffset = angleOffset-(pi/nAttributes)


  df= data.frame(
    classes=rep(classes, each=length(attributes)),
    variable = rep(attributes, length(c)),
    q0=NA,
    q25=NA,
    q50=NA,
    q75=NA,
    q100=NA
  )

  for (i in attributes) {
    df[df$variable==i,3:7] = cbind(q0[i], q25[i], q50[i], q75[i], q100[i])
  }

  cols1 = c("classes", "variable", "q0", "q25", "q75")
  cols2 = c("classes", "variable", "q25", "q75", "q100")
  df2 = df[df$variable %in% firstHalfAttr,cols1]
  nRowdf2 = nrow(df2)
  df2 = rbind(df2, setNames(df[df$variable %in% firstHalfAttr,cols2], names(df2))[seq(nRowdf2, 1, -1),])

  df3 = df[df$variable %in% secondHalfAttr,cols1]
  nRowdf3 = nrow(df3)
  attribute1 = df$variable == attributes[1]
  rbind0 = df[df$variable == attributes[1],cols1]
  rbind1 = setNames(df[attribute1,cols2][sum(attribute1):1,], names(df3))
  rbind2 = setNames(df[df$variable %in% secondHalfAttr,cols2], names(df3))[seq(nRowdf3, 1, -1),]
  df3 = rbind(df3, rbind0, rbind1, rbind2)
  nrows=2
  df$variable = factor(df$variable, levels=attributes)
  df2$variable = factor(df2$variable, levels=attributes)
  df3$variable = factor(df3$variable, levels=attributes)
  df2$pol = 1
  df3$pol = 2
  df4=df[df$variable %in% attributes[c(1, nAttributes)],]

  # create new coord : inherit coord_polar
  coord_radar <-
    function(theta='x', start=angleOffset, direction=1){
      # input parameter sanity check
      match.arg(theta, c('x','y'))

      ggplot2::ggproto(
        "CoordRadar", ggplot2::CoordPolar,
        theta=theta, r=ifelse(theta=='x','y','x'),
        start=start, direction=sign(direction),
        is_linear=function() TRUE)
    }

  aes = ggplot2::aes
  geom_point = ggplot2::geom_point
  geom_polygon = ggplot2::geom_polygon
  geom_line = ggplot2::geom_line
  scale_y_continuous = ggplot2::scale_y_continuous
  element_blank = ggplot2::element_blank


  defaultInnerPolArgs = list(
    fill = grDevices::rgb(1,0,0,0.6), col = NA
  )
  if (is.list(innerPolygon)) {
    defaultInnerPolArgs[names(innerPolygon)] = innerPolygon
  }

  defaultOuterPolArgs = list(
    fill = grDevices::rgb(0,0,1,0.6), col = NA
  )
  if (is.list(outerPolygon)) {
    defaultOuterPolArgs[names(outerPolygon)] = outerPolygon
  }

  defaultInnerBorderArgs = list(
    col=paste(substr(defaultInnerPolArgs$fill, 1, 7), "FF", sep="")
  )
  if (is.list(innerBorder)) {
    defaultInnerBorderArgs[names(innerBorder)] = innerBorder
  }

  defaultOuterBorderArgs = list(
    col=rgb(0,0,0,0)
  )
  if (is.list(outerBorder)) {
    defaultOuterBorderArgs[names(outerBorder)] = outerBorder
  }

  defaultMedianLineArgs = list(
    col=rgb(0,0,0,0)
  )
  if (is.list(medianLine)) {
    defaultMedianLineArgs[names(medianLine)] = medianLine
  }

  argPoints = list(
    data = outliers,
    mapping = aes(x=variable, y=value, group=classes),
    pch=1
  )
  if (is.list(outlierPoints)) {
    argPoints[names(outlierPoints)] = outlierPoints
  }

  return(ggplot2::ggplot(rbind(df2, df3)) +
            # Plot inner polygon
            do.call(geom_polygon, mergeListArgs(defaultInnerPolArgs, mapping=aes(x=variable, y=q25, group = pol))) +

            # Plot outer polygon
            do.call(geom_polygon, mergeListArgs(defaultOuterPolArgs, mapping=aes(x=variable, y=q0, group = pol))) +
            do.call(geom_polygon, mergeListArgs(defaultOuterPolArgs, mapping=aes(x=variable, y=q75, group = pol))) +

            # Plot inner border
            do.call(geom_line, mergeListArgs(defaultInnerBorderArgs, data=df, mapping=aes(x=variable, y=q25, group = classes))) +
            do.call(geom_line, mergeListArgs(defaultInnerBorderArgs, data=df4, mapping=aes(x=variable, y=q25, group = classes))) +
            do.call(geom_line, mergeListArgs(defaultInnerBorderArgs, data=df, mapping=aes(x=variable, y=q75, group = classes))) +
            do.call(geom_line, mergeListArgs(defaultInnerBorderArgs, data=df4, mapping=aes(x=variable, y=q75, group = classes))) +

            # Plot outer border
            do.call(geom_line, mergeListArgs(defaultOuterBorderArgs, data=df, mapping=aes(x=variable, y=q0, group = classes))) +
            do.call(geom_line, mergeListArgs(defaultOuterBorderArgs, data=df4, mapping=aes(x=variable, y=q0, group = classes))) +
            do.call(geom_line, mergeListArgs(defaultOuterBorderArgs, data=df, mapping=aes(x=variable, y=q100, group = classes))) +
            do.call(geom_line, mergeListArgs(defaultOuterBorderArgs, data=df4, mapping=aes(x=variable, y=q100, group = classes))) +

            # Plot median line
            do.call(geom_line, mergeListArgs(defaultMedianLineArgs, data=df, mapping=aes(x=variable, y=q50, group = classes))) +
            do.call(geom_line, mergeListArgs(defaultMedianLineArgs, data=df4, mapping=aes(x=variable, y=q50, group = classes))) +

            # Plot outliers
            do.call(geom_point, argPoints) +
            coord_radar() +
            scale_y_continuous(limits=c(0, 1), breaks=c(0,0.28, 0.56, 0.84)) +
            ggplot2::facet_wrap(~ classes, nrow=nrows) +
            ggplot2::theme_bw() +
            ggplot2::theme(
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              panel.border = element_blank(),
              legend.position = 'none'))
}


# Function to compare misclassified polygons
# observed/predicted over the radar-boxplot
#
# @rdname radarBoxplotError
#
# @param x a data frame or matrix of attributes or a formula describing the
# attributes for the class
# @param y a response vector
# @param data dataset for fomula variant for which formula was defined
# @param observedPredicted a data frame with observed and predicted labels
# @param first the number of plots to emmit at once
# @param plot.median boolean value to flag if median should be plotted, defaults to FALSE
# @param col the colors to use for radar-boxplot, first color is the percentile 25-75\%
# second is the total range and third color is the color for the misclassified feature
# @param ... optional parameters to be passed to the function `radarBoxplot.default`
#
radarBoxplotError = function(x, ...) {
  UseMethod("radarBoxplotError")
}

# @rdname radarBoxplotError
"radarBoxplotError.formula" = function(x, data, ...) {
  results = parseFormula(x, data)
  radarBoxplotError.default(results[[1]], results[[2]], ...)
}

# @importFrom grDevices rgb
# @rdname radarBoxplotError
radarBoxplotError.default = function(x, y, observedPredicted, first=100, plot.median=FALSE, col=c('red', 'blue', '#9A6324'), ...) {
  if (length(col) < 3) {
    stop("Must provide at least three colors")
  }
  standardizedData = x
  colNames = colnames(x)
  mins = apply(standardizedData, 2, min)
  maxs = apply(standardizedData, 2, max)

  standardizedData = t((t(standardizedData)-mins)/(maxs-mins))*0.9+0.1
  TAIL_THRESHOLD = 0.25



  classes = unique(y)
  par(mfrow=c(1,2),
      oma = c(5,4,0,0) + 0.1,
      mar = c(0,0,1,1) + 0.1)

  colNames = colnames(standardizedData)

  ncols = length(colNames)
  data2=standardizedData[1:2,colNames]
  data2[1,]=0
  data2[2,]=1
  alpha = (pi/2) + pi*2*0:(ncols-1)/ncols
  radarData=list()
  for (class_i in classes) {
    dataClass=standardizedData[y==class_i,]

    q0 = sapply(dataClass[,colNames], min)
    q25 = sapply(dataClass[,colNames], quantile, probs=0.25)
    q50 = sapply(dataClass[,colNames], median)
    q75 = sapply(dataClass[,colNames], quantile, probs=0.75)
    q100 = sapply(dataClass[,colNames], max)
    iqr = q75 - q25
    outlier_max = q75+1.5*iqr
    outlier_min = q25-1.5*iqr

    dataClass=dataClass[,colNames]
    test_outlier = outlier_min <= q0
    outlier_min[test_outlier] = q0[test_outlier]
    test_outlier = outlier_max >= q100
    outlier_max[test_outlier] = q100[test_outlier]



    x1 = cos(alpha)*outlier_min
    y1 = sin(alpha)*outlier_min
    x1 = c(x1, x1[1])
    y1 = c(y1, y1[1])
    x2 = cos(alpha)*q25
    y2 = sin(alpha)*q25
    x2 = c(x2, x2[1])
    y2 = c(y2, y2[1])
    x3 = cos(alpha)*q75
    y3 = sin(alpha)*q75
    x4 = cos(alpha)*outlier_max
    y4 = sin(alpha)*outlier_max
    x3 = c(x3,x3[1])
    y3 = c(y3,y3[1])
    x4 = c(x4,x4[1])
    y4 = c(y4,y4[1])
    medX = cos(alpha)*q50
    medY = sin(alpha)*q50
    medX = c(medX,medX[1])
    medY = c(medY,medY[1])


    outliers=dataClass[,colNames]<sapply(outlier_min, rep, dim(dataClass)[1]) | dataClass[,colNames]>sapply(outlier_max, rep, dim(dataClass)[1])


    outlierNa = outliers
    outlierNa[!outlierNa] = NA
    dataOutliers=dataClass[,colNames]*(outlierNa)
    cosAlpha=sapply(cos(alpha), rep, dim(dataClass)[1])
    sinAlpha=sapply(sin(alpha), rep, dim(dataClass)[1])
    xOut = cosAlpha*dataOutliers
    yOut = sinAlpha*dataOutliers
    xOut = xOut[outliers]
    yOut = yOut[outliers]

    radarData[[class_i]] = list(x1=x1, x2=x2, x3=x3, x4=x4, y1=y1, y2=y2, y3=y3, y4=y4, xOut=xOut, yOut=yOut, medX=medX, medY=medY)
  }

  # Define colors
  col2_alpha = paste(col[2], "99", sep="")
  col1_alpha = paste(col[1], "99", sep="")

  i = 0
  for (class_i in classes) {
    for (erro in rownames(observedPredicted[observedPredicted[,2]==class_i & observedPredicted[,1]!=class_i, ])) {
      recordError=standardizedData[erro,colNames]
      observed=observedPredicted[erro,1]
      polX=cos(alpha)*recordError
      polY=sin(alpha)*recordError
      polX=c(polX, polX[1])
      polY=c(polY, polY[1])
      emptyRadarPlot(ncols, colNames)
      title(main = paste("Obs: ", observed, sep=""))
      observedData=radarData[[observed]]
      addRadarPolygons (observedData$x1, observedData$x2, observedData$x3,
                        observedData$x4, observedData$y1, observedData$y2,
                        observedData$y3, observedData$y4, observedData$xOut,
                        observedData$yOut, observedData$medX,
                        observedData$medY, plot.median=plot.median,
                        col=c(col1_alpha, col2_alpha, col[1]))
      lines(polX, polY, lwd=2, lty = "longdash", col=col[3])

      emptyRadarPlot(ncols, colNames)
      title(main = paste("Pred: ", class_i, sep=""))
      observedData=radarData[[class_i]]
      addRadarPolygons (observedData$x1, observedData$x2, observedData$x3, observedData$x4, observedData$y1, observedData$y2, observedData$y3, observedData$y4, observedData$xOut, observedData$yOut, observedData$medX, observedData$medY, plot.median=plot.median)
      lines(polX, polY, lwd=2, lty = "longdash", col=col[3])
      i = i + 1
      val=readline(prompt=paste("Plotted instance ", erro, ", press [enter] to continue, or type c then [enter] to cancel: ", sep = ""))
      if (val == "c") {
        return()
      }
      if (i == first) {
        return()
      }
    }
  }
}
