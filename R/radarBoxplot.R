#'  Function to plot the Radar-Boxplot
#'
#' This function will plot the Radar-Boxplot
#'
#' @usage
#' #Method for formula
#' radarBoxplot(formula, data, plot.median=FALSE, col=c('red', 'blue'), ...)
#'
#' #Method for default
#' radarBoxplot(x, y, plot.median=FALSE, col=c('red', 'blue'), ...)
#'
#' @param formula a formula describing the class and attributes to use
#' @param x a data frame or matrix of attributes
#' @param y a response vector
#' @param plot.median boolean value to flag if median should be plotted, defaults to FALSE
#' @param col the colors to use for radar-boxplot, first color is the percentile 25-75\%
#' second is the total range and third color is the color for median line
#' @param use.ggplot2 if ggplot2, data.table and dplyr are available it will use ggplot for plotting. defaults to TRUE
#'
#' @examples
#' radarBoxplot(Species ~ ., iris)
#'
#' @export
"radarBoxplot" = function(x, ...) {
  UseMethod("radarBoxplot")
}



#' @export
"radarBoxplot.default" = function(x, y, plot.median=F, use.ggplot2=T, mfrow=NA, col=c('red', 'blue'), oma = c(5,4,0,0) + 0.1, mar=c(0,0,1,1) + 0.1) {
  if (length(col) != 2) {
    stop("Must provide at least two colors")
  }

  if (use.ggplot2)
    if (require("ggplot2") & require("dplyr") & require("data.table")) {
      nrows = NA
      if (length(mfrow) == 2) {
        nrows = mfrow[1]
      }
      return(radarGgplot2(x, y, plot.median = plot.median, col = col, nrows = nrows))
    }


  TAIL_THRESHOLD = 0.25

  # Standardize data to range between 0.1 to 1
  standardizedData=x
  mins=apply(standardizedData, 2, min)
  maxs=apply(standardizedData, 2, max)
  standardizedData=t((t(standardizedData)-mins)/(maxs-mins))*0.9+0.1

  #Get unique classes
  classes = sort(unique(y))

  #Calculate appropriate plot matrix size
  if (is.na(mfrow)) {
    classLength = length(classes)
    sqSize = ceiling(sqrt(classLength))

    if (sqSize*(sqSize-1) >= classLength)
      mfrow = c(sqSize-1, sqSize)
    else
      mfrow = c(sqSize, sqSize)
  }

  #Save previous values
  prevPars = par(c("mfrow", "oma", "mar"))
  par(mfrow=mfrow,
      oma = oma,
      mar = mar)


  nCols = dim(x)[2]+1
  #Calculate q25, q50 and q75
  q25 = aggregate(standardizedData, list(classes=y), quantile, .25)[,2:nCols]
  q75 = aggregate(standardizedData, list(classes=y), quantile, .75)[,2:nCols]
  if (plot.median) {
    q50 = aggregate(standardizedData, list(classes=y), quantile, .50)[,2:nCols]
  }

  #Remove outlier to calculate q0 and q100
  iqr = q75-q25
  outlier_min = q25 - 1.5*iqr
  outlier_max = q75 + 1.5*iqr
  reps_min=plyr::join(data.frame(classes=y), cbind(classes,outlier_min), by="classes")
  reps_max=plyr::join(data.frame(classes=y), cbind(classes,outlier_max), by="classes")
  mask = standardizedData<reps_min[,2:nCols] | standardizedData>reps_max[,2:nCols]
  masked = standardizedData
  masked[mask] = NA
  q0=aggregate(masked, list(classes=y), min, na.rm=TRUE)[, 2:nCols]
  q100=aggregate(masked, list(classes=y), max, na.rm=TRUE)[, 2:nCols]


  # Calculate angle alpha for transforming values
  # to coordinates for drawing lines and polygons
  nCols = dim(x)[2]
  nClasses = length(classes)
  alpha = (pi/2) + pi*2*0:(nCols-1)/nCols
  alphaClasses = rep((pi/2) + pi*2*0:(nCols-1)/nCols, each=nClasses)

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


  if (plot.median) {
    medX = cos(alphaClasses)*q50
    medY = sin(alphaClasses)*q50
    medX = cbind(medX,medX[,1])
    medY = cbind(medY,medY[,1])
  }

  for (classes_i in 1:length(classes)) {
    data2=x[1:2,]
    data2[1,]=0
    data2[2,]=1
    fmsb::radarchart(df=data2, lwd=4, maxmin=FALSE, pcol = rgb(1,0,0,0))
    title(main = classes[classes_i])

    scales::col2hcl(col[1])
    rgb2hsv(col2rgb(col[1]))
    col1 = col[1]
    col1_pale = scales::alpha(col1, 0.6)
    col2 =  col[2]
    col2_alph = scales::alpha(col2, 0.6)

    col1_alpha = scales::alpha(col[1], 0.5)
    polygon(c(x3[classes_i,], x2[classes_i,]), c(y3[classes_i,], y2[classes_i,]), col=col1_pale, border=NA, fillOddEven = TRUE)
    polygon(c(x4[classes_i,], x3[classes_i,]), c(y4[classes_i,], y3[classes_i,]), col=col2_alph, border=NA, fillOddEven = TRUE)
    polygon(c(x1[classes_i,], x2[classes_i,]), c(y1[classes_i,], y2[classes_i,]), col=col2_alph, border=NA, fillOddEven = TRUE)
    lines(x3[classes_i,], y3[classes_i,], col=col1, lwd=1.8)
    lines(x2[classes_i,], y2[classes_i,], col=col1, lwd=1.8)

    if (plot.median) {
      col3 = rgb(0,0,0)
      if (length(col) > 2)
        col3 = col[3]
      lines(medX[classes_i,], medY[classes_i,], col=col3)
    }

    class_name = classes[classes_i]
    outlierClassMask = outliers$classes==class_name
    points(outliers[outlierClassMask,"x"], outliers[outlierClassMask,"y"])
  }

  par(prevPars)
}


"radarGgplot2" = function(x, y, plot.median=F, nrows=NA, col=c('red', 'blue')) {
  classes = unique(y)

  if (is.na(nrows)) {
    nrows = ceiling(sqrt(length(classes)))
  }

  scale_zero_to_one <-
    function(x) {
      r <- range(x, na.rm = TRUE)
      min <- r[1]
      max <- r[2]
      ((x - min) / (max - min))
    }

  scaled.data <-
    x %>%
    lapply(scale_zero_to_one) %>%
    as.data.frame %>%
    cbind(classes=y) %>%
    melt(id.vars="classes")


  percentiles=scaled.data %>%
    group_by(classes, variable) %>% summarise_all(
      funs(
        q25=quantile(., probs=0.25),
        q75=quantile(., probs=0.75),
        q50=quantile(., probs=0.50))
    ) %>%
    mutate(iqr=1.5*(q75-q25)) %>%
    mutate(outlier_max=q75+1.5*iqr) %>%
    mutate(outlier_min=q25-1.5*iqr)

  without_outliers = scaled.data %>%
    inner_join(percentiles, by=c("classes", "variable")) %>%
    filter(value <= outlier_max & value >= outlier_min) %>%
    group_by(classes, variable) %>%
    summarise(q0=min(value)*0.9+0.1,
              q25=min(q25)*0.9+0.1,
              q50=min(q50)*0.9+0.1,
              q75=min(q75)*0.9+0.1,
              q100=max(value)*0.9+0.1
    ) %>%
    rbind(subset(., variable == names(x)[1]))

  polygons=rbind(without_outliers, without_outliers %>%
                   mutate(
                     q0=q25,
                     q25=q75,
                     q75=q100
                   ))

  outliers = scaled.data %>%
    inner_join(percentiles, by=c("classes", "variable")) %>%
    filter(value >= outlier_max | value <= outlier_min)


  # create new coord : inherit coord_polar
  coord_radar <-
    function(theta='x', start=0, direction=1){
      # input parameter sanity check
      match.arg(theta, c('x','y'))

      ggproto(
        "CoordRadar", CoordPolar,
        theta=theta, r=ifelse(theta=='x','y','x'),
        start=start, direction=sign(direction),
        is_linear=function() TRUE)
    }

  median=NULL
  if (plot.median) {
    median = geom_polygon(data=without_outliers, aes(x=variable, y=q50), color = 'black', fill=NA, size = 0.5)
  }

  return (polygons %>%
            ggplot(aes(x=variable, y=value, group=classes, colour=classes)) +
            geom_point(data=outliers, aes(x=variable, y=value), color="black", pch=1) +
            geom_polygon(aes(x=variable, y=q25, group = classes), color = NA, fill = col[1], size = 1, alpha=0.5) +
            geom_polygon(data=without_outliers, aes(x=variable, y=q25, group = classes), fill=NA, color = col[1], size = 1) +
            geom_polygon(data=without_outliers, aes(x=variable, y=q75, group = classes), fill=NA, color = col[1], size = 1) +
            geom_polygon(aes(x=variable, y=q75, group = classes), color = NA, fill = col[2], size = 1, alpha=0.5) +
            geom_polygon(aes(x=variable, y=q0, group = classes), color = NA, fill = col[2], size = 1, alpha=0.5) +
            median +
            coord_radar() +
            scale_y_continuous(limits=c(0, 1), breaks=c(0,0.28, 0.56, 0.84)) +
            facet_wrap(~ classes, nrow=nrows) +
            theme_bw() +
            theme(
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              panel.border = element_blank(),
              legend.position = 'none'))
}



radarBoxplotError = function(data, factorVarName, observedPredicted, plot.median=F, first=100) {
  standardizedData = data
  colNames = colnames(standardizedData)
  colNames = colNames[colNames!=factorVarName]
  standardizedData=standardizedData[,colNames]
  mins=apply(standardizedData, 2, min)
  maxs=apply(standardizedData, 2, max)

  standardizedData[,colNames]=t((t(standardizedData)-mins)/(maxs-mins))*0.9+0.1
  TAIL_THRESHOLD = 0.25



  classes = unique(data[,factorVarName])
  par(mfrow=c(1,2),
      oma = c(5,4,0,0) + 0.1,
      mar = c(0,0,1,1) + 0.1)

  colNames = colnames(standardizedData)
  colNames = colNames[colNames!=factorVarName]
  ncols = length(colNames)
  data2=standardizedData[1:2,colNames]
  data2[1,]=0
  data2[2,]=1
  alpha = (pi/2) + pi*2*0:(ncols-1)/ncols
  radarData=list()
  for (class_i in classes) {
    dataClass=standardizedData[data[,factorVarName]==class_i,]

    q0 = sapply(dataClass[,colNames], perc0)
    q25 = sapply(dataClass[,colNames], perc25)
    q50 = sapply(dataClass[,colNames], perc50)
    q75 = sapply(dataClass[,colNames], perc75)
    q100 = sapply(dataClass[,colNames], perc100)
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
  i = 0
  for (class_i in classes) {
    for (erro in rownames(observedPredicted[observedPredicted[,2]==class_i & observedPredicted[,1]!=class_i, ])) {
      recordError=standardizedData[erro,colNames]
      observed=observedPredicted[erro,1]
      polX=cos(alpha)*recordError
      polY=sin(alpha)*recordError
      polX=c(polX, polX[1])
      polY=c(polY, polY[1])
      fmsb::radarchart(df=data2,
                       lwd=4, maxmin=FALSE, pcol = rgb(1,0,0,0))
      title(main = paste("Obs: ", observed, sep=""))
      observedData=radarData[[observed]]
      addRadarPolygons (observedData$x1, observedData$x2, observedData$x3, observedData$x4, observedData$y1, observedData$y2, observedData$y3, observedData$y4, observedData$xOut, observedData$yOut, observedData$medX, observedData$medY, plot.median=plot.median)
      lines(polX, polY, lwd=2, lty = "longdash", col="#9A6324")

      fmsb::radarchart(df=data2,
                       lwd=4, maxmin=FALSE, pcol = rgb(1,0,0,0))
      title(main = paste("Pred: ", class_i, sep=""))
      observedData=radarData[[class_i]]
      addRadarPolygons (observedData$x1, observedData$x2, observedData$x3, observedData$x4, observedData$y1, observedData$y2, observedData$y3, observedData$y4, observedData$xOut, observedData$yOut, observedData$medX, observedData$medY, plot.median=plot.median)
      lines(polX, polY, lwd=2, lty = "longdash", col="#9A6324")
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
