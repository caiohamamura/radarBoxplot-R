# Radar-Boxplot

This package provides the implementation of the radar-boxplot, a charting technique create and developed by the author.

## Installation

Currently the installation process is through devtools.

```r
# install.packages("devtools")
devtools::install_github("caiohamamura/radarBoxplot-R")
```

## Usage

There are two variants of the function that accepts two different sets of arguments as input. 

### Default
 - `x`: numerical data frame or matrix of quantitative variables/ordinal categorical variables.
 - `y`: vector of labels/factors 

### Formula
 - `x`: formula describing the relationship between the categorical target variable and the descriptive numerical variables.
 - `data`: the data frame which the formula refers to.
 
### Additional optional arguments
 - `plot.median`: boolean value to flag if median should be plotted, defaults to FALSE
 - `use.ggplot2`: boolean argument. If ggplot, data.table and dplyr are available it will use ggplot for plotting. defaults to TRUE
 - `mfrow`: mfrow argument for defining the subplots nrows and ncols
 - `col`: the colors to use for radar-boxplot, first color is the percentile 25-75% second is the total range and third color is the color for median line
 - `oma`: outer margins of the subplots
 - `mar`: margins of the subplots

## Description

It merges the concepts of both radar chart and the boxplot chart, allowing to compare multivariate data for multiple classes/clusters at a time. It provides a intuitive understanding over the data by creating radar polygons which can be compared in terms of shape and thickness, giving a meaningful insight towards identifying high inner variation and similar classes/clusters.

By interpreting the radar-boxplot, it is possible to predict classification confusion over classes and understand why and what could be done to achieve better results.

The radar-boxplot draws two different regions colors representing the same a boxplot would, but for multiple attributes at once. The following example shows an example of the radar-boxplot over Iris Dataset. The inner red region represents the 25-75% percentiles of each attribute, while the blue area represents the total range, excluding the outliers as defined by [Tukey, 1977](https://amstat.tandfonline.com/doi/abs/10.1080/00031305.1978.10479236). Outlier appears as whiskers, just like the classic boxplot.

<p align="center">
IQR = Q3 - Q1
<br/>
LOWER_OUTLIER = Q1 - (1.5 x IQR)
<br/>
UPPER_OUTLIER = Q3 + (1.5 x IQR)
</p>

![Radar-boxplot example with iris](https://github.com/caiohamamura/radarBoxplot-R/blob/master/data/Example.png?raw=true)

Intuitively you can see that *Iris setosa* has a significant different distribution of its attributes. Although the radar-boxplot is still useful for this dataset, because it has only 4 variables, this could also be visualized by pairs of two variables or either a 3D scatter plot with 3 variables.

The radar-boxplot is best suited when you have more than 4 relevant variables for your clustering/classification task, because it gives the possibility to represent higher dimensionality while still being readable.


## Example

```r
library(radarBoxplot)

# Default
radarBoxplot(iris[,1:4], iris[,5])

# Formula variant
radarBoxplot(Species ~ ., iris)

# Plot all in the same row with orange/green colors and median
radarBoxplot(Species ~ ., iris, plot.median=T, mfrow=c(1,3), col=c("orange", "green"))



```
