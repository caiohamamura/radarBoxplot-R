# Radar-Boxplot

This package provides the implementation of the radar-boxplot, a charting technique create and developed by the author (Caio Hamamura).

## Installation

Currently the installation process is through devtools.

```r
install.packages("devtools")
devtools::install_github("caiohamamura/radarBoxplot-R")
```

## Description

It merges the concepts of both radar chart and the boxplot chart, allowing to compare multivariate data for multiple classes/clusters at a time. It provides a intuitive understanding over the data by creating radar polygons which can be compared in terms of shape and thickness, giving a meaningful insight towards identifying high inner variation and similar classes/clusters.

By interpreting the radar-boxplot, it is possible to predict classification confusion over classes and understand why and what could be done to achieve a better result.

The radar-boxplot draws two different regions colors representing the same a boxplot would, but for multiple attributes at once. The following example shows an example of the radar-boxplot over Iris Dataset. The inner red region represents the 25-75% percentiles of each attribute, while the blue area represents the total range, excluding the outliers as defined by [Tukey, 1977](https://amstat.tandfonline.com/doi/abs/10.1080/00031305.1978.10479236). Outlier appears as whiskers, just like the classic boxplot.

<center>
  
IQR = Q3 - Q1

LOWER_OUTLIER = Q1 - (1.5 x IQR)

UPPER_OUTLIER = Q3 + (1.5 x IQR)

</center>

![Radar-boxplot example with iris](https://github.com/caiohamamura/radarBoxplot-R/blob/master/data/Example.png?raw=true)

Intuitively you can see that *Iris setosa* has a significant different distribution of its attributes. Although the radar-boxplot is still useful for this dataset, because it has only 4 variables, this could also be visualized by pairs of two variables or either a 3D scatter plot with 3 variables.

The radar-boxplot is best suited when you have more than 4 relevant variables for your clustering/classification task, because it gives the possibility to represent higher dimensionality while still being readable.
