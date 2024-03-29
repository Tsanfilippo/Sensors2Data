% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AcousticFishCount.R
\name{AcousticFishCount}
\alias{AcousticFishCount}
\title{Calculate areal fish density from Sv and TS data exported from Echoview.}
\usage{
AcousticFishCount(TSexpansion = FALSE, VerticalBinExpansion = 1,
  HorizontalBinSize = 2500)
}
\arguments{
\item{TSexpansion}{Logical scalar to dictate whether cells with missing TS should borrow
TS from other cells.}

\item{VerticalBinExpansion}{Numeric scalar. The default = 1, which aallows the function to look for TS values for the 10m layer in the
adjoining 5m and 15m layers. If 'VerticalBinExpansin=2' then TS values from the 0, 5, 10, 15, and 20 m layers could be used
to estimate density in the 10m layer.}

\item{HorizontalBinSize}{Allows you to specify the horizontal cell size used in Echoview for sv integration and single
target detections.}
}
\value{
Estimates of fish/hectare using NASC and average TS from nearest cell in the same layer that is not an NA valuu and
a tile plot of the density along the transect.
}
\description{
Calculate areal fish density from Sv and TS data exported from Echoview.
}
\details{
Function to produce a rough areal density estimate of fish during an acoustic survey ###
Code estimates fish/hectare using NASC and average TS from nearest cell in the same layer that is not an NA value.
If no cells in that layer have a non NA TS value, the argument in the function 'VerticalBinExpansion' allows
adjacent layers to be searched for valid TS values. For example, when using 5m vertical cell size and
VerticalBinExpansion=1" (default), this would allow the function to look for TS values for the 10m layer in the
adjoining 5m and 15m layers. If 'VerticalBinExpansion=2' then TS values from the 0, 5, 10, 15, and 20 m layers could be used
to estimate density in the 10m layer. An additional argument in the function, 'HorizontalBinSize' allows you to specify the
horizontal cell size used in Echoview for sv integration and single target detections.
Input requires two *.csv files from sv integration and TS single target detections from Echoview.
I have been using a 5m surface exclusion line and a 1 m bottom exclusion line.
I use a 2500m horizontal bin and 5m vertical bin for cell size.
I don't have the advanced operators license which would make noise correction easier so
I haven't been doing that but I would guess that background noise correction step in Echoview might be beneficial.
Currently there is no minimum number of single target detection in a cell to preclude use in calculating density mainly because
we have so few single target detections!
}
\author{
Ben Turschak
}
