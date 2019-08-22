#' Calculate areal fish density from Sv and TS data exported from Echoview.
#'
#' @title Calculate areal fish density from Sv and TS data exported from Echoview.

#' @param TSexpansion
#'   Logical scalar to dictate whether cells with missing TS should borrow
#'   TS from other cells.
#' @param VerticalBinExpansion
#' Numeric scalar. The default = 1, which aallows the function to look for TS values for the 10m layer in the
#' adjoining 5m and 15m layers. If 'VerticalBinExpansin=2' then TS values from the 0, 5, 10, 15, and 20 m layers could be used
#' to estimate density in the 10m layer.
#'
#' @param HorizontalBinSize
#' Allows you to specify the horizontal cell size used in Echoview for sv integration and single
#' target detections.
#'
#' @return
#' Estimates of fish/hectare using NASC and average TS from nearest cell in the same layer that is not an NA valuu and
#' a tile plot of the density along the transect.
#'
#'
#' @details
#' Function to produce a rough areal density estimate of fish during an acoustic survey ###
#' Code estimates fish/hectare using NASC and average TS from nearest cell in the same layer that is not an NA value.
#' If no cells in that layer have a non NA TS value, the argument in the function 'VerticalBinExpansion' allows
#' adjacent layers to be searched for valid TS values. For example, when using 5m vertical cell size and
#' VerticalBinExpansion=1" (default), this would allow the function to look for TS values for the 10m layer in the
#' adjoining 5m and 15m layers. If 'VerticalBinExpansion=2' then TS values from the 0, 5, 10, 15, and 20 m layers could be used
#' to estimate density in the 10m layer. An additional argument in the function, 'HorizontalBinSize' allows you to specify the
#' horizontal cell size used in Echoview for sv integration and single target detections.

#' Input requires two *.csv files from sv integration and TS single target detections from Echoview.
#' I have been using a 5m surface exclusion line and a 1 m bottom exclusion line.
#' I use a 2500m horizontal bin and 5m vertical bin for cell size.
#' I don't have the advanced operators license which would make noise correction easier so
#' I haven't been doing that but I would guess that background noise correction step in Echoview might be beneficial.

#' Currently there is no minimum number of single target detection in a cell to preclude use in calculating density mainly because
#' we have so few single target detections!
#'
#' @author Ben Turschak
#'
#' @import ggplot2
#' @export
#'

AcousticFishCount<-function(TSexpansion=FALSE, VerticalBinExpansion=1,HorizontalBinSize=2500){
  library(ggplot2)

  svFile<-choose.files(default="",caption="Select sv Data")
  TSFile<-choose.files(default="",caption="Select TS Data")
  sv<-read.csv(svFile)
  TS<-read.csv(TSFile)

  sv[,"IntervalLayer"]<-paste(sv$Interval,sv$Layer,sep=".")
  TS[,"IntervalLayer"]<-paste(TS$Interval,TS$Layer,sep=".")

  svTS<-merge(sv,TS, by="IntervalLayer",all.x=TRUE)

  if(TSexpansion){
    for(i in 1:nrow(svTS)){
      if(is.na(svTS[i,"TS_mean"])){ #Get nearest cells in same layer that have TS values.

        layerIndex<-which(!is.na(svTS[,"TS_mean"]) & svTS[,"Layer.x"]==svTS[i,"Layer.x"]) #cells in the same layer as target cell
        DistVec<-abs(svTS[i,"Dist_M.x"]-svTS[,"Dist_M.x"])[layerIndex] #horizontal distance from target cell to nearest cell in the same layer with TS
        if(length(DistVec>0)){
          minDistVec<-min(DistVec)
          index<-layerIndex[which(abs(svTS[layerIndex,"Dist_M.x"]-svTS[i,"Dist_M.x"])==minDistVec)] #identify cell with closest horizontal distance in the same layer with TS
          svTS[i,"TS_mean"]<-svTS[index,"TS_mean"] #Fill in TS for target cell with TS from closest cell in the same layer
        }else{
          binExpansion<-VerticalBinExpansion #plus/minus depth bin for assigning TS to layers with no TS data
          layerIndex<-which(!is.na(svTS[,"TS_mean"]) & abs(svTS[,"Layer.x"]-svTS[i,"Layer.x"])==VerticalBinExpansion) #plus/minus depth bin for assigning TS to layers with no TS data
          DistVec<-abs(svTS[i,"Dist_M.x"]-svTS[,"Dist_M.x"])[layerIndex]
          if(length(DistVec>0)){
            minDistVec<-min(DistVec)
            index<-layerIndex[which(abs(svTS[layerIndex,"Dist_M.x"]-svTS[i,"Dist_M.x"])==minDistVec)] #identify cell with closest horizontal distance in the same layer with TS
            svTS[i,"TS_mean"]<-svTS[index,"TS_mean"] #Fill in TS for target cell with TS from closest cell in the same layer
          }
        }
      }
    }
  }
  #Areal Density
  svTS$sigmabs<-10^(svTS$TS_mean/10)
  svTS$rho.a<-svTS$NASC/(4*pi*1852^2*svTS$sigmabs)/0.0001 #number per hectare
  svTS$rho.a[is.na(svTS$rho.a)]<-0

  svTS<<-svTS

  #plot data
  axis.form <- theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14),
                     axis.title.x = element_text(size=14), axis.title.y = element_text(size=14),
                     axis.text=element_text(colour="black")) + theme(panel.border=element_rect(colour="black"))
  p <- ggplot(svTS,aes(x=(Interval.x*HorizontalBinSize/1000),y=-Layer_depth_max.x,z=rho.a)) + geom_tile(aes(fill=rho.a), colour = "grey60")
  p <- p + scale_fill_gradientn(colours = c("white", "yellow", "darkred"), values = c(0,0.1,1),name="fish / hectare")
  p <- p +labs(x="Transect Distance (km)",y="Depth (m)")
  p <- p + theme_bw() + axis.form
  print(p)
}

