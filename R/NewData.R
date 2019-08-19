#' Input new values for fishing depth and cable length used in midwater trawling
#' for use in modeling the cable length required to reach a desired fishing depth.
#' Will prompt user for innformation about the trawl operations.
#'
#' Input new values for fishing depth and cable length used in midwater trawling
#' for use in modeling the cable length required to reach a desired fishing depth.
#'
#' #' @param rdat
#' A character scalar giving the name of the RData file in \code{CablePred}
#' to which new data will be added and which is used by \code{\link{CablePred}}.
#'
#' @return
#' Adds a data pair to the trawl cable length and fishing depth file selected. Creates
#' a plot of cable length versus fishing depth.
#'
#' @details
#'  This function currently works for/is intended for midwater trawl data collected
#'  on the USGS RV Sturgeon using a 50 ft midwater trawl.
#'
#' @import svDialogs
#' @export
#'
new.dat <- function(){
  rdat <- dlg_open(title = "Select data file with cable length and fishing depth data.",
                   filters = dlg_filters[c("R", "All"), ])$res
  warp.fdepth.dat <- read.csv(rdat, header=TRUE)
  df <- data.frame(Date = NA, Time= NA, Cable.feet = NA, Depth.meters = NA,
                   Speed = NA, Lake=NA, Vessel = NA)
  df$Date <- dlg_input(GUI =Date,"Enter date as MM/DD/YY")$res
  df$Time<- dlg_input(GUI =Time,"Enter time as HH:MM:SS AM/PM")$res
  df$Cable.feet <- as.numeric(dlg_input(GUI = Cable.feet,"Enter length of trawl cable used in feet")$res)
  df$Depth.meters <- as.numeric(dlg_input(GUI =Depth.meters,"Enter headrope depth in meters")$res)
  df$Speed <- as.numeric(dlg_input(GUI =Speed,"Enter speed in mph")$res)
  df$Lake <- dlg_input(GUI =Lake,"Enter lake as number")$res
  df$Vessel <- dlg_input(GUI =Vessel,"Enter vessel as number")$res
  warp.fdepth.dat <- rbind(warp.fdepth.dat, df)
  write.csv(warp.fdepth.dat, paste0(rdat), row.names=FALSE)
  plot(Cable.feet ~ Depth.meters, data=warp.fdepth.dat )
}



