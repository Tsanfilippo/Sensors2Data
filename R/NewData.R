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
#' a regression plot of cable length versus fishing depth. Creates a plot
#' of residuals from the regression.
#'
#' @details
#'  This function asks the user for new trawl cable length and fishing depth
#'  values and saves them to the user-specific csv file.
#'
#' @import svDialogs ggplot2
#' @export
#'
NewData<- function(){
  rdat <- dlg_open(title = "Select data file with cable length and fishing depth data.",
                   filters = dlg_filters[c("R", "All"), ])$res
  warp.fdepth.dat <- read.csv(rdat, header=TRUE, stringsAsFactors = FALSE,
          colClasses = c('character', 'character', 'numeric', 'numeric',
      'numeric', 'numeric', 'numeric','numeric'))
  #warp.fdepth.dat$Date.time <- as.POSIXct(warp.fdepth.dat$Date.time)
  df <- data.frame(Date.time = NA, Transect = NA, Serial = NA, Cable.feet = NA, Depth.meters = NA,
                   Speed = NA, Lake=NA, Vessel = NA, Trawl_Id = NA, Tr_Door = NA)
  df$Date.time <- dlg_input(GUI =Date.time,"Enter the date-time (e.g. 2018-08-12 22:45:00) of the start of the tow")$res
  df$Serial <- as.numeric(dlg_input(GUI = Serial,"Enter serial as number")$res)
  df$Transect <- as.character(dlg_input(GUI = Transect,"Enter transect as text (mw4)")$res)
  df$Cable.feet <- as.numeric(dlg_input(GUI = Cable.feet,"Enter length of trawl cable used in feet")$res)
  df$Depth.meters <- as.numeric(dlg_input(GUI =Depth.meters,"Enter headrope depth in meters")$res)
  df$Speed <- as.numeric(dlg_input(GUI =Speed,"Enter speed in mph")$res)
  df$Lake <- dlg_input(GUI =Lake,"Enter lake as number")$res
  df$Vessel <- dlg_input(GUI =Vessel,"Enter vessel as number")$res
  df$Trawl_Id <- dlg_input(GUI =Trawl_Id,"Enter trawl ID as number")$res
  df$Tr_Door <- dlg_input(GUI =Tr_Door,"Enter door ID as number")$res
  warp.fdepth.dat <- rbind(warp.fdepth.dat, df)
  write.csv(warp.fdepth.dat, paste0(rdat), row.names=FALSE)

  fit <- lm(Cable.feet ~ Depth.meters, data=warp.fdepth.dat)
  p1 <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "magenta") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
                       "Intercept =",signif(fit$coef[[1]],3 ),
                       " Slope =",signif(fit$coef[[2]], 3),
                       " P =",signif(summary(fit)$coef[2,4], 3), "N = ",
                       length(warp.fdepth.dat$Depth.meters)))
  print(p1)
  p2 <- ggplot(fit, aes(fit$fitted.values, fit$residuals)) +
    geom_point() +
    labs(x = "Fitted values (ft)", y="Residuas", size=16)
  print(p2)
  }



