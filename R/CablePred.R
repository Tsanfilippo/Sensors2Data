#' Predict the amount of cable required to reach a desired headrope depth
#' with a trawl.
#'
#'Predict cable required (in feet) to reach a headrope depth (in meters) with a midwater
#'trawl

#' @param desired.depth
#'   A numeric scalar providing the desired head rope depth in meters (also known as
#'   fishing depth).
#' @param rdat
#'   A character scalar giving the name of the RData file that contains the cable length
#'   and fishing depth data that will be used to fit a model (currently uses lm())
#'   with fishing depth as the predictor and the cable length as the predicted.

#' @return
#'   The function generates a plot of fishing depth versus cable lenght and returns estimate
#'  of the cable length required to achieve a desired fishing depth to the console.
#'
#'
#' @details
#'  This function currently works for/is intended for midwater trawl data collected
#'  on the USGS RV Sturgeon using a 50 ft midwater trawl. The modeled cable length
#'  only includes one predictor variable at present. Given more data and more variable
#'  conditions, additional predictors as well as types of models may be added or
#'  required.
#'
#' @import svDialogs
#' @export
#'

CablePred <- function(desired.depth) {
  rdat <- dlg_open(title = "Select data file with cable length and fishing depth data.",
                   filters = dlg_filters[c("R", "All"), ])$res
  dat <- readRDS (rdat)
  cable.lm <- lm(Cable.feet ~ Depth.meters, data = dat)
  plot(Cable.feet ~ Depth.meters, data=dat)
  desired.depth <- dlg_input(GUI =depth,"Enter desired fishing depth in meters")$res
  cable.required <- as.numeric(coef(cable.lm)[1] + coef(cable.lm)[2]*as.numeric(desired.depth))
  cable.required2 <- paste0("Estimated cable required = ", round(cable.required), " ft")
  return(cable.required2)
}



