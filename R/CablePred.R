#' Predict the amount of cable required to reach a desired headrope depth
#' with a trawl.
#'
# 'Predict cable required (in feet) to reach a headrope depth (in meters) with a midwater
# 'trawl
#'
#' @param desired.depth
#' Numeric scale the user enters after being prompted which is equal to the desired
#' fishing depth in meters.
#'
#' @return
#' The function generates a regression plot of fishing depth versus cable length, a plot
#' of regression residuals versus predicted fishing depth. Also returns an estimate
#' of the cable length required and the confidence interval to the console.
#'
#'
#' @details
#' This function requires input data in the form of a csv file with a variable called
#' Cable.feet and another called Depth.meters. The user will be prompted to lcoate this
#' file by the function, then asked to input a desired fishing depth (desired.depth)
#'
#' @import svDialogs
#' @export
#'

CablePred <- function(desired.depth) {
  rdat <- dlg_open(title = "Select data file with cable length and fishing depth data.",
                   filters = dlg_filters[c("R", "All"), ])$res
  dat <- read.csv (rdat, header=TRUE)
  desired.depth = desired.depth
  cable.lm <- lm(Cable.feet ~ Depth.meters, data = dat)
  #desired.depth <- data.frame(Depth.meters = as.numeric(dlg_input(GUI =depth,"Enter desired fishing depth in meters")$res))
  cable.required <- predict.lm(cable.lm, newdata = desired.depth, interval = "confidence")
  #cable.required <- as.numeric(coef(cable.lm)[1] + coef(cable.lm)[2]*as.numeric(desired.depth))
  cable.required2 <- paste0("Estimated cable required = ", round(cable.required[1]), " ft,  ", " range = ", round(cable.required[2]),
                            " - ", round(cable.required[3]), " ft")
  return(cable.required2)
}



