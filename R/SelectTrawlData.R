#' This function is called by TimeOnBottom(). It provides the interactive data selection
#' and generation of metadata to be recorded with the selected data.
#'
#' @return
#' A file path and parameters of the data contained in the file at that path.
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
#'
#'
select_trawl_data <- function() {

# This code was written by Dave Warner on a Mac Mini using R 3.3.3 while on the
# R/V Sturgeon on Lake Michigan, 11 August 2018.


############################################################################################################
#  1.  Run this by highlighting the portion prior to the write.csv() function and hitting control-enter
#  2.  After running the portion just mentioned, highlight the portion from write.csv() to the end and
#      hit control-enter
#  3.  If you try to run this using the Run App button, which is present because of the shiny portion
#      of the code, it will fail.



# I discovered that this code gives you a different result for date-time than the Excel formula
# provided by Marport.  The time produced by this code actually matches the times observed
# using Scala replay and also matches what we recorded by hand during the tow.
# I don't know why.


# The code does a few things.
# 1) it reads in extracted Marport sensor data and calculates a useable time
# 2) it prompts you to select the temporal chunk of data you want
# 3) it saves a csv of a subset of the sensor data with this time field added and using information
# you are prompted to provide as the file name.
# These items are year (taken from the date-time you input), vessel, transect, and serial(s).
# 4) it makes an interactive plot of the headline sensor depth and temperature for the time chunk
# you selected and prompts you to draw a box around the data you want to use to calculate
# mean fishing depth and mean temperature.

# Read in a file exported from Marport
#prevent Time from being in scientific notation.  Not sure why.  Just hate it.
options(scipen = 999)

# Prompt to select a data file that has been exported/extracted from Marport SDS.
the.file <- dlg_open(title = "Select one Marport data file in data folder", filters = dlg_filters[c("R", "All"), ])$res

mydat<-read.csv(the.file, dec = ".", numerals =  "no.loss",
                col.names = c("Time", "Receiver", "Sensor.Location",
              "Type.of.Data", "Value", "Type", "SNR", "Noise.floor"),
                na.strings = "NA",
                skip = 1, fill=TRUE)


#Take a look at the data to make sure it was read ok
head(mydat)

#subset the data to get just the data for sensors we want
#  11 is a TE/CATCH on the headrope.  23 is a spread slave.  26 is a spread
# master.  These numbers correspond to the sensor nodes in Scala.
# This MAY need to be changed depending on which sensors you have on the net.  It is YOUR job
# to figure out which ones you are using.  Not anyone elese.
catch.and.spread <- subset(mydat, Sensor.Location %in% c("11","12","18","20",  "23", "26"))

# The time vairable is a string.  Need to convert to a number
catch.and.spread$Time <- as.numeric(catch.and.spread$Time)

# the Time variable is milliseconds and below I convert it to seconds
# because the time has an origin of 01/01/1970 and will need to be added
# to that value in seconds because that is how POSIX items work.
catch.and.spread$date.time <- as.POSIXct((catch.and.spread$Time/1000), origin = "1970-01-01", tz="America/New_York")


# Now you need to enter a date-time that will mark the beginning of the data you want to keep.
datetime <- as.POSIXct(dlg_input(GUI =datetime,"Enter the date-time (e.g. 2018-08-12 22:45:00) of the start of data you wish to keep")$res )
datetime2 <- as.POSIXct(dlg_input(GUI =datetime,"Enter the date-time (e.g. 2018-08-12 22:45:00) of the end of data you wish to keep")$res )
year <- substr(datetime, 1,4)
lake <- dlg_input(GUI =vessel,"Enter lake # (e.g. L2 for LM, 3 for LH)")$res
vessel <- dlg_input(GUI =vessel,"Enter vessel (e.g. v88 for Sturgeon, v17 For Steelhead)")$res
transect <- dlg_input(GUI=transect, "Enter transect (e.g. mw2, no3")$res
serials <- dlg_input(GUI = serials, "Enter serials (e.g. ser123, for multiple enter ser123ser456)")$res
type1 <- dlg_input(GUI = Type.of.Data, "Enter 1st data type name (e.g. Depth, Temperature)")$res
type2 <- dlg_input(GUI = Type.of.Data, "Enter 2nd data type name (e.g. Depth, Temperature)")$res
}




