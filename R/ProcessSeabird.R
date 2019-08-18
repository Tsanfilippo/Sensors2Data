#' CTD processing from cnv files
#'
#' @title Process Seabird cnv files
#'
#'
#' @return
#' A graph of water temperature and fluorescence profiles as well as
#' a bin-averaged csv file.
#'
#'
#' @details
#' Reads a user-selected cnv file and carries out several operations. These
#' include data editing, bin averaging in 1-m increments, calculation of
#' surface temperature, thermocline depth, metalimnion depths, and water column
#' temperature.
#'
#'
#'
#' @import rLakeAnalyzer plyr oce
#' @export
#'
ProcessSeabird <- function() {
#### Part 1. Define directories, Load data ####

  # prompt for file path
    path <- file.choose(new=FALSE)
  # Pull full file name for use in readLines

  # pull file name and directories from specified file path, create output folder
    file.name <- basename(path)
    INPUT <- dirname(path)
    dir.create(paste0(INPUT, "\\CSV OUTPUT"), showWarnings=T)
    OUTPUT <- paste0(dirname(path), "\\CSV OUTPUT\\")
  #
    ##########################################################################
    # This first section looks at the header to find
    #1) how many lines to skip to get past the header and
    #2) what the names of the variables are and how many there are.
    warn <- options("warn")$warn
    options(warn = -1)
    lines <- readLines(path, encoding = "UTF-8")
    options(warn = warn)
    skippyline <- grep(pattern = "END", lines,
                       ignore.case = TRUE)
    nameLines <- grep("^# name [0-9][0-9]* = .*:.*$", lines,
                      ignore.case = TRUE)
    snline <- grep("Temperature SN", lines,
                   ignore.case = TRUE)
    sntext <- lines[snline]
    CTD_serial_number <- substr(strsplit(sntext, "=")[[1]][2], 2,6)
    colUnits <- vector("list", length(nameLines))
    colNamesInferred <- NULL
    dataNamesOriginal <- list()
    namesUsed <- NULL
    columns <- NULL

    for (iline in seq_along(nameLines)) {
      nu <- cnvName2oceName(lines[nameLines[iline]], columns)
      if (nu$name %in% namesUsed) {
        trial <- 2
        while (paste(nu$name, trial, sep = "") %in% namesUsed) {
          trial <- trial + 1
          if (trial > 10)
            break
        }
        nu$name <- paste(nu$name, trial, sep = "")
      }
      namesUsed <- c(namesUsed, nu$name)
      dataNamesOriginal[[nu$name]] <- nu$nameOriginal
      colUnits[[iline]] <- nu$unit
      colNamesInferred <- c(colNamesInferred, nu$name)
    }
    colNamesInferred <- unduplicateNames(colNamesInferred)
    names(colUnits) <- colNamesInferred
    numcols <- length(colNamesInferred)


  # read file, define column names
    the.file <- read.csv(path, header=FALSE, sep="", skip=skippyline)
      names(the.file) <- colNamesInferred

  # Save max depth for later
    max.depth <- round(max(the.file$depth),1)

  # Remove all depths < 0
    the.file <- the.file[the.file$depth >= 0,]

  # Bin average by depth (1m).
    the.file <- ddply(the.file, .(cut(the.file$depth, seq(0, ceiling(max(the.file$depth)), 1),  labels=seq(0,ceiling(max(the.file$depth))-1,1))), colwise(mean))
    names(the.file)[1] <- "Depth Bin"
    the.file$`Depth Bin` <- as.numeric(the.file$`Depth Bin`)

  # Write cast details into file
    split.name <- strsplit(file.name, "ser")
    split.name2 <- strsplit(split.name[[1]][2], "_")
      the.file$CTD <- CTD_serial_number
      the.file$Serial <- split.name2[[1]][1]



#### Part 2. Export plot and csv ####

  png(paste0(OUTPUT, substr(file.name,0,nchar(file.name)-4), " - Temp & Fluorescense plot.png"),height=800,width=800)

    shape <- rbind(c(2,2,2,2), c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(1,1,1,1))
      layout(shape)

    par(mar=c(5.5,6.5,5.5,2))

      thermo <- round(thermo.depth(the.file$temperature, the.file$`Depth Bin`),1)
      m.d <- round(meta.depths(the.file$temperature, the.file$`Depth Bin`),2)

      plot(-the.file$`Depth Bin`~the.file$temperature, type='l', ylab=NA, xlab=NA, lwd=2, xlim=c(0, max(the.file$temperature)+5), ylim=c(min(-the.file$`Depth Bin`)-5,0), col="blue", cex.axis=2.5, cex.lab=2.5, las=1)
        abline(h=-thermo, lty=2, col="red")
        abline(h=c(-m.d[1],-m.d[2]), lty=2)

      lines(-the.file$`Depth Bin`~the.file$fluorescence, lwd=2, col="darkgreen")

        fmax <- max(the.file$fluorescence)
        effs <- seq(0, fmax+5, 5)

        axis(3, at = effs, col.ticks="darkgreen", lwd.ticks=2, cex.axis=2.5)
          mtext("Temperature (C)", side=1, line=4, cex=1.75)
          mtext("Depth (m)", side=2, line=4.5, cex=1.75)
          mtext("Fluoresence (rel. units)", side=3, line=3.5, cex=1.5)

          rug(1, x=0:(max(the.file$temperature)+5), ticksize = -0.01, side=1, col="blue", lwd=2)
          rug(1, x=0:(fmax+5), ticksize = -0.01, side=3, col="darkgreen", lwd=2)
          rug(1, x=seq(0,-max(the.file$`Depth Bin`)-5, -1), ticksize = -0.005, side=2)
          rug(1, x=seq(0,-max(the.file$`Depth Bin`)-5, -5), ticksize = -0.01, side=2)

          mtext(paste0("CTD: ", the.file$CTD, ", ser#: ", the.file$Serial, "                                       Thermocline depth  ", thermo, " m"), cex=1.5, side=3, line=11, font=3, adj=0)

      column.temp <- round(mean(the.file$temperature),1)
      surface.temp <- round(mean(the.file$temperature[the.file$`Depth Bin`<=1.0]),1)
      ref.depth <- the.file$`Depth Bin`[abs(the.file$temperature - column.temp) == min(abs(the.file$temperature - column.temp))]


        mtext(paste0("Max Depth = ", max.depth, "m", '                                                   Reference Depth = ', ref.depth, "m"), cex=1.5, side=3, line=9, font=3, adj=0)
        mtext(paste0("Surface Temp = ", surface.temp, "C                                                 Mean Water Column Temp = ", column.temp, "C" ), cex=1.5, side=3, line=7, font=3, adj=0)

  dev.off()


  write.csv(the.file, paste0(OUTPUT, substr(file.name,0, nchar(file.name)-4), '.csv'), row.names=FALSE)

}
