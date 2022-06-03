#' runGGIR
#'
#' @param datadir GGIR argument, see document R package GGIR
#' @param outputdir GGIR argument, see document R package GGIR
#' @param mode GGIR argument, see document R package GGIR
#' @param do.report GGIR argument, see document R package GGIR
#' @param overwrite GGIR argument, see document R package GGIR
#' @param do.visual GGIR argument, see document R package GGIR
#' @param visualreport GGIR argument, see document R package GGIR
#' @param acc.metric GGIR argument, see document R package GGIR
#' @param chunksize GGIR argument, see document R package GGIR
#' @param loglocation GGIR argument, see document R package GGIR
#' @param do.parallel GGIR argument, see document R package GGIR
#' @param testbatch If set to TRUE the function will only process the first three files
#' @param sleeplogidnum GGIR argument, see document R package GGIR
#' @param hrs.del.start GGIR argument, see document R package GGIR
#' @param hrs.del.end GGIR argument, see document R package GGIR
#' @param sleepwindowType GGIR argument, see document R package GGIR
#' @return no object is returned, GGIR writes all its outputs to files
#' @export

runGGIR = function(datadir = c(), outputdir = c(), mode = c(), do.report = c(),
                   overwrite = FALSE, do.visual = FALSE, 
                   visualreport = FALSE, acc.metric = "BFEN", chunksize = 1,
                   loglocation = c(), do.parallel = TRUE, testbatch = FALSE,
                   sleeplogidnum = TRUE, hrs.del.start = 0, hrs.del.end = 0, sleepwindowType = "TimeInBed") {
  if (testbatch == TRUE) {
    f0 = 1
    f1 = 3
  } else {
    f0 = f1 = c()
  }
  if (length(loglocation) > 0) {
    SLEEPLOG = read.csv(loglocation)
    nnights = round((ncol(SLEEPLOG) - 1) / 2)
  } else {
    nnights = 18
  }
  # activitylog = paste0(unlist(strsplit(activitylog,"[.]cs"))[1],"2.csv")
  GGIR::GGIR(#=======================================
             # INPUT NEEDED:
             #-------------------------------
             # General parameters
             #-------------------------------
             mode = mode, #specify above
             datadir = datadir, #specify above
             outputdir = outputdir, #specify above
             f0 = f0, #specify above
             f1 = f1, #specify above
             overwrite = overwrite, #overwrite previous milestone data?
             do.report = do.report, #for what parts does and report need to be generated? (option: 2, 4 and 5)
             do.imp = TRUE, # Do imputation? (recommended)
             idloc = 5, #id location (1 = file header, 2 = filename)
             print.filename = TRUE,
             storefolderstructure = TRUE,
             do.parallel = do.parallel,
             do.bfen = TRUE,
             do.enmo = TRUE,
             do.anglez = TRUE,
             acc.metric = acc.metric, # more similar original aktometer approach and reduced impact of Actigraph calibration challengee
             hb = 10,
             lb = 0.8,
             coln1 = 3,
             imputeTimegaps = TRUE,
             chunksize = chunksize,
             do.cal = TRUE,
             minloadcrit = 3 * 24,
             desiredtz = "Europe/Amsterdam",
             strategy = 1, #Strategy (see tutorial for explanation)
             ndayswindow = 7, #only relevant when strategy = 3
             hrs.del.start = hrs.del.start, # Only relevant when strategy = 2. How many HOURS need to be ignored at the START of the measurement?
             hrs.del.end = hrs.del.end, # Only relevant when strategy = 2. How many HOURS need to be ignored at the END of the measurement?
             maxdur = 18, # How many DAYS of measurement do you maximumally expect?
             includedaycrit = 16, # number of minimum valid hours in a day to attempt physical activity analysis
             winhr = c(5), # size of M5 and L5 (5 hours by default)
             qlevels = c(c(1380/1440), c(1410/1440)), #quantiles to calculate, set value at c() if you do not want quantiles
             # qwindow=c(0,9,21,24), #window over which to calculate quantiles
             qwindow = c(0, 24), #activitylog,
             nnights = nnights,
             sleeplogidnum = sleeplogidnum ,
             loglocation = loglocation,
             do.visual = do.visual,
             sleepwindowType = sleepwindowType,
             ilevels = c(0, 40, 100, 8000), #acceleration values (metric ENMO) from which a frequency distribution needs to be derived, set value at c() if you do not want quantiles
             mvpathreshold = c(100), #MVPA (moderate and vigorous physical activity threshold
             mvpadur = c(10, 20, 30),
             threshold.lig = 40,
             threshold.mod = 100,
             threshold.vig = 400,
             bout.metric = 4,
             visualreport = visualreport,
             timewindow = "WW",
             dofirstpage = FALSE, #first page of pdf-report with simple summary histograms
             viewingwindow = 1) #viewingwindow of visual report: 1 centres at day and 2 centers at night
}
