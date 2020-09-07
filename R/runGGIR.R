#' runGGIR
#'
#' @param datadir ...
#' @param outputdir ...
#' @param mode ...
#' @param do.report ...
#' @param overwrite ...
#' @param do.visual ...
#' @param visualreport ...
#' @param acc.metric ...
#' @param chunksize ...
#' @param loglocation ...
#' @param do.parallel ...
#' @param testbatch ...
#' @return no object is returned, GGIR writes all its outputs to files
#' @export

runGGIR = function(datadir=c(), outputdir =c(), mode = c(), do.report=c(),
                   overwrite=FALSE, do.visual=FALSE,
                   visualreport=FALSE, acc.metric = "BFEN", chunksize = 1,
                   loglocation = c(), do.parallel = TRUE, testbatch = FALSE) {
  if (testbatch == TRUE) {
    f0 = 1
    f1 = 2
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
  GGIR::g.shell.GGIR(#=======================================
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
               do.imp = TRUE,
               storefolderstructure = TRUE,
               do.parallel = do.parallel,
               do.bfen = TRUE,
               acc.metric = "BFEN",
               hb = 10,
               lb = 0.8,
               coln1 = 2,
               chunksize = chunksize,
               do.cal=TRUE,
               minloadcrit = 3 * 24,
               desiredtz = "Europe/Amsterdam",
               strategy = 1, #Strategy (see tutorial for explanation)
               ndayswindow=7, #only relevant when strategy = 3
               hrs.del.start = 0, # Only relevant when strategy = 2. How many HOURS need to be ignored at the START of the measurement?
               hrs.del.end = 0, # Only relevant when strategy = 2. How many HOURS need to be ignored at the END of the measurement?
               maxdur = 18, # How many DAYS of measurement do you maximumally expect?
               includedaycrit = 16, # number of minimum valid hours in a day to attempt physical activity analysis
               winhr = c(5), # size of M5 and L5 (5 hours by default)
               qlevels = c(c(1380/1440),c(1410/1440)), #quantiles to calculate, set value at c() if you do not want quantiles
               # qwindow=c(0,9,21,24), #window over which to calculate quantiles
               qwindow = c(0, 24), #activitylog,
               nnights= nnights,
               loglocation = loglocation,
               do.visual = do.visual,
               ilevels = c(seq(0,300,by=50),8000), #acceleration values (metric ENMO) from which a frequency distribution needs to be derived, set value at c() if you do not want quantiles
               mvpathreshold = c(100), #MVPA (moderate and vigorous physical activity threshold
               threshold.lig=40,
               threshold.mod=100,
               threshold.vig=400,
               bout.metric = 4,
               visualreport = visualreport,
               timewindow = "WW",
               dofirstpage = FALSE, #first page of pdf-report with simple summary histograms
               viewingwindow = 1) #viewingwindow of visual report: 1 centres at day and 2 centers at night
}
  