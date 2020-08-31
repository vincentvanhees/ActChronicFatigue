#' runGGIR
#'
#' @param datadir ...
#' @param outputdir ...
#' @param mode ...
#' @param do.report ...
#' @param overwrite ...
#' @return no object is returned, GGIR writes all its outputs to files
#' @export

runGGIR = function(datadir=c(), outputdir =c(), mode = c(), do.report=c(),
                   overwrite=FALSE) {

  # activitylog = paste0(unlist(strsplit(activitylog,"[.]cs"))[1],"2.csv")
  GGIR::g.shell.GGIR(#=======================================
               # INPUT NEEDED:
               #-------------------------------
               # General parameters
               #-------------------------------
               mode = mode, #specify above
               datadir = datadir, #specify above
               outputdir = outputdir, #specify above
               f0 = c(), #specify above
               f1 = c(), #specify above
               overwrite = overwrite, #overwrite previous milestone data?
               do.report = do.report, #for what parts does and report need to be generated? (option: 2, 4 and 5)
               do.imp = TRUE, # Do imputation? (recommended)
               idloc = 2, #id location (1 = file header, 2 = filename)
               print.filename = TRUE,
               do.imp = TRUE,
               storefolderstructure = TRUE,
               do.parallel = TRUE,
               do.bfen = TRUE,
               acc.metric = "BFEN",
               hb = 10,
               lb = 0.8,
               chunksize = 0.8,
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
               nnights= 18,
               do.visual = TRUE,
               ilevels = c(seq(0,300,by=50),8000), #acceleration values (metric ENMO) from which a frequency distribution needs to be derived, set value at c() if you do not want quantiles
               mvpathreshold = c(50), #MVPA (moderate and vigorous physical activity threshold
               threshold.lig=15,
               threshold.mod=50,
               threshold.vig=150,
               bout.metric = 4,
               visualreport = TRUE,
               timewindow = "WW",
               dofirstpage = FALSE, #first page of pdf-report with simple summary histograms
               viewingwindow = 1) #viewingwindow of visual report: 1 centres at day and 2 centers at night
}
  