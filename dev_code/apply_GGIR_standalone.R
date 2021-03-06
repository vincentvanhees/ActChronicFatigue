# R script to run analyses for the Whitehall study II, by Vincent van Hees
rm(list=ls())
library("GGIR")

#==================================================================
# INPUT NEEDED:
# specify file number to start and end with, fill in c() if unknown

datadir = "/media/vincent/DATA/actometer_nkcv/rawactigraph_nkcv/nkcv_wrist"
outputdir = "/media/vincent/DATA/actometer_nkcv"
studyname= "nkcv_wrist"

f0 = c()
f1 = c()
#=====================================================================================
# load functions directly from local clone of the R package repository
dirR = "~/GGIR/R"
ffnames = dir(dirR) # creating list of filenames of scriptfiles to load
ffnames = ffnames[which(ffnames != "g.cwaread.R")]
for (i in 1:length(ffnames)) {
  source(paste(dirR,"/",ffnames[i],sep="")) #loading scripts for reading geneactiv data
}
loglocation = "/media/vincent/DATA/actometer_nkcv/sleepdiary/Logboek Vincent_def.xlsx2.csv"
SLEEPLOG = read.csv(loglocation)
nnights = round((ncol(SLEEPLOG) - 1) / 2)

g.shell.GGIR(mode = 5, #specify above
             datadir = datadir, #specify above
             outputdir = outputdir, #specify above
             f0 = c(), #specify above
             f1 = c(), #specify above
             overwrite = FALSE, #overwrite previous milestone data?
             do.report = c(5), #for what parts does and report need to be generated? (option: 2, 4 and 5)
             do.imp = TRUE, # Do imputation? (recommended)
             idloc = 5, #id location (1 = file header, 2 = filename)
             print.filename = TRUE,
             do.imp = TRUE,
             storefolderstructure = TRUE,
             do.parallel = FALSE,
             do.bfen = TRUE,
             acc.metric = "BFEN", # more similar original aktometer approach and reduced impact of Actigraph calibration challengee
             hb = 10,
             lb = 0.8,
             coln1 = 3,
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
             nnights= nnights,
             sleeplogidnum =TRUE ,
             loglocation = loglocation,
             do.visual = TRUE,
             sleepwindowType ="TimeInBed",
             ilevels = c(seq(0,300,by=50),8000), #acceleration values (metric ENMO) from which a frequency distribution needs to be derived, set value at c() if you do not want quantiles
             mvpathreshold = c(100), #MVPA (moderate and vigorous physical activity threshold
             threshold.lig=40,
             threshold.mod=100,
             threshold.vig=400,
             bout.metric = 4,
             visualreport = FALSE,
             timewindow = "WW",
             dofirstpage = FALSE, #first page of pdf-report with simple summary histograms
             viewingwindow = 1)