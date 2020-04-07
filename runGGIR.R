# R script for processing Actigraph data
# By: Vincent van Hees (2019)
rm(list=ls())

# library("devtools")
# install_github("wadpac/GGIR")
# packageVersion("GGIR")
# library("GGIR")
dirR = "~/GGIR/R"
ffnames = dir(dirR) # creating list of filenames of scriptfiles to load
for (i in 1:length(ffnames)) {
  source(paste(dirR,"/",ffnames[i],sep="")) #loading scripts for reading geneactiv data
}
#==================================================================
# INPUT NEEDED:

# Locatie data:
datadir = "/media/vincent/DATA/actometer_nkcv/rawactigraph_nkcv" #~/data/rawactigraph"

# Locatie van waar je de resultaten wilt hebben (mag niet een subfolder zijn van data folder):
outputdir = "/media/vincent/DATA/actometer_nkcv"


#==================================================================
g.shell.GGIR(#=======================================
             # INPUT NEEDED:
             #-------------------------------
             # General parameters
             #-------------------------------
             mode=c(), #specify above
             datadir=datadir, #specify above
             outputdir=outputdir, #specify above
             f0=c(), #specify above
             f1=c(), #specify above
             overwrite = FALSE, #overwrite previous milestone data?
             do.report=c(2), #for what parts does and report need to be generated? (option: 2, 4 and 5)
             do.imp=TRUE, # Do imputation? (recommended)
             idloc=1, #id location (1 = file header, 2 = filename)
             print.filename=TRUE,
             storefolderstructure = TRUE,
             do.parallel = TRUE,
             do.bfen=TRUE,
             hb = 10,
             lb=0.8,
             chunksize=0.8,
             do.cal=TRUE,
             desiredtz = "Europe/Amsterdam",
             strategy = 1, #Strategy (see tutorial for explanation)
             ndayswindow=7, #only relevant when strategy = 3
             hrs.del.start = 0, # Only relevant when strategy = 2. How many HOURS need to be ignored at the START of the measurement?
             hrs.del.end = 0, # Only relevant when strategy = 2. How many HOURS need to be ignored at the END of the measurement?
             maxdur = 14, # How many DAYS of measurement do you maximumally expect?
             includedaycrit = 16, # number of minimum valid hours in a day to attempt physical activity analysis
             winhr = c(5), # size of M5 and L5 (5 hours by default)
             qlevels = c(c(1380/1440),c(1410/1440)), #quantiles to calculate, set value at c() if you do not want quantiles
             qwindow=c(0,9,21,24), #window over which to calculate quantiles
             ilevels = c(seq(0,300,by=50),8000), #acceleration values (metric ENMO) from which a frequency distribution needs to be derived, set value at c() if you do not want quantiles
             mvpathreshold =c(100), #MVPA (moderate and vigorous physical activity threshold
             bout.metric = 4,
             visualreport=FALSE,
             dofirstpage = FALSE, #first page of pdf-report with simple summary histograms
             viewingwindow=1) #viewingwindow of visual report: 1 centres at day and 2 centers at night
