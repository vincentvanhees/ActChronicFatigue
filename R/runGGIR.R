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
#' @param sep separator used by csv files stored by GGIR
#' @return no object is returned, GGIR writes all its outputs to files
#' @importFrom stats runif var
#' @export

runGGIR = function(datadir = c(), outputdir = c(), mode = c(), do.report = c(),
                   overwrite = FALSE, do.visual = FALSE, 
                   visualreport = FALSE, acc.metric = "BFEN", chunksize = 1,
                   loglocation = c(), do.parallel = TRUE, testbatch = FALSE,
                   sleeplogidnum = TRUE, hrs.del.start = 0, hrs.del.end = 0, sleepwindowType = "TimeInBed",
                   sep = ",") {
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
  #----------------------------------------------
  # Define step detection code locally to avoid that user has to specify it as a separate script
  verisense_count_steps <- function(input_data=runif(500,min=-1.5,max=1.5), coeffs=c(0,0,0)) {
    # by Matthew R Patterson, mpatterson@shimmersensing.com
    ## Find peaks of RMS acceleration signal according to Gu et al, 2017 method
    # This method is based off finding peaks in the summed and squared acceleration signal
    # and then using multiple thresholds to determine if each peak is a step or an artefact.
    # An additional magnitude threshold was added to the algorithm to prevent false positives 
    # in free living data. 
    #
    # returns sample location of each step
    fs = 15 # temporary for now, this is manually set
    acc <- sqrt(input_data[,1]^2 + input_data[,2]^2 + input_data[,3]^2)
    
    if (sd(acc) < 0.025) {
      # acceleration too low, no steps
      num_seconds = round(length(acc) / fs)
      steps_per_sec = rep(0,num_seconds)
    } else {
      # Search for steps
      # Thresholds
      k <- coeffs[[1]]
      period_min <- coeffs[[2]]
      period_max <- coeffs[[3]]
      sim_thres <- coeffs[[4]]   # similarity threshold
      cont_win_size <- coeffs[[5]]  # continuity window size
      cont_thres <- coeffs[[6]]     # continuity threshold
      var_thres <- coeffs[[7]]  # variance threshold
      mag_thres <- coeffs[[8]]
      
      # find the peak rms value is every range of k
      half_k <- round(k/2)
      segments <- floor(length(acc) / k)
      peak_info <- matrix(NA,nrow=segments,ncol=5)
      # peak_info[,1] - peak location
      # peak_info[,2] - acc magnitude
      # peak_info[,3] - periodicity (samples)
      # peak_info[,4] - similarity
      # peak_info[,5] - continuity
      
      # for each segment find the peak location
      for (i in 1:segments) {
        start_idx <- (i-1) * k + 1
        end_idx <- start_idx + (k-1)
        tmp_loc_a <- which.max(acc[start_idx:end_idx])
        tmp_loc_b <- (i-1) * k + tmp_loc_a
        # only save if this is a peak value in range of -k/2:+K/2
        start_idx_ctr <- tmp_loc_b - half_k
        if (start_idx_ctr < 1) {
          start_idx_ctr <- 1
        }
        end_idx_ctr <- tmp_loc_b + half_k
        if (end_idx_ctr > length(acc)) {
          end_idx_ctr <- length(acc)
        }
        check_loc <- which.max(acc[start_idx_ctr:end_idx_ctr])
        if (check_loc == (half_k + 1)) {
          peak_info[i,1] <- tmp_loc_b
          peak_info[i,2] <- max(acc[start_idx:end_idx])
        }
      }
      peak_info <- peak_info[is.na(peak_info[,1])!=TRUE,] # get rid of na rows
      
      # filter peak_info[,2] based on mag_thres
      peak_info <- peak_info[peak_info[,2] > mag_thres,]
      if (length(peak_info) > 10) {  # there must be at least two steps
        num_peaks <- length(peak_info[,1])
        
        no_steps = FALSE
        if (num_peaks > 2) {
          # Calculate Features (periodicity, similarity, continuity)
          peak_info[1:(num_peaks-1),3] <- diff(peak_info[,1]) # calculate periodicity
          peak_info <- peak_info[peak_info[,3] > period_min,] # filter peaks based on period_min
          peak_info <- peak_info[peak_info[,3] < period_max,]   # filter peaks based on period_max 
        } else {
          no_steps = TRUE
        }
      } else {
        no_steps = TRUE
      }
      
      if ( length(peak_info)==0 || length(peak_info) == sum(is.na(peak_info)) || no_steps == TRUE) {
        # no steps found
        num_seconds = round(length(acc) / fs)
        steps_per_sec = rep(0,num_seconds)
      } else {
        # calculate similarity
        num_peaks <- length(peak_info[,1])
        peak_info[1:(num_peaks-2),4] <- -abs(diff(peak_info[,2],2)) # calculate similarity
        peak_info <- peak_info[peak_info[,4] > sim_thres,]  # filter based on sim_thres
        peak_info <- peak_info[is.na(peak_info[,1])!=TRUE,] # previous statement can result in an NA in col-1
        
        # calculate continuity
        if (length(peak_info[,3]) > 5) {
          end_for <- length(peak_info[,3])-1
          for (i in cont_thres:end_for) {
            # for each bw peak period calculate acc var
            v_count <- 0 # count how many windows were over the variance threshold
            for (x in 1:cont_thres) {
              if (var(acc[peak_info[i-x+1,1]:peak_info[i-x+2,1]]) > var_thres) {
                v_count = v_count + 1
              }
            }
            if (v_count >= cont_win_size) {
              peak_info[i,5] <- 1 # set continuity to 1, otherwise, 0
            } else {
              peak_info[i,5] <- 0
            }
          }
        } 
        peak_info <- peak_info[peak_info[,5]==1,1] # continuity test - only keep locations after this
        peak_info <- peak_info[is.na(peak_info)!=TRUE] # previous statement can result in an NA in col-1
        
        if (length(peak_info)==0) {
          # no steps found
          num_seconds = round(length(acc) / fs)
          steps_per_sec = rep(0,num_seconds)
        } else {
          
          # debug plot
          # is_plot = F
          # if (is_plot) {
          #   library(ggplot2)
          #   library(plotly)
          #   acc.df <- data.frame(acc=acc, det_step=integer(length(acc)))
          #   acc.df$det_step[peak_info] <- 1  # to plot annotations, prepare a 0/1 column on dataframe
          #   acc.df$idx <- as.numeric(row.names(acc.df))
          #   pl <- ggplot(data=acc.df,aes(x=idx,y=acc)) 
          #   pl2 <- pl + geom_line()
          #   pl3 <- pl2 + geom_point(data=subset(acc.df,det_step==1),aes(x=idx,y=acc),color='red',size=1,alpha=0.7)
          #   pl4 <- ggplotly(pl3)
          #   print(pl4)  
          # }
          
          # for GGIR, output the number of steps in 1 second chunks
          start_idx_vec <- seq(from=1,to=length(acc),by=fs)
          steps_per_sec <- table(factor(findInterval(peak_info, start_idx_vec), levels = seq_along(start_idx_vec)))
          steps_per_sec <- as.numeric(steps_per_sec)
        }
      }
    }
    
    return(steps_per_sec)
  }
  myfun = list(FUN = verisense_count_steps,
               parameters = c(4, 4, 20, -1.0, 4, 4, 0.01, 1.25), # updated based on Rowlands et al Stepping up with GGIR 2022
               expected_sample_rate = 15,
               expected_unit = "g",
               colnames = c("step_count"),
               outputres = 1,
               minlength = 1,
               outputtype = "numeric",
               aggfunction = sum,
               timestamp = F,
               reporttype = "event")
  
  # activitylog = paste0(unlist(strsplit(activitylog,"[.]cs"))[1],"2.csv")
  GGIR::GGIR(#=======================================
             # INPUT NEEDED:
             #-------------------------------
             # General parameters
             #-------------------------------
             myfun = myfun,
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
             sep_reports = sep, #<= because this repo is used in The Netherlands
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
             mvpadur = c(5, 10, 15),
             threshold.lig = 40,
             threshold.mod = 100,
             threshold.vig = 400,
             bout.metric = 4,
             visualreport = visualreport,
             timewindow = "WW",
             dofirstpage = FALSE, #first page of pdf-report with simple summary histograms
             viewingwindow = 1) #viewingwindow of visual report: 1 centres at day and 2 centers at night
}
