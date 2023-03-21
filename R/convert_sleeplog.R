#' convert_sleeplog
#'
#' @param sleeplog path to sleeplog (diary) file kept by the study (xlsx) assumed to have a column BEGINDAT with the date of recording and at least a column bed1 to indicate the time of going to bed on night 1.
#' @param part2resultsfile Path to GGIR part 2 person level summary file (csv).
#' @return no object is returned, only a new file is created in the same folder as the activitylog
#' @export
#' @importFrom utils write.csv
#'
convert_sleeplog = function(sleeplog = c(), part2resultsfile=c()) {
  if (file.exists(sleeplog) == TRUE) {
    colid = 1 # hard-coded assumption that id is stored in first column of sleeplog
    D = as.data.frame(readxl::read_excel(sleeplog))
    coln1 = which(colnames(D) == "bed1")
    startdate = which(colnames(D) == "BEGINDAT")
    # convert dates
    if (.Platform$OS.type == "windows") {
      options(warn = -1)
      testD = as.character(D$BEGINDAT)
      tmp1 = testD[which(is.na(testD) == FALSE)[1]]
      tmp2 = unlist(strsplit(tmp1, "-"))
      if (length(tmp2) == 1) { #dates as number (laptop Manon)
        D$BEGINDAT = as.Date(testD, origin = "1900-1-1") - 2 # to correct for Excel and R bias
      } else { # dates as readable character (laptop Tanja)
        D$BEGINDAT = as.Date(as.POSIXlt(D$BEGINDAT))
      }
      options(warn = 0)
    } else {
      D$BEGINDAT = as.Date(D$BEGINDAT)
    }
    D = D[which(is.na(D$BEGINDAT) == FALSE),]
    Ncols = length(coln1:ncol(D)) # make sure only an even number of colums are loaded
    D = D[, c(colid,  startdate, coln1:(ncol(D) - (Ncols %% 2)))]
    myfun = function(x) {
      if (is.na(x) == TRUE | x == "") {
        x = ""
      } else {
        if (is.character(x)) {
          x <- gsub(',', '.', x)
          x <- gsub('[.][.]', '.', x)
          if (!is.na(x)) {
            x = paste0(unlist(strsplit(x, " |`")), collapse = "")
            x <- as.numeric(x)
          }
        }
        if (is.numeric(x) == FALSE) {
          stop("\nTijd in slaapdagboek wordt niet herkent")
        }
        if (is.na(x) == TRUE | x == "") {
          x = ""
        } else {
          if (x >= 24) x = x - 24
          HRS = floor(x)
          MIN = floor((x - HRS)*60)
          SEC = floor((((x - HRS)*60) - MIN) * 60)
          if (SEC == 60) {
            SEC = 0
            MIN = MIN + 1
          }
          if (MIN == 60) {
            MIN = 0
            HRS = HRS + 1
          }
          convert2char = function(y) {
            y = as.character(y)
            if (nchar(y) == 1) {
              y = paste0("0",y)
            }
            return(y)
          }
          x = paste0(convert2char(HRS),":",convert2char(MIN),":", convert2char(SEC))
        }
      }
      return(x)
    }
    for (i in 3:ncol(D)) {
      D[,i] =  sapply(X = D[,i], FUN = myfun)
    }
    
    #---------------------------------------------------------------------------
    outputfile = paste0(unlist(strsplit(sleeplog,"[.]cs"))[1],"2.csv")
    colnames(D)[1] = "ID"
    dup = which(duplicated(D$ID, incomparables = NA) == TRUE)
    
    if (length(dup) > 0) {
      cat("\nLet op: Er zijn twee metingen voor dezelfde persoon in het slaapdagboek. We gebruiken de meeste recente.")
      rows2delete = c()
      for (ki in 1:length(dup)) {
        date2use = max(D$BEGINDAT[which(D$ID == D$ID[dup[ki]])], na.rm = TRUE)
        rows2delete = c(rows2delete, which(D$ID == D$ID[dup[ki]] & D$BEGINDAT != date2use ))
      }
      if (length(rows2delete) > 0) D = D[-rows2delete,]
    }
    if (length(part2resultsfile) > 0) {
      P2 = read.csv(part2resultsfile)
      P2$start_time
      P2$start_time = as.Date(as.POSIXlt(P2$start_time,
                                         format = "%Y-%m-%dT%H:%M:%S%z",
                                         tz = ""))
      D = cbind(D, matrix("",nrow(D), 14)) # add 7 days (14 entries)
      for (hi in 1:nrow(D)) {
        if (is.na(D$BEGINDAT[hi]) == FALSE) {
          matchingID = which(P2$ID == D$ID[hi])
          if (length(matchingID) > 0) {
            starttime_acc = P2$start_time[matchingID]
            if (length(starttime_acc) > 1) starttime_acc = max(starttime_acc, na.rm = TRUE)
            starttime_diary = D$BEGINDAT[hi]
            delay = as.numeric(starttime_diary - starttime_acc)
            if (delay < -1) {
              D[hi,3:ncol(D)] = ""
            } else if (delay > 0) { # & delay < 8) {
              deldays = delay * 2
              D[hi, (3 + deldays):(max(which(D[hi, ] != "")) + deldays)] = D[hi, 3:max(which(D[hi, ] != ""))]
              D[hi, 3:(2 + deldays)] = ""
            }
          }
        }
      }
      cnt = 1
      while (cnt > 0) { # remove rows without sleep diary
        tmp = as.character(D[cnt,3:ncol(D)])
        if (length(which(tmp != "")) == 0 |
            length(which(is.na(tmp) == FALSE)) == 0) {
          D = D[-cnt,]
        } else {
          cnt = cnt + 1
        }
        if (cnt > nrow(D)) cnt = cnt = -1
      }
    }
    options(warn = -1)
    D$ID = as.numeric(D$ID)
    options(warn = 0)
    write.csv(D, file = outputfile, row.names = FALSE)
  } else {
    if (length(sleeplog) != 0) {
      warning(paste0("\nBestand ",sleeplog, " bestaat niet. Conversie niet mogelijk."))
    }
    outputfile = c()
  }
  
  # options(warn=0)
  return(outputfile)
}
