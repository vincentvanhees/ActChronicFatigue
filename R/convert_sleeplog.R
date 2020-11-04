#' convert_sleeplog 
#'
#' @param sleeplog ...
#' @param part2resultsfile ...
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
    Ncols = length(coln1:ncol(D)) # make sure only an even number of colums are loaded
    D = D[,c(colid,  startdate, coln1:(ncol(D)-(Ncols %% 2)))]
    myfun = function(x) {
      if (is.na(x) == TRUE | x == "") {
        x = ""
      } else {
        if (is.character(x)) {
          x <- gsub(',','.',x)
          x <- as.numeric(x)
        }
        if (is.numeric(x) == FALSE) {
          stop("\nTijd in slaapdagboek wordt niet herkent")
        }
        if (x >= 24) x = x - 24
        HRS = floor(x)
        MIN = floor((x - HRS)*60)
        SEC = floor((((x - HRS)*60) - MIN) * 60)
        if (SEC == 60) {
          SEC = 0
          MIN = MIN +1
        }
        if (MIN == 60) {
          MIN = 0
          HRS = HRS +1
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
      return(x)
    }
    for (i in 3:ncol(D)) {
      
      D[,i] =  sapply(X = D[,i], FUN = myfun)
    }
    D$BEGINDAT = as.Date(D$BEGINDAT)
    #---------------------------------------------------------------------------
    outputfile = paste0(unlist(strsplit(sleeplog,"[.]cs"))[1],"2.csv")
    colnames(D)[1] = "ID"
    dup = which(duplicated(D$ID,incomparables=NA) == TRUE)
    if (length(dup) > 0) {
      warning("\nEr zijn twee metingen voor dezelfde persoon in het slaapdagboek. We gebruiken de meeste recente.")
      for (ki in 1:length(dup)) {
        date2use = max(D$BEGINDAT[which(D$ID == D$ID[dup[ki]])], na.rm = TRUE)
        D = D[-which(D$ID == D$ID[dup[ki]] & D$BEGINDAT == date2use),]
      }
    }
    
    #
    if (length(part2resultsfile) > 0) {
      P2 = read.csv(part2resultsfile)
      P2$start_time
      P2$start_time = as.Date(as.POSIXlt(P2$start_time,format="%Y-%m-%dT%H:%M:%S%z",tz=""))
      D = cbind(D, matrix("",nrow(D), 14)) # add 7 days (14 entries)
      
      for (hi in 1:nrow(D)) {
        if (is.na(D$BEGINDAT[hi]) == FALSE) {
          matchingID = which(P2$ID == D$ID[hi])
          if (length(matchingID) > 0) {
            starttime_acc = P2$start_time[matchingID]
            if (length(starttime_acc) > 1) starttime_acc = max(starttime_acc, na.rm = TRUE)
            starttime_diary = D$BEGINDAT[hi]
            delay = starttime_diary - starttime_acc
            if (delay <= 0) {
              D[hi,3:ncol(D)] = ""
            } else if (delay > 0 & delay < 8) {
              deldays = delay * 2
              D[hi,(3+deldays):(max(which(D[hi,] != ""))+deldays)] = D[hi,3:max(which(D[hi,] != ""))]
              D[hi,3:(2+deldays)] = ""
            } else if (delay >= 8) {
              D[hi,3:ncol(D)] = ""
            }
          }
        }
      }
      cnt = 1
      while (cnt > 0) { # remove rows without sleep diary
        tmp = as.character(D[cnt,3:ncol(D)])
        if (length(which(tmp != "")) == 0 | length(which(is.na(tmp) ==FALSE)) == 0) {
          D = D[-cnt,]
        } else {
          cnt = cnt + 1
        }
        if (cnt > nrow(D)) cnt = cnt = -1
      }
    }
    D$ID = as.numeric(D$ID)
    write.csv(D, file = outputfile, row.names = FALSE)
  } else {
    if (length(sleeplog) != 0) {
      warning(paste0("\nBestand ",sleeplog, " bestaat niet. Conversie niet mogelijk."))
    }
    outputfile = c()
  }
  return(outputfile)
}