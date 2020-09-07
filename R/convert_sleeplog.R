#' convert_sleeplog 
#'
#' @param sleeplog ...
#' @return no object is returned, only a new file is created in the same folder as the activitylog
#' @export
#' @importFrom utils write.csv
#' @importFrom readxl read_excel
#'
convert_sleeplog = function(sleeplog = c()) {
  if (file.exists(sleeplog) == TRUE) {
    colid = 1 # hard-coded assumption that id is stored in first column of sleeplog
    D = as.data.frame(readxl::read_excel(sleeplog))
    coln1 = which(colnames(D) == "bed1")
    Ncols = length(coln1:ncol(D)) # make sure only an even number of colums are loaded
    D = D[,c(colid, coln1:(ncol(D)-(Ncols %% 2)))]
    for (i in 2:ncol(D)) {
      myfun = function(x) {
        if (is.na(x) == TRUE | x == "") {
          x = ""
        } else {
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
      D[,i] =  sapply(X = D[,i], FUN = myfun)
    }
    #---------------------------------------------------------------------------
    outputfile = paste0(unlist(strsplit(sleeplog,"[.]cs"))[1],"2.csv")
    colnames(D)[1] = "ID"
    write.csv(D, file = outputfile, row.names = FALSE)
  } else {
    outputfile = c()
  }
  return(outputfile)
}