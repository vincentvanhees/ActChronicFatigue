#' gt3x_to_csv
#'
#' @param path full path to file(s)
#' @param outpath directory where converted files should be stored (if not supplied it will be in same location
#' @param gzip Boolean to indicate whether output file should be gunzipped
#' @return no object is returned, only a new file is created
#' @export
#' @importFrom utils packageVersion
#' 
# This function adapted from the script written by Hansjoerg Baurecht 2020
gt3x_to_csv<-function(path = c(), outpath =c(), gzip=T){
  print(path)
  gt3x<-read.gt3x::read.gt3x(path=path, imputeZeroes=TRUE, asDataFrame=FALSE, verbose=TRUE)
  if (length(outpath) == 0) {
    outpath<-gsub(".gt3x",".csv",path,fixed=T)
  } else {
    outpath = paste0(outpath,"/",gsub(".gt3x",".csv",basename(path), fixed=T))
  }
  # convert date formats
  start.time<-unlist(strsplit(as.character(attributes(gt3x)$header$'Start Date'),split=" "))
  start.date<-format(attributes(gt3x)$header$'Start Date',format="%d.%m.%Y")
  download.time<-unlist(strsplit(as.character(attributes(gt3x)$header$'Download Date'),split=" "))
  download.date<-format(attributes(gt3x)$header$'Download Date',format="%d.%m.%Y")
  # write header to csv-file
  header<-paste("------------ Data File Created By ActiGraph ",attributes(gt3x)$header$'Device Type'," R read.gt3x v", packageVersion("read.gt3x"), " Firmware v", attributes(gt3x)$header$'Firmware', " date format dd.MM.yyyy at ", attributes(gt3x)$header$'Sample Rate', " Hz Filter Normal -----------",sep="")
  header<-c(header,paste("Serial Number:",attributes(gt3x)$header$'Serial Number',sep=" "))
  header<-c(header,paste("Start Time",start.time[2]))
  header<-c(header,paste("Start Date",start.date))
  epoch<-ifelse(is.null(attributes(gt3x)$header$'Epoch Period'),"",attributes(gt3x)$header$'Epoch Period')
  header<-c(header,paste("Epoch Period (hh:mm:ss)",epoch))
  header<-c(header,paste("Download Time",download.time[2]))
  header<-c(header,paste("Download Date",download.date))
  addr<-ifelse(is.null(attributes(gt3x)$header$'Current Memory Address'),"",attributes(gt3x)$header$'Current Memory Address')
  header<-c(header,paste("Current Memory Address:",addr))
  voltage<-gsub(",",".",attributes(gt3x)$header$'Battery Voltage')
  mode<-ifelse(is.null(attributes(gt3x)$header$'Mode'),"",attributes(gt3x)$header$'Mode')
  header<-c(header,paste("Current Battery Voltage: ",voltage," Mode=",mode,sep=""))
  header<-c(header,"--------------------------------------------------")
  fileConn<-file(outpath)
    writeLines(header, fileConn)
  close(fileConn)
  # write gravity data
  outdat<-as.matrix(gt3x)
  # identify lines with 0, 0, 0 and replace them by NA
  correct1<-base::rowSums(outdat)
  correct2<-Rfast::rowMins(outdat,value=T)
  outdat[,1]<-ifelse(correct1==0 & correct2==0,NA,outdat[,1])
  outdat[,2]<-ifelse(correct1==0 & correct2==0,NA,outdat[,2])
  outdat[,3]<-ifelse(correct1==0 & correct2==0,NA,outdat[,3])
  # if first line begins with vector of zeros, leaf it as vector of zeros
  if(correct1[1]==0 & correct2[1]==0){outdat[1,]<-c(0,0,0)}
  # replace NA by last value carried forward
  outdat[,1]<-DescTools::LOCF(outdat[,1])
  outdat[,2]<-DescTools::LOCF(outdat[,2])
  outdat[,3]<-DescTools::LOCF(outdat[,3])
  data.table::fwrite(x=outdat,file=outpath,col.name=F,append=T)
  if(gzip==T){
    command<-paste("gzip",outpath)
    system(command)
  }
}
