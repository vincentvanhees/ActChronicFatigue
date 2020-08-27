#' convert_diary 
#'
#' @param activitylog ...
#' @return no object is returned, only a new file is created in the same folder as the activitylog
#' @export
#' @importFrom utils read.csv write.csv
#'
convert_diary = function(activitylog = c()) {
  # rm(list=ls())
  # Script to convert waking times table to activity log that can be used by GGIR
  #---------------------------------------------------------------------------
  
  # Step 1: Assumed structure of Excel file:
  #   ID = 61249			
  #   Datum:	    08-11-19  09-11-19  10-11-19
  #   Starttijd:	3:54	    6:39	    8:02
  #   Eindtijd:	  23:40	    20:06	    20:46
  #   Meegenomen:	JA 	      JA	      JA
  # 
  #   ID = 61292			
  #   Datum:	    03-12-19	04-12-19	05-12-19
  #   Starttijd:	7:15	    7:05	    7:05
  #   Eindtijd:	  23:20	    21:35	    20:55
  #   Meegenomen:	JA 	      JA	      JA
  
  # Step 2: Save xlsx file as csv.
  
  # Step 3: specify path of activity log in csv-format:
  # activitylog ="/media/vincent/DATA/actometer_nkcv/actometer_tijden/Actometer_tijden_volledig.csv"
  
  # Step 4: Run code below
  #---------------------------------------------------------------------------
  outputfile = paste0(unlist(strsplit(activitylog,"[.]cs"))[1],"2.csv")
  
  D <- read.csv(activitylog, header = F)
  
  blocks = grep(pattern = "ID", x = D[,1])
  AL = matrix("", length(blocks), 1+((ncol(D)-1)*3)) # initialise new output format
  for (i in 1:length(blocks)) {
    AL[i,1] = unlist(strsplit(D[blocks[i],1],"D = "))[2]
    tmp = D[blocks[i]+1:4,2:ncol(D)]
    tmp = as.matrix(tmp[1:3,grep(pattern = "JA", x = tmp[4,])])
    dim(tmp) = c(1,length(tmp))
    if (length(grep(pattern = "(+)|24:00|0:00", x = tmp)) > 0) { 
      # waking up at or after midnight is currently now permitted,
      # because that is already the end of the day
      tmp[grep(pattern = "[(+)]|24:00|0:00", x = tmp)] = "23:55"
    }
    AL[i,2:(length(tmp)+1)] = tmp
  }
  AL = as.data.frame(AL)
  CN = c("ID", rep(c("date","start","end"), times=floor((ncol(AL)-1) / 3)))
  colnames(AL)[1:length(CN)] = CN
  
  write.csv(AL, file = outputfile, row.names = FALSE)
}