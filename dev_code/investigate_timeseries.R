rm(list=ls())
graphics.off()


iso8601chartime2POSIX = function(x,tz){
  return(as.POSIXlt(x,format="%Y-%m-%dT%H:%M:%S%z",tz))
}

problematic_wrist_id = c(60942, 61038, 61044, 61113, 61207, 61358, 61359)
problematic_hip_id = c(60984, 61105, 61121, 61158, 61330)

# specify data paths:
mydatadir = "/media/vincent/DATA" # directory where the labels.csv file is stored
datfiles = paste0(mydatadir,"/actometer_nkcv/dat_eat_files/dat")
GGIRfiles = paste0(mydatadir,"/actometer_nkcv/output_rawactigraph_nkcv/meta/ms2.out")
filewithlabels = paste0(mydatadir,"/actometer_nkcv/labels.csv") # specify file location

fnames = dir(datfiles, full.names = T) # actometer dat files
gnames = dir(GGIRfiles, full.names = T)

pdf(file = paste0(mydatadir,"/actometer_nkcv/timeseries_comparison_actometer_actigraph_wrist.pdf"))
labels = read.csv(filewithlabels, sep=",")
for (i in 1:length(fnames)) {
  id = unlist(strsplit(basename(fnames[i]),"[.]dat"))
  if (as.numeric(id) %in% problematic_wrist_id) {
    performance = "incorrect"
  } else {
    performance = "correct"
  }
  ss = labels[labels$ID==id,]
  if (nrow(ss) > 0) {
    label = ss$label
    loc = ss$loc
    S = grep(pattern = id, x = gnames)
  } else {
    S = c()
    loc=""
  }
  if (length(S) > 0 & loc == "wrist") {
    # Load Actigraph data
    load(gnames[S[1]])
    AG = IMP$metashort
    AG$timestamp = iso8601chartime2POSIX(AG$timestamp, tz= "Europe/Amsterdam")
    # Aggregate per five minute to ease comparison
    AG$timenum = as.numeric(AG$timestamp)
    AG$timenum = round(AG$timenum/300) * 300
    AG = aggregate(AG[,c("BFEN")], by =list(AG$timenum), FUN=mean)
    nag = names(AG)
    names(AG)[which(nag == "x")] = "BFEN"
    AG$timestamp = as.POSIXlt(AG$Group.1,  origin = "1970-1-1", tz="Europe/Amsterdam")
    
    # Load actometer data
    H = read.csv(fnames[i],skip=0,nrow=6, sep=" ",header=F)
    s0 = paste0(H[4,1]," ",H[5,1])
    s1 = paste0(H[2,1]," ",H[3,1])
    
    tmp = as.POSIXlt(s0,format = "%d-%m-%y %H:%M:%S", tz="Europe/Amsterdam")
    if (is.na(tmp)) tmp = as.POSIXlt(s0,format = "%d-%m-%Y %H:%M:%S", tz="Europe/Amsterdam")
    s0 = tmp
    tmp = as.POSIXlt(s1,format = "%d-%m-%y %H:%M:%S", tz="Europe/Amsterdam")
    if (is.na(tmp)) tmp = as.POSIXlt(s1,format = "%d-%m-%Y %H:%M:%S", tz="Europe/Amsterdam")
    s1 = tmp
    
    D = read.csv(fnames[i],skip=10, sep=" ",header=F)
    time = seq(s0,s1,by=300)
    if (length(time) > nrow(D)) {
      time = time[1:nrow(D)]
    } else if (length(time) < nrow(D)) {
      D = D[1:length(time),]
    }
    D$time = time
    # Align timerange
    actR = as.numeric(range(D$time))
    agR = as.numeric(range(AG$timestamp))
    xMAX = as.POSIXlt(min(c(actR[2],agR[2])), origin = "1970-1-1", tz="Europe/Amsterdam")
    xMIN = as.POSIXlt(max(c(actR[1],agR[1])), origin = "1970-1-1", tz="Europe/Amsterdam")
    D = D[which(D$time >= xMIN & D$time <= xMAX),]
    AG = AG[which(AG$timestamp>= xMIN & AG$timestamp <= xMAX),]
    
    if (nrow(AG) > 3000 & nrow(D) > 3000) {
      if (nrow(AG) > nrow(D)) AG = AG[1:nrow(D),]
      if (nrow(D) > nrow(AG)) D = D[1:nrow(AG),]
      
      # Plot
      actometer = D[,2]
      actigraph = AG$BFEN
      par(mfrow=c(2,1))
      if (performance == "correct") {
        colmain = "darkgreen"
      } else {
        colmain = "red"
      }
      plot(D$time,(actometer / sd(actometer)) + 6,type="l",col = "blue", main=paste0("Actometer (blue): ",label,", id: ",id,", Classification: ",performance),
           bty="l",xlab="time",ylab="activity",ylim=c(0,18), col.main=colmain)
      lines(AG$timestamp,actigraph / sd(actigraph),type="l",bty="l",xlab="time",ylab="activity",ylim=c(0,0.5))
      ccf(actometer,actigraph)
    }
  } 
}
dev.off()