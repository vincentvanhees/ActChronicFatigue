#' summarise
#'
#' @param outputdir ...
#' @param part5_summary ...
#' @param Nmostrecent ...
#' @return no object is returned, only a summary is printed to the screen
#' @export

summarise = function(outputdir, part5_summary, Nmostrecent = 10){
  part2_summary_file = grep(dir(paste0(outputdir,"/results"), full.names = TRUE),
                            pattern = "part2_summary", value = T)
  part2_summary = read.csv(file=part2_summary_file, sep=",")
  part2_summary$ID = as.numeric(sapply(part2_summary$ID, FUN=function(x) unlist(strsplit(x," "))[1]))
  part2_summary = part2_summary[,c("ID", "ENMO_fullRecordingMean")]
  colnames(part2_summary) = c("ID","Activity_zscore")
  part2_summary$Activity_zscore = round((part2_summary$Activity - 23.64) / 15.5, digits=1)
  part5_summary = merge(part5_summary, part2_summary, by.x = "ID2", by.y = "ID")
  
  cat(paste0("Klassificaties voor meest recente ", Nmostrecent, " metingen: "))
  most_recent_recordings = which(order(as.Date(Sys.time()) - 
                                         as.Date(part5_summary$calendar_date), decreasing = T) <= Nmostrecent)
  part5_summary = part5_summary[order(as.Date(part5_summary$calendar_date)),]
  recent_recording = part5_summary[most_recent_recordings,
                                   c("ID2", "calendar_date", "prop_perv_passive", "Activity_zscore")]
  
  
  recent_recording$prop_perv_passive = round(recent_recording$prop_perv_passive* 100)
  recent_recording$classification = "not pervasively passive"
  pp = which(recent_recording$prop_perv_passive >= 50)
  if (length(pp) > 0) recent_recording$classification[pp] = "pervasively passive"
  
  # # add feedback
  # recent_recording$feedback = ""
  # ppbutnot = which(recent_recording$Activity_zscore > 0 & recent_recording$classification == "pervasively passive")
  # notbutpp = which(recent_recording$Activity_zscore < 1 & recent_recording$classification == "not pervasively passive")
  # if (length(ppbutnot) > 0) recent_recording$feedback[ppbutnot] = "mogelijk niet pervasively passive"
  # if (length(notbutpp) > 0) recent_recording$feedback[notbutpp] = "mogelijk toch wel pervasively passive"
  
  # recent_recording$prop_perv_passive = 100 - recent_recording$prop_perv_passive
  
  colnames(recent_recording) = c("ID", "Start meeting", "Kans op perv. passive (%)",
                                 "Klassificatie","Activiteit (z-score)") #,"feedback")
  cat("\n")
  print(recent_recording)
}