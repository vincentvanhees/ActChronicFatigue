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
  extractid = function(x) {
    tmp = unlist(strsplit(x," "))
    if (length(tmp) > 1) {
      tmp = tmp[1]
    } else {
      tmp = x
    }
    return(tmp)
  }
  if (is.character(part2_summary$ID) == TRUE) {  
    part2_summary$ID = as.numeric(sapply(part2_summary$ID, FUN=extractid))
  }
  part2_summary = part2_summary[,c("ID", "ENMO_fullRecordingMean")]
  colnames(part2_summary) = c("ID","Activity_zscore")
  part2_summary$Activity_zscore = round((part2_summary$Activity - 23.64) / 15.5, digits=1)
  part5_summary = merge(part5_summary, part2_summary, by.x = "ID2", by.y = "ID")
  
  cat(paste0("Klassificaties voor meest recente ", Nmostrecent, " metingen: "))
  most_recent_recordings = which(order(as.Date(Sys.time()) - 
                                         as.Date(part5_summary$calendar_date), decreasing = T) <= Nmostrecent)
  part5_summary = part5_summary[order(as.Date(part5_summary$calendar_date)),]
  recent_recording = part5_summary[most_recent_recordings,
                                   c("ID2", "calendar_date", "prop_perv_passive", "Activity_zscore",
                                     "dur_spt_sleep_min_pla", "dur_spt_min_pla", "sleep_efficiency_pla",
                                     "sleeponset_pla", "wakeup_pla")]
  recent_recording$dur_spt_sleep_min_pla = round(recent_recording$dur_spt_sleep_min_pla / 60, digits=2)
  recent_recording$dur_spt_min_pla = round(recent_recording$dur_spt_min_pla / 60, digits=2)
  recent_recording$sleep_efficiency_pla =  round(recent_recording$sleep_efficiency_pla * 100, digits=0)
  recent_recording$sleeponset_pla = round(recent_recording$sleeponset_pla - 24, digits=2)
  recent_recording$wakeup_pla = round(recent_recording$wakeup_pla - 24, digits=2)
  
  recent_recording$prop_perv_passive = round(recent_recording$prop_perv_passive* 100)
  recent_recording$classification = "normaal of hoog actief"
  pp = which(recent_recording$prop_perv_passive >= 50)
  if (length(pp) > 0) recent_recording$classification[pp] = "laag actief"
  
  # # add feedback
  # recent_recording$feedback = ""
  # ppbutnot = which(recent_recording$Activity_zscore > 0 & recent_recording$classification == "pervasively passive")
  # notbutpp = which(recent_recording$Activity_zscore < 1 & recent_recording$classification == "not pervasively passive")
  # if (length(ppbutnot) > 0) recent_recording$feedback[ppbutnot] = "mogelijk niet pervasively passive"
  # if (length(notbutpp) > 0) recent_recording$feedback[notbutpp] = "mogelijk toch wel pervasively passive"
  
  # recent_recording$prop_perv_passive = 100 - recent_recording$prop_perv_passive
  
  colnames(recent_recording) = c("ID", "Start meeting", "Kans op laag actief (%)",
                                 "Activiteit (z-score)",
                                 "Gem. slaap binnen slaapperioden (uren)", "Gem. duur slaapperioden (uren)", 
                                 "Slaap efficiency binnen slaapperioden (%)", "Gemiddelde slaap start t.o.v. middernacht", 
                                 "Gemiddelde slaap einde t.o.v. middernacht", "Klassificatie") #,"feedback")
  recent_recording = recent_recording[,c(1:4,10,5:9)]
  cat("\n")
  print(recent_recording)
}