#' summarise
#'
#' @param outputdir ...
#' @param part5_summary ...
#' @param Nmostrecent ...
#' @param model_threshold ...
#' @return no object is returned, only a summary is printed to the screen
#' @export
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics abline axis barplot par

summarise = function(outputdir, part5_summary, Nmostrecent = 10,
                     model_threshold = 75){
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
  
  part5_daysummary_file = grep(dir(paste0(outputdir,"/results"), full.names = TRUE),
                            pattern = "part5_daysummary", value = T)
  part5_daysummary = read.csv(file=part5_daysummary_file, sep=",")
  
  days = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  dagen = c("maandag", "dinsdag", "woensdag", "donderdag", "vrijdag", "zaterdag", "zondag")
  
  for (ki in 1:length(days)) {
    part5_daysummary$weekday = gsub(x = part5_daysummary$weekday,
                                    pattern = days[ki], replacement = dagen[ki]) 
  }
  for (ID in unique(recent_recording$ID)) {
    P5D = part5_daysummary[which(part5_daysummary$ID == ID),]
    P5P = part5_summary[which(part5_summary$ID == ID),]
    pdf(file = paste0(outputdir, "/results/report_",ID,".pdf"), paper = "a4")
    par(mfrow=c(2,1), las = 3)
    plot(P5D$ACC_day_mg, type = "b", pch=20, 
         main=paste0("ID ", ID), xlab = "", ylab="Beweging per dag", bty="l", axes=FALSE)
    axis(side=1, at=1:nrow(P5D), labels=P5D$weekday, cex.axis=0.7)
    abline(h = model_threshold, col="red")
    
    A = P5D[,c("dur_spt_sleep_min", "dur_spt_min")] / 60
    A[,2] = A[,2] - A[,1]
    barplot(t(as.matrix(A)), space=rep(0, nrow(A)), ylab="slaaplengte (uren)",
              names.arg = P5D$weekday)
    
    dev.off()
  }
  
}