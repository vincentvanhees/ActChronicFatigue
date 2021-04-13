#' summarise
#'
#' @param outputdir ...
#' @param part5_summary ...
#' @param model_threshold ...
#' @param referentiewaarden ...
#' @return no object is returned, only a summary is printed to the screen
#' @export
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics abline axis barplot par
#' @importFrom graphics legend lines text
summarise = function(outputdir, part5_summary, 
                     model_threshold = 75, referentiewaarden){
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
  part2_summary = part2_summary[,c("ID", "ENMO_fullRecordingMean")] #  "BFEN_fullRecordingMean" "ENMO_fullRecordingMean"
  colnames(part2_summary) = c("ID","Activity_zscore")
  part2_summary$Activity_zscore = round((part2_summary$Activity - referentiewaarden[1]) / referentiewaarden[2], digits=1) # - 23.64) / 15.5 for ENMO
  part5_summary = merge(part5_summary, part2_summary, by.x = "ID2", by.y = "ID")
  
  # most_recent_recordings = which(order(as.Date(Sys.time()) - 
  #                                        as.Date(part5_summary$calendar_date), decreasing = T) <= Nmostrecent)
  
  # outputdir = "/media/vincent/DATA/actometer_nkcv/output_nkcv_wrist"
  # check for which IDs reports have already been generated:
  filesinresults = dir(paste0(outputdir, "/results"))
  report_names = grep(pattern = "eweeg_en_slaap_rapport_", x = filesinresults, value = TRUE)
  IDdone =c()
  if (length(report_names) > 0) {
    extractID = function(x) {
      return(unlist(strsplit(x, "_rapport_|[.]pd"))[2])
    }
    IDdone = unlist(lapply(X = report_names, FUN =  extractID))
  }
  if (length(IDdone) > 0) {
    most_recent_recordings = which(part5_summary$ID2 %in% IDdone == FALSE)
  } else {
    most_recent_recordings = 1:nrow(part5_summary)
  }
  if (length(most_recent_recordings) == 0) {
    cat("\nDe bestanden waren al eerder genalyseerd en de rapporten staan in de resultaten. Mocht je de rapporten opnieuw willen creeeren, verwijder dan eerst de oude pdf bestanden.")
  } else {
    cat(paste0("Klassificaties voor meest recente ", length(most_recent_recordings), " metingen: "))
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
    recent_recording$classification = "Actief"
    pp = which(recent_recording$prop_perv_passive >= 50)
    if (length(pp) > 0) recent_recording$classification[pp] = "Laag Actief"
    colnames(recent_recording) = c("ID", "Datum start meeting", "Kans op laag actief (%)",
                                   "Gemiddelde beweging t.o.v. referentie groep (z-score)",
                                   "Uren slaap per nacht", "Gem. duur slaapperioden (uren)", 
                                   "Slaap efficiency binnen slaapperioden (%)", "Gemiddelde slaap start t.o.v. middernacht", 
                                   "Gemiddelde slaap einde t.o.v. middernacht", "Klassificatie") #,"feedback")
    
    varnames = c("Datum start meeting", "Klassificatie", "Kans op laag actief (%)",
                 "Gemiddelde beweging t.o.v. referentie groep (z-score)")
    recent_recording = recent_recording[,c(1:4,10,5:9)]
    cat("\n")
    part5_daysummary_file = grep(dir(paste0(outputdir,"/results"), full.names = TRUE),
                                 pattern = "part5_daysummary", value = T)
    part5_daysummary = read.csv(file=part5_daysummary_file, sep=",")
    # Load sleep/wake times from both GGIR and diary
    part4_nightsummaryfull_file = grep(dir(paste0(outputdir,"/results/QC"), full.names = TRUE),
                                       pattern = "part4_nightsummary_sleep_full.csv", value = T)
    part4_nightsummary = read.csv(part4_nightsummaryfull_file)
    
    days = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    dagen = c("maandag", "dinsdag", "woensdag", "donderdag", "vrijdag", "zaterdag", "zondag")
    for (ki in 1:length(days)) {
      part5_daysummary$weekday = gsub(x = part5_daysummary$weekday,
                                      pattern = days[ki], replacement = dagen[ki])
      part4_nightsummary$weekday = gsub(x = part4_nightsummary$weekday,
                                        pattern = days[ki], replacement = dagen[ki]) 
    }
    for (ID in unique(recent_recording$ID)) {
      P5D = part5_daysummary[which(part5_daysummary$ID == ID),]
      P4N = part4_nightsummary[which(part4_nightsummary$ID == ID),]
      #---------------------------------------------
      # add empty rows for missing days
      P5D$calendar_date = as.Date(P5D$calendar_date, "%Y-%m-%d")
      dates = P5D$calendar_date
      dates_theoretical = seq(min(dates), max(dates), by = 1)
      missing_dates = dates_theoretical[which(dates_theoretical %in% dates == FALSE)]
      if (length(missing_dates) > 0) {
        newvalues = (nrow(P5D)+1):(nrow(P5D)+length(missing_dates))
        P5D[newvalues,] = NA
        P5D$calendar_date[newvalues] = format(as.Date(missing_dates), "%Y-%m-%d")
        P5D$weekday[newvalues] = weekdays(missing_dates)
        P5D = P5D[order(P5D$calendar_date),]
        if (length(which(P5D$weekday[newvalues] %in% days)) > 0) {
          for (ki in 1:length(days)) {
            P5D$weekday = gsub(x = P5D$weekday,
                               pattern = days[ki], replacement = dagen[ki])
          }
        }
      }
      P5D$calendar_date = format(as.Date(P5D$calendar_date), "%d")
      # Now shorten weekday
      dagen_kort = c("ma", "di", "wo", "do", "vr", "za", "zo")
      # dagen = c("maandag", "dinsdag", "woensdag", "donderdag", "vrijdag", "zaterdag", "zondag")
      dagen = c("maandag", "dinsdag", "woensdag", "donderdag", "vrijdag", "zaterdag", "zondag")
      for (ki in 1:length(days)) {
        P5D$weekday = gsub(x = P5D$weekday,
                           pattern = dagen[ki], replacement = dagen_kort[ki]) 
        P4N$weekday = gsub(x = P4N$weekday,
                           pattern = dagen[ki], replacement = dagen_kort[ki]) 
      }
      # Extract person summary for part 5
      P5P = part5_summary[which(part5_summary$ID == ID),]
      
      pdffile = paste0(outputdir, "/results/Beweeg_en_slaap_rapport_",ID,".pdf")
      pdf(file = pdffile, paper = "a4r")
      par(mfrow=c(3,1), mar=c(4, 4, 3, 0.5),oma=c(0,0,0,0))  # las = 3
      # Key facts
      keystats = t(recent_recording[which(recent_recording$ID == ID), varnames])
      colnames(keystats)[1] = paste0("ID: ", ID)
      CX = 1.1
      CXdays = 1.0
      relprop = as.numeric(keystats[3])
      if (keystats[2] != "Laag Actief") relprop = 100 - relprop
      titel = paste0(ID," | Start: ", keystats[1]," | ",
                     keystats[2], " (kans=", relprop,"%) | ",
                     "beweging (z-score) = ", keystats[4])
      CL = 1.0
      CXdots = 1.5
      #========================================
      # Plot activity time series
      plot(P5D$ACC_day_mg, type = "b", pch=20, ylim = range(c(model_threshold * 2, P5D$ACC_day_mg), na.rm = T),
           main=titel, xlab = "", ylab="Beweging per dag", bty="l", axes=FALSE, cex.main=1.5, cex.lab=CL, cex= CXdots)
      axis(side=1, at=1:nrow(P5D), labels=paste0(P5D$weekday," ",P5D$calendar_date), cex.axis=CXdays)
      abline(h = model_threshold, col="black", lty=2, lwd=1.3)
      text(x = 0.8, y = model_threshold + 5, labels="grenswaarde laag actief", pos = 4)
      # par(mar=c(4, 4, 3, 0.5)) 
      #========================================
      # Plot sleep duration
      A = P5D[,c("dur_spt_sleep_min", "dur_spt_min")] / 60
      maxvalue = max(A[,2], na.rm = T)
      A[,2] = A[,2] - A[,1]
      brpos = barplot(t(as.matrix(A)), space=rep(0, nrow(A)), ylab="Tijd in uren", ylim=c(0, maxvalue + 1.5),cex.axis = 0.9,
                      names.arg = P5D$weekday, cex.names=CXdays, cex.lab=CL,
                      main="Slaap (donker), Wakker na start slaap (licht) en Slaap efficientie % na start slaap (nummers)")
      
      text(brpos, rowSums(A)+1, labels = round(100*(A[,1] / (rowSums(A))), digits = 0))
      
      #========================================
      # Niet gedragen tijd
      A = P5D[,c("nonwear_perc_day", "nonwear_perc_spt")]
      barplot(t(as.matrix(A)),  ylab="Percentage (%)", beside=TRUE, space=c(0,0.2),#rep(1, nrow(A)),
              names.arg = P5D$weekday, cex.names=CXdays, cex.lab=CL,
              main="Percentage van tijd beweegmeter niet gedragen", legend.text = c("overdag", "nacht"),
              args.legend=list(ncol=2), ylim=c(0,100))
      
      #========================================
      # Slaap waak tijden
      sleeponset_ts = P4N$sleeponset_ts # c("22:00", "23:20", "01:10")
      wakeup_ts = P4N$wakeup_ts # c("6:55", "8:30", "10:40")
      sleeponset_log_ts = P4N$guider_onset_ts # c("22:00", "23:20", "01:10")
      wakeup_log_ts = P4N$guider_wakeup_ts # c("6:55", "8:30", "10:40")
      WV = which(is.na(wakeup_ts) == FALSE & is.na(sleeponset_ts) == FALSE &
                   is.na(wakeup_log_ts) == FALSE & is.na(sleeponset_log_ts) == FALSE)
      sleeponset_ts = as.POSIXlt(sleeponset_ts[WV],format="%H:%M")
      wakeup_ts = as.POSIXlt(wakeup_ts[WV],format="%H:%M")
      sleeponset_log_ts = as.POSIXlt(sleeponset_log_ts[WV],format="%H:%M")
      wakeup_log_ts = as.POSIXlt(wakeup_log_ts[WV],format="%H:%M")
      
      for (j in 1:length(wakeup_ts)) {
        if (sleeponset_ts[j] < as.POSIXlt("18:00", format="%H:%M")) {
          sleeponset_ts[j] = sleeponset_ts[j] + (24 * 3600)
        }
        if (sleeponset_log_ts[j] < as.POSIXlt("18:00", format="%H:%M")) {
          sleeponset_log_ts[j] = sleeponset_log_ts[j] + (24 * 3600)
        }
        if (wakeup_ts[j] < sleeponset_ts[j] |
            (wakeup_ts[j] > as.POSIXlt("00:00", format="%H:%M")) & (wakeup_ts[j] < as.POSIXlt("12:00", format="%H:%M"))) {
          wakeup_ts[j] = wakeup_ts[j] + (24 * 3600)
        }
        if (wakeup_log_ts[j] < sleeponset_log_ts[j] |
            (wakeup_log_ts[j] > as.POSIXlt("00:00", format="%H:%M")) & (wakeup_log_ts[j] < as.POSIXlt("12:00", format="%H:%M"))) {
          wakeup_log_ts[j] = wakeup_log_ts[j] + (24 * 3600)
        }
        wakeup_ts[j] = wakeup_ts[j] - (24 * 3600)
        wakeup_log_ts[j] = wakeup_log_ts[j] - (24 * 3600)
      }
      YLIM = c(as.POSIXlt("12:00", format="%H:%M")- 12*3600, (as.POSIXlt("12:00", format="%H:%M") + 24*3600))
      timeaxis = c(as.numeric(YLIM[1]) + (c(0:36) * (3600)))
      timeaxislabels = format(as.POSIXlt(timeaxis, origin="1970-1-1"), "%H:%M")
      
      
      waki = 1:(length(wakeup_ts)-1)
      onsi = 2:length(sleeponset_ts)
      
      COL = c("blue", "red") 
      CL = 1
      par(mfrow=c(1,1), oma=c(0,0,0,0))
      plot(WV[waki], wakeup_ts[waki], type = "b", pch=16, xlab = "",  ylab = "",
           ylim=as.numeric(YLIM), axes=FALSE, main="Opsta- en bed-tijden", cex.lab=CL, cex=CXdots, col=COL[1])
      for (ii in -24:24) {
        abline(h = as.numeric(as.POSIXlt("24:00", format="%H:%M"))+(ii*3600), col="grey", lty=3 )
      }
      axis(side=1, at=waki, labels=P4N$weekday[onsi], cex.axis=CXdays)
      abline(v = waki, col="grey", lty=3 ) # vertical lines
      par(las=1)
      axis(side = 2, at = timeaxis, labels = as.character(timeaxislabels), cex.axis=1.0)
      lines(WV[waki], sleeponset_ts[onsi], type = "b", lty=1, pch=17, cex=CXdots, col=COL[1])
      lines(WV[waki], wakeup_log_ts[waki], type = "b", lty=1, pch=16, cex=CXdots, col=COL[2])
      lines(WV[waki], sleeponset_log_ts[onsi], type = "b", lty=1, pch=17, cex=CXdots, col= COL[2])
      legend("top", legend = c("beweegmeter opstatijden", "beweegmeter bedtijden",
                               "dagboek opstatijden", "dagboek bedtijden"),
             col = COL[c(1,1,2,2)], lty=rep(1,4), pch=c(16,17,16,17), ncol=2, cex= 0.8)
      dev.off()
      cat(paste0("\nDe PDF rapporten zijn nu opgeslagen in ",pdffile,"."))
    }
  }
}
