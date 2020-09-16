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
  recent_recording$classification = "Normaal Actief"
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
    #---------------------------------------------
    # add empty rows for missing days
    P5D$calendar_date = as.Date(P5D$calendar_date, "%d-%m-%Y")
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
    # Now shorten weekday
    dagen_kort = c("ma", "di", "wo", "do", "vr", "za", "zo")
    dagen = c("maandag", "dinsdag", "woensdag", "donderdag", "vrijdag", "zaterdag", "zondag")
    for (ki in 1:length(days)) {
      P5D$weekday = gsub(x = P5D$weekday,
                         pattern = dagen[ki], replacement = dagen_kort[ki]) 
    }
    # Extract person summary for part 5
    P5P = part5_summary[which(part5_summary$ID == ID),]
    
    
    pdf(file = paste0(outputdir, "/results/report_",ID,".pdf"), paper = "a4r")
    # par(mfrow=c(4, 1), las = 3, oma = rep(0.1,4), mar=c(5, 5, 3, 0.1), omi=rep(0.1, 4))
    par(mfrow=c(3,1), mar=c(4, 4, 3, 0.5),oma=c(0,0,0,0))  # las = 3
    # Key facts
    keystats = t(recent_recording[which(recent_recording$ID == ID), varnames])
    colnames(keystats)[1] = paste0("ID: ", ID)
    # plot(0:1,0:1, axes=F, col="white", xlab="",ylab="", 
    #      main = "")
    CX = 1.1
    CXdays = 1.1
    relprop = as.numeric(keystats[3])
    if (keystats[2] != "Laag Actief") relprop = 100 - relprop
    
    titel = paste0("ID ",ID," | ", keystats[1]," | ",
                   keystats[2], " (kans = ", relprop, "%) | ",
                   "z-score gem. beweging ", keystats[4])
    
    # paste0("ID: ", ID," --- Datum start meeting: ",keystats[1])
    # FNT = 2
    # text(x = 0.95, y = 0.9, labels = paste0("ID: ", ID),
    #      pos = 2, col="black", cex = CX, font = FNT )
    # text(x = 0.95, y = 0.65, labels = paste0(rownames(keystats)[1], ": ", keystats[1]),
    #      pos = 2, col="black", cex = CX, font = FNT )
    # 
    # text(x = 0.95, y = 0.4, labels = paste0(rownames(keystats)[2], ": ", keystats[2], " (kans = ", relprop, "%)"),
    #      pos = 2, col="black", cex = CX, font = FNT )
    # text(x = 0.95, y = 0.15, labels = paste0(rownames(keystats)[4], ": ", keystats[4]),
    #      pos = 2, col="black", cex = CX, font  =FNT )
    CL = 1.1
    CXdots = 1.5
    #========================================
    # Plot activity time series
    plot(P5D$ACC_day_mg, type = "b", pch=20, ylim = range(c(model_threshold * 2, P5D$ACC_day_mg), na.rm = T),
         main=titel, xlab = "", ylab="Beweging per dag", bty="l", axes=FALSE, cex.main=1.5, cex.lab=CL, cex= CXdots)
    axis(side=1, at=1:nrow(P5D), labels=P5D$weekday, cex.axis=CXdays)
    abline(h = model_threshold, col="black", lty=2, lwd=1.3)
    # par(mar=c(4, 4, 3, 0.5)) 
    #========================================
    # Slaap waak tijden
    sleeponset_ts = P5D$sleeponset_ts # c("22:00", "23:20", "01:10")
    wakeup_ts = P5D$wakeup_ts # c("6:55", "8:30", "10:40")
    WV = which(is.na(wakeup_ts) == FALSE & is.na(sleeponset_ts) == FALSE)
    sleeponset_ts = as.POSIXlt(sleeponset_ts[WV],format="%H:%M")
    wakeup_ts = as.POSIXlt(wakeup_ts[WV],format="%H:%M")
    for (j in 1:length(wakeup_ts)) {
      if (sleeponset_ts[j] < as.POSIXlt("18:00", format="%H:%M")) {
        sleeponset_ts[j] = sleeponset_ts[j] + (24 * 3600)
      }
      if (wakeup_ts[j] < sleeponset_ts[j] | 
          (wakeup_ts[j] > as.POSIXlt("00:00", format="%H:%M")) & (wakeup_ts[j] < as.POSIXlt("12:00", format="%H:%M"))) {
        wakeup_ts[j] = wakeup_ts[j] + (24 * 3600)
      }
      
    }
    YLIM = c(as.POSIXlt("12:00", format="%H:%M"), (as.POSIXlt("18:00", format="%H:%M") + 24*3600))
    timeaxis = c(as.numeric(YLIM[1]) + (c(0, 6, 12, 18, 24, 30) * (3600)))
    timeaxislabels = format(as.POSIXlt(timeaxis, origin="1970-1-1"), "%H:%M")
    plot(WV, wakeup_ts, type = "p", pch=20, xlab = "",  ylab = "",
         ylim=as.numeric(YLIM), axes=FALSE, main="Slaap- en waak-tijden van langste slaap periode", cex.lab=CL, cex=CXdots)
    abline(h = as.numeric(as.POSIXlt("24:00", format="%H:%M")), col="black", lty=2 )
    abline(h = as.numeric(as.POSIXlt("24:00", format="%H:%M"))-(6*3600), col="black", lty=2 )
    abline(h = as.numeric(as.POSIXlt("24:00", format="%H:%M"))+(6*3600), col="black", lty=2 )
    abline(h = as.numeric(as.POSIXlt("24:00", format="%H:%M"))+(12*3600), col="black", lty=2 )
    axis(side=1, at=1:nrow(P5D), labels=P5D$weekday, cex.axis=CXdays)
    par(las=1)
    axis(side = 2, at = timeaxis, labels = as.character(timeaxislabels), cex.axis=1.0)
    lines(WV, sleeponset_ts, type = "p", pch=20, cex=CXdots)
    
    #========================================
    # Plot sleep duration
    A = P5D[,c("dur_spt_sleep_min", "dur_spt_min")] / 60
    A[,2] = A[,2] - A[,1]
    barplot(t(as.matrix(A)), space=rep(0, nrow(A)), ylab="Slaapduur (uren)",
            names.arg = P5D$weekday, cex.names=CXdays, cex.lab=CL)
    
    dev.off()
  }
  
}
