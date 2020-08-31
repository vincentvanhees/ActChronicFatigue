rm(list=ls())

# Ask user to select directories
# Store choices
# Next time, ask user to reload choices from previous time



development.mode = TRUE

#=========================================
# Install code if not available:


if (development.mode == TRUE) {
  roxygen2::roxygenise()
  do.gt3x.conversion = FALSE
  locationRcode = "/home/vincent/projects/ActChronicFatigue/R" 
  ffnames = dir(locationRcode) # creating list of filenames of scriptfiles to load
  for (i in 1:length(ffnames)) {
    source(paste(locationRcode,"/",ffnames[i],sep=""))
  }
} else { # install code from GitHub
  do.gt3x.conversion = TRUE
  install_again = FALSE
  if ("ActChronicFatigue" %in% rownames(installed.packages()) == TRUE) {
    Q1 = menu(c("Ja", "Nee"), title="Wil je de software opnieuw installeren?")
    if (Q1 == 1) {
      install_again = TRUE
    }
  }
  if ("ActChronicFatigue" %in% rownames(installed.packages()) == FALSE | install_again == TRUE) {
    if ("devtools" %in% rownames(installed.packages()) == FALSE) {
      install.packages("devtools")
    }
    library("devtools")
    install_github("vincentvanhees/ActChronicFatigue", dependencies=TRUE)
  }
}


datalocaties = optain_folder_paths() # Obtain folder paths from user
gt3xdir = datalocaties$gt3xdir
datadir = datalocaties$datadir
outputdir = datalocaties$outputdir

cat("Bestand locaties:\n")
cat(paste0("\nLocatie ",length(dir(datalocaties$gt3xdir))," gt3x bestanden = ",datalocaties$gt3xdir))
cat(paste0("\nLocatie ",length(dir(datalocaties$datadir))," csv bestanden = ",datalocaties$datadir))
cat(paste0("\nLocatie resultaten =  ",datalocaties$outputdir,"\n"))

if (length(dir(datalocaties$gt3xdir)) == 0 & length(dir(datalocaties$datadir)) == 0) {
  stop("\nGeen data gevonden. Controleer data folders.")
}



#=============================================================================
# Converteer all gt3x bestanden naar csv bestanden en plaats die in datadir
if (do.gt3x.conversion ==  TRUE) {
  gt3x_files_to_convert = dir(gt3xdir, full.names = T)
  if (length(gt3x_files_to_convert ) > 0) {
    for (gt3xfile in gt3x_files_to_convert ) {
      cat(paste0("\nConverteer ",basename(gt3xfile)," -> .csv"))
      gt3x_to_csv(path = gt3xfile, outpath =datadir, gzip=T)
    }
  }
}
#=============================================================================
# If sleeplog exists convert sleeplog to expected format

#=============================================================================
# Start processing of raw accelerometer data with GGIR
cat("\nStart analyse met GGIR...")
runGGIR(datadir=datadir, outputdir = outputdir, mode = c(),
        do.report=c(5), overwrite=FALSE, do.visual = FALSE, visualreport=FALSE)

#=============================================================================
# Add extra variables, specifically needed for classification
cat("\nExpand GGIR part5 output with extra variables...")
tmp = unlist(strsplit(datadir,"/|\")"))
outputdir = paste0(outputdir,"/output_",tmp[length(tmp)])
addVariables5(outputdir=outputdir)

#=============================================================================
# Load the resulting part5_personsummary.csv bestand
cat("\nApply pre-trained model to the data...")
part5_summary_file = grep(dir(paste0(outputdir,"/results"), full.names = TRUE),
                          pattern = "part5_personsummary_WW_", value = T)
part5_summary = read.csv(file=part5_summary_file, sep=",")

# If all went well this part2_summary object will have a column act90, gradient_mean, towards the end.

#=============================================================================
# Load the trained model
modelfile = "inst/extdata/final_model_wrist.Rdata"
load(modelfile)
# Make predictions by applying the model
prop_perv_passive = stats::predict(object=final_model_wrist, newdata=part5_summary, type="response")
part5_summary = cbind(part5_summary, prop_perv_passive)

# Note that we assumed that all data in part2_summary comes from accelerometer
# worn on the hip, because that is what the model was trained for.
# If all went well object prop_perv_passive will have the predictions.

# Check that estimate is matches with labels in our traininge data
# labels = read.csv("/media/vincent/DATA/actometer_nkcv/labels.csv")
##part5_summary part5_summary = part5_summary[which(part5_summary$ID2 %in% labels$ID[which(labels$loc == "wrist")]),]
# EV = merge(labels, part5_summary, by.x = "ID", by.y = "ID2")
# EV = EV[which(EV$loc == "wrist"),]
# EV$pp = 0
# EV$pp[which(EV$label == "pp")] = 1
# x11();plot(EV[,c("pp","prop_perv_passive")],type="p", pch=20)

# Save predictions
write.csv(part5_summary, file = part5_summary_file)

# Extract additional information from part 2:
part2_summary_file = grep(dir(paste0(outputdir,"/results"), full.names = TRUE),
                          pattern = "part2_summary", value = T)
part2_summary = read.csv(file=part2_summary_file, sep=",")
part2_summary$ID = as.numeric(sapply(part2_summary$ID, FUN=function(x) unlist(strsplit(x," "))[1]))
part2_summary = part2_summary[,c("ID", "ENMO_fullRecordingMean")]
colnames(part2_summary) = c("ID","Activity_zscore")
part2_summary$Activity_zscore = round((part2_summary$Activity - 23.64) / 15.5, digits=1)
part5_summary = merge(part5_summary,part2_summary, by.x = "ID2", by.y = "ID")

cat("Klassificaties voor meest recente tien metingen: ")
most_recent_recordings = which(order(as.Date(Sys.time()) - 
                                       as.Date(part5_summary$calendar_date), decreasing = T) <= 10)
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
