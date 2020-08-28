rm(list=ls())

# Ask user to select directories
# Store choices
# Next time, ask user to reload choices from previous time
# B = easycsv::choose_dir()



gt3xdir = "/media/vincent/DATA/actometer_nkcv/calibratie_experiment_26Aug2020"
datadir = "/media/vincent/DATA/actometer_nkcv/rawactigraph_nkcv"
outputdir = "/media/vincent/DATA/actometer_nkcv"
# activitylog ="/media/vincent/DATA/actometer_nkcv/actometer_tijden/Actometer_tijden_volledig.csv"
# sleeplog = 
development.mode = TRUE
do.gt3x.conversion = FALSE
#=========================================
# Install code if not available:

if (development.mode == TRUE) {
  roxygen2::roxygenise()
  locationRcode = "/home/vincent/projects/ActChronicFatigue/R" 
  ffnames = dir(locationRcode) # creating list of filenames of scriptfiles to load
  for (i in 1:length(ffnames)) {
    source(paste(locationRcode,"/",ffnames[i],sep=""))
  }
} else {
  if ("ActChronicFatigue" %in% rownames(installed.packages()) == FALSE) {
    if ("devtools" %in% rownames(installed.packages()) == FALSE) {
      install.packages("devtools")
    }
    library("devtools")
    install_github("vincentvanhees/ActChronicFatigue", dependencies=TRUE)
  }
}

#=============================================================================
# Converteer all gt3x bestanden naar csv bestanden en plaats die in datadir
if (do.gt3x.conversion ==  TRUE) {
  gt3x_files_to_convert = dir(gt3xdir, full.names = T)
  if (length(gt3x_files_to_convert ) > 0) {
    for (gt3xfile in gt3x_files_to_convert ) {
      gt3x_to_csv(path = gt3xfile, outpath =datadir, gzip=T)
    }
  }
}
#=============================================================================
# If sleeplog exists convert sleeplog to expected format

#=============================================================================
# Start processing of raw accelerometer data with GGIR
runGGIR(datadir=datadir, outputdir = outputdir, mode = 1:5,
        do.report=c(2,4,5), overwrite=FALSE)

#=============================================================================
# Add extra variables, specifically needed for classification
tmp = unlist(strsplit(datadir,"/|\")"))
outputdir = paste0(outputdir,"/output_",tmp[length(tmp)])


addVariables5(outputdir=outputdir)

#=============================================================================
# Load the resulting part5_personsummary.csv bestand
part5_summary_file = grep(dir(paste0(outputdir,"/results"), full.names = TRUE),pattern = "part5_personsummary_WW_", value = T)
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

# # Check that estimate is matches with labels in our traininge data
# labels = read.csv("/media/vincent/DATA/actometer_nkcv/labels.csv")
# EV = merge(labels, part5_summary, by.x = "ID", by.y = "ID2")
# EV = EV[which(EV$loc == "wrist"),]
# EV$pp = 0
# EV$pp[which(EV$label == "pp")] = 1
# x11();plot(EV[,c("pp","prop_perv_passive")],type="p", pch=20)

# Save predictions
write.csv(part5_summary, file = part5_summary_file)


