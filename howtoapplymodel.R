rm(list=ls())

gt3xdir = "/media/vincent/DATA/actometer_nkcv/calibratie_experiment_26Aug2020"
datadir = "/media/vincent/DATA/actometer_nkcv/rawactigraph_nkcv"
outputdir = "/media/vincent/DATA/actometer_nkcv"
activitylog ="/media/vincent/DATA/actometer_nkcv/actometer_tijden/Actometer_tijden_volledig.csv"
# sleeplog = 
development.mode = TRUE

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
gt3x_files_to_convert = dir(gt3xdir, full.names = T)
if (length(gt3x_files_to_convert ) > 0) {
  for (gt3xfile in gt3x_files_to_convert ) {
    gt3x_to_csv(path = gt3xfile, outpath =datadir, gzip=T)
  }
}

#=============================================================================
# If sleeplog exists convert sleeplog to expected format


#=============================================================================
# Start processing of raw accelerometer data with GGIR
runGGIR(datadir=datadir, outputdir = outputdir, activitylog = activitylog, mode = 1:5)

#=============================================================================
# Add extra variables, specifically needed for classification
tmp = unlist(strsplit(datadir,"/|\")"))
outputdir = paste0(outputdir,"/output_",tmp[length(tmp)])
addVariables(outputdir=outputdir)

#=============================================================================
# Load the resulting part5_personsummary.csv bestand
part2_summary_file = paste0(outputdir,"/results/part2_summary.csv")
part2_summary = read.csv(file=part2_summary_file, sep=",")

# If all went well this part2_summary object will have a column act90, gradient_mean, towards the end.

#=============================================================================
# Load the trained model
modelfile = "inst/extdata/final_model_hip.Rdata"
load(modelfile)
# Make predictions by applying the model
mypredictions = stats::predict(object=final_model_hip, newdata=part2_summary)

# Note that we assumed that all data in part2_summary comes from accelerometer
# worn on the hip, because that is what the model was trained for.
# If all went well object mypredictions will have the predictions.

# Save predictions
write.csv(mypredictions, file = paste0(outputdir,"/results/mypredictions.csv"))

