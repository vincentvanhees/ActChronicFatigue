rm(list=ls())

datadir = "/media/vincent/DATA/actometer_nkcv/rawactigraph_nkcv"
outputdir = "/media/vincent/DATA/actometer_nkcv"
activitylog ="/media/vincent/DATA/actometer_nkcv/actometer_tijden/Actometer_tijden_volledig.csv"

#=========================================

# TO DO: 
# - Add QC on output from activity log, GGIR, addVariables
# - Show classification for most recently processed recording
# - Make option to choose hip or wrist
# - Make it possible to avoid using an activitylog
# - Also show other descriptives
# - Turn code into installable package

# Install code if not available:
# if ("actometer_experiments" %in% rownames(installed.packages()) == FALSE) {
#   if ("devtools" %in% rownames(installed.packages()) == FALSE) {
#     install.packages("devtools")
#   }
#   library("devtools")
#   install_github("vincentvanhees/actometer_experiments")
# }
setwd(".")
source("runGGIR.R")
source("convert_diary.R")
source("addVariables.R")
modelfile = "final_model_hip.Rdata"

# Step 1: Start processing of raw accelerometer data with GGIR
runGGIR(datadir=datadir, outputdir = outputdir, activitylog = activitylog, mode = 1)

# Step 2: In the mean time convert diary into file, and safe file
cat("\n")
readline(prompt="Press [enter] if you sleep diary file has been created")
if (file.exists(activitylog) == FALSE) {
  warning("Sleep diary file as specified does not exists.")
}
# Step 3: Load and convert time diary
convert_diary(activitylog = activitylog)

# Step 4: Continue processing with GGIR
runGGIR(datadir=datadir, outputdir = outputdir, activitylog = activitylog, mode = 2)

# Step 5: apply addVariables.R
tmp = unlist(strsplit(datadir,"/|\")"))
outputdir = paste0(outputdir,"/output_",tmp[length(tmp)])
addVariables(outputdir=outputdir)

# Step 6: Load the resulting part2_summary.csv bestand
part2_summary_file = paste0(outputdir,"/results/part2_summary.csv")
part2_summary = read.csv(file=part2_summary_file, sep=",")

# If all went well this part2_summary object will have a column act90, gradient_mean, towards the end.

# Step 7: Load the trained model
load(modelfile)

# Step 8: Make predictions by applying the model
mypredictions = stats::predict(object=final_model_hip, newdata=part2_summary)

# Note that we assumed that all data in part2_summary comes from accelerometer
# worn on the hip, because that is what the model was trained for.
# If all went well object mypredictions will have the predictions.

# Step 9: Save predictions
write.csv(mypredictions, file = paste0(outputdir,"/results/mypredictions.csv"))

