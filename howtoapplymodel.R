rm(list=ls())

# Step 1: process raw accelerometer data with runGGIR.R

# Step 2: apply addVariables_v4.R

# Step 3: Load the resulting part2_summary.csv bestand

part2_summary_file = "~/data/output_rawactigraph/results/part2_summary.csv" #part2_daysummary.csv
part2_summary = read.csv(file=part2_summary_file, sep=",")

# If all went well this part2_summary object will have a column act90, gradient_mean, towards the end.

# Step 4: Load the trained model
load("~/data/final_model_hip.Rdata")

# Step 5: Make predictions by applying the model
mypredictions = stats::predict(object=final_model_hip, newdata=part2_summary)
# Note that we assumed that all data in part2_summary comes from accelerometer
# worn on the hip, because that is what the model was trained for.

# If all went well object mypredictions will have the predictions.

# Step 6: Save predictions
write.csv(mypredictions, file = "~/data/mypredictions.csv")
