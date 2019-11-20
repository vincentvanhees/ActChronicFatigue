rm(list=ls())
graphics.off()
show.training.performance = FALSE
library(nnet)
library(psych)
#=====================================================
# Input needed:
#=====================================================
outputdir = "~/data/output_rawactigraph" # GGIR output directory
mydatadir = "~/data" # directory where the labels.csv file is stored
# Specify location of file:
part2_summary_file = paste0(outputdir,"/results/part2_summary.csv")
# Specify location of file with the labels
# This file needs to have the columns names: id, label, loc (loc refers to body location)
filewithlabels = paste0(mydatadir,"/labels.csv") # specify file location

# Specifiy what abbraviations are used (update the character string on the right side of the =-sign)
pervasivepassive = "pp"
pervasiveactive = "pa"
fluctuationactive = "fa"
hip = "hip"
wrist = "wrist"
id_column_labels = "id" # specify here the name where you store the id values in the filewithlabels
id_column_part2 = "ID2" # specify here the name where you store the id values in the part2_summary.csv

#====================================================
# load data
labels = read.csv(filewithlabels, sep=",")
labels$label[which(labels$label == "pa")] = "fa"
labels = labels[which(as.character(labels$label) %in% c("pp","fa")),] #, "pa"
labels = droplevels(labels)
D = read.csv(file=part2_summary_file, sep=",")

D = D[,c("act90", "gradient_mean", "y_intercept_mean", "ID2", "ID",
         "filename", "wear_dur_def_proto_day", "X1", "X2", "X3", "X4",
         "ENMO_fullRecordingMean", "calib_err")]
D = D[which(D$wear_dur_def_proto_day > 5 & D$calib_err < 0.01),]
# Merge data with labels
NmatchingIDs = length(which(labels[,id_column_labels] %in% D[,id_column_part2] == TRUE))
if (NmatchingIDs == 0) {
  print("No matching id could be found, please check that correct column is specified")
  print(paste0("format of id in labels: ", labels[1,id_column_labels]))
  print(paste0("format of id in part2_summary.csv: ",  D[1,id_column_part2]))
}
MergedData = merge(labels,D,by.x=id_column_labels,by.y=id_column_part2)

findwinner = function(x) {
  y = x[c(fluctuationactive,pervasivepassive,pervasiveactive)]
  if (sort(y)[2] > 0.45) {
    winner = paste0(names(sort(y)[2:3]), collapse="") # call it a combined class
  } else {
    win = which.max(y)
    winner = names(win)
  }
  return(winner)
}
for (location in c(wrist,hip)) {
  cat("\n===============================")
  cnt = 1
  # select subset of one sensor location
  D = MergedData[which(MergedData$loc == location),]
  D$label <- stats::relevel(D$label, ref = fluctuationactive)
  for (testind in sample(nrow(D))) {
    S = D # create copy, because in the second iteration of the loop we will need the original data again
    # split into training and test set
    testset = S[testind,]
    S = S[-testind,] # training set
    # Fit model, this where we decide what variables will be used:
    if (location == wrist) {
      fit <- nnet::multinom(label ~ gradient_mean + y_intercept_mean, data = S, trace = F) # + act90
    } else if (location == hip) {
      fit <- nnet::multinom(label ~ gradient_mean + y_intercept_mean, data = S, trace = F) #+gradient_mean + y_intercept_mean
    }
    # training performance:
    if (show.training.performance == TRUE) { 
      pp <- as.data.frame(stats::fitted(fit)) # three probabilities per person
      pp2 = round(pp,digits=4)
      S = cbind(S,pp2)
      S$estimate = apply(S,1, findwinner)
      S$estimate = as.factor(S$estimate)
      S$label = as.factor(S$label)
      S$estimate = factor(S$estimate, levels = 
                            c(pervasivepassive, fluctuationactive, pervasiveactive,
                              levels(S$estimate)[which(levels(S$estimate) %in%
                                                         c(pervasivepassive, 
                                                           fluctuationactive,
                                                           pervasiveactive) == FALSE)]))
      S$label = factor(S$label, levels = c(pervasivepassive, fluctuationactive, pervasiveactive,
                                           levels(S$estimate)[which(levels(S$estimate) %in%
                                                                      c(pervasivepassive,
                                                                        fluctuationactive,
                                                                        pervasiveactive) == FALSE)]))
      cat(paste0("\ntraining ",location))
      print(table(S$estimate, S$label))
    }
    # testing performance:
    pp_testset <- stats::predict(object=fit, newdata=testset)
    testset$estimate = pp_testset
    if (cnt == 1) {
      output = testset
    } else {
      output = rbind(output,testset)
    }
    cnt = cnt +1
  }
  cat(paste0("\n",location,": overall performance on leave one out test set (rows are labels, columns estimates)"))
  print(table(output$label,output$estimate)) # estimate are columns, label are rows
  print(psych::cohen.kappa(table(output$label,output$estimate)))
  output$result = FALSE
  output$result[which(output$estimate == output$label)] = TRUE
  cat("\nIDs corresponding to errors:\n")
  cat(output$id[which(output$result == FALSE)])
}

# fit model on all the data:
final_model_hip <- nnet::multinom(label ~ act90 +gradient_mean + y_intercept_mean,
                                  data = MergedData[which(MergedData$loc == hip),], trace = F)
final_model_wrist <- nnet::multinom(label ~ act90 +gradient_mean + y_intercept_mean, 
                                    data = MergedData[which(MergedData$loc == wrist),], trace = F)

# you can now save and load these finalmodels with the save() and load() function
# or inspect them with summary()
save(final_model_hip, file = paste0(mydatadir,"/final_model_hip.Rdata"))
save(final_model_wrist, file = paste0(mydatadir,"/final_model_wrist.Rdata"))


#====================================================
# Explorative plots


# x11()
# hpp = which(MergedData$label == "pp" & MergedData$loc == "hip")
# wpp = which(MergedData$label == "pp" & MergedData$loc == "wrist")
# hfa = which(MergedData$label == "fa" & MergedData$loc == "hip")
# wfa = which(MergedData$label == "fa" & MergedData$loc == "wrist")
# hpa = which(MergedData$label == "pa" & MergedData$loc == "hip")
# wpa = which(MergedData$label == "pa" & MergedData$loc == "wrist")
# par(mfrow=c(2,2))
# plot(MergedData$act90[hpp], MergedData$gradient_mean[hpp], type="p", pch=20, col="red",
#      xlim=range(MergedData$act90),ylim=range(MergedData$gradient_mean), xlab="Act90", ylab = "gradient", main="hip")
# lines(MergedData$act90[hfa], MergedData$gradient_mean[hfa], type="p", pch=20, col="green")
# lines(MergedData$act90[hpa], MergedData$gradient_mean[hpa], type="p", pch=20, col="blue")
# 
# plot(MergedData$act90[wpp], MergedData$gradient_mean[wpp], type="p", pch=20, col="red",
#      xlim=range(MergedData$act90),ylim=range(MergedData$gradient_mean), xlab="Act90", ylab = "gradient", main="wrist")
# lines(MergedData$act90[wfa], MergedData$gradient_mean[wfa], type="p", pch=20, col="green")
# lines(MergedData$act90[wpa], MergedData$gradient_mean[wpa], type="p", pch=20, col="blue")

# plot(MergedData$act90[hpp], MergedData$y_intercept_mean[hpp], type="p", pch=20, col="red",
#      xlim=range(MergedData$act90),ylim=range(MergedData$y_intercept_mean), xlab="Act90", ylab = "intercept", main="hip")
# lines(MergedData$act90[hfa], MergedData$y_intercept_mean[hfa], type="p", pch=20, col="green")
# lines(MergedData$act90[hpa], MergedData$y_intercept_mean[hpa], type="p", pch=20, col="blue")
# 
# plot(MergedData$act90[wpp], MergedData$y_intercept_mean[wpp], type="p", pch=20, col="red",
#      xlim=range(MergedData$act90),ylim=range(MergedData$y_intercept_mean), xlab="Act90", ylab = "intercept", main="wrist")
# lines(MergedData$act90[wfa], MergedData$y_intercept_mean[wfa], type="p", pch=20, col="green")
# lines(MergedData$act90[wpa], MergedData$y_intercept_mean[wpa], type="p", pch=20, col="blue")
# 
# plot(MergedData$ENMO_fullRecordingMean[wpp], MergedData$y_intercept_mean[wpp], type="p", pch=20, col="red",
#      xlim=range(MergedData$act90),ylim=range(MergedData$y_intercept_mean), xlab="Act90", ylab = "intercept", main="wrist")
# lines(MergedData$ENMO_fullRecordingMean[wfa], MergedData$y_intercept_mean[wfa], type="p", pch=20, col="green")
# lines(MergedData$ENMO_fullRecordingMean[wpa], MergedData$y_intercept_mean[wpa], type="p", pch=20, col="blue")
