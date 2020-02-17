rm(list=ls())
graphics.off()
show.training.performance = FALSE
library(psych)
library(ROCR)

#=====================================================
# Input needed:
#=====================================================
outputdir = "/media/vincent/DATA/actometer_nkcv/output_rawactigraph_nkcv" # GGIR output directory
mydatadir = "/media/vincent/DATA/actometer_nkcv" # directory where the labels.csv file is stored
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
separator = "," # Note: replace by "\t" if you are working with tab seperated data
derive.roc.cutoff = TRUE # Optimize cut-off for classification

#====================================================
# declare function
derive_threshold = function(fit, data) {
  # False negatives (missing pervasively passive) will be rated twice as
  # costly as false positives (incorrect classifying as pervasively passive)
  # Sensitivity for pp will go up, specificity will go down
  # https://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/
  # Note: If cost.fp = 1 and cost.fn =1, then threshold is still optimized, but with equal importance
  statspredict <- stats::predict(object = fit, newdata = data, type = "response")
  pred = prediction(statspredict,data$label)
  opt.cut = function(perf, pred){
    cut.ind = mapply(FUN=function(x, y, p){
      d = (x - 0)^2 + (y-1)^2
      ind = which(d == min(d))
      c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
        cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, pred@cutoffs)
  }
  roc.perf = performance(pred, cost.fp = 1, cost.fn = 2, measure = "tpr", x.measure = "fpr") #, ) #
  threshold = opt.cut(roc.perf, pred)[3]
}

# load data
labels = read.csv(filewithlabels, sep=separator)
labels$label[which(labels$label == "pa")] = "fa"
labels = labels[which(as.character(labels$label) %in% c("pp","fa")),] #, "pa"
labels = droplevels(labels)
D = read.csv(file=part2_summary_file, sep=separator)

D = D[,c("act9167", "gradient_mean", "y_intercept_mean", "ID2", "ID",
         "filename", "wear_dur_def_proto_day", "X1", "X2", "X3", "X4",
         "ENMO_fullRecordingMean", "calib_err")]
D = D[which(D$wear_dur_def_proto_day > 11 & D$calib_err < 0.01),]
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
MergedData$label <- stats::relevel(MergedData$label, ref = fluctuationactive)
MergedData$label = as.integer(MergedData$label) - 1L

for (location in c(wrist,hip)) {
  cat("\n===============================")
  cnt = 1
  # select subset of one sensor location
  D = MergedData[which(MergedData$loc == location),]
  
  for (testind in sample(nrow(D))) {
    S = D # create copy, because in the second iteration of the loop we will need the original data again
    # split into training and test set
    testset = S[testind,]
    S = S[-testind,] # training set
    # Fit model, this where we decide what variables will be used:
    if (location == wrist) {
      # fit = glm(label ~ gradient_mean + y_intercept_mean, data = S, family = binomial)
      fit = glm(label ~ act9167, data = S, family = binomial)
    } else if (location == hip) {
      # fit = glm(label ~ gradient_mean + y_intercept_mean, data = S, family = binomial)
      fit = glm(label ~ act9167, data = S, family = binomial)
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
    #=============================================================================================
    # predict class probability with logistic regression model:
    pp_testset <- stats::predict(object = fit, newdata = testset, type = "response")
    if (derive.roc.cutoff == TRUE) {
      # Do roc analysis, to find best threshold to favor sensitivity over specificity.
      threshold = derive_threshold(fit =fit, data=  S)
      # Use threshold to get class prediction
      testset$estimate = ifelse(test = pp_testset < threshold, yes = 0, no = 1) # give more weight to pp
    } else {
      # Round to get class prediction:
      testset$estimate = round(test = pp_testset)
    }
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
# final_model_hip = glm(label ~ gradient_mean + y_intercept_mean, data = MergedData[which(MergedData$loc == hip),], family = binomial)
# final_model_wrist = glm(label ~ gradient_mean + y_intercept_mean, data = MergedData[which(MergedData$loc == wrist),], family = binomial)
final_model_hip = glm(label ~ act9167, data = MergedData[which(MergedData$loc == hip),], family = binomial)
final_model_wrist = glm(label ~ act9167, data = MergedData[which(MergedData$loc == wrist),], family = binomial)

threshold_hip = threshold = 0.5

if (derive.roc.cutoff == TRUE) {
  threshold_hip = derive_threshold(fit = final_model_hip, data=  MergedData[which(MergedData$loc == hip),])
  threshold_wrist = derive_threshold(fit = final_model_wrist, data=  MergedData[which(MergedData$loc == wrist),])
  cat("\n")
  cat(paste0("\nThresholds for final models: Hip ",threshold_hip,", Wrist ",threshold_wrist))
}
# you can now save and load these finalmodels with the save() and load() function
# or inspect them with summary()
save(final_model_hip, threshold_hip, file = paste0(mydatadir,"/final_model_hip.Rdata"))
save(final_model_wrist, threshold_wrist, file = paste0(mydatadir,"/final_model_wrist.Rdata"))




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
# plot(MergedData$act9167[hpp], MergedData$gradient_mean[hpp], type="p", pch=20, col="red",
#      xlim=range(MergedData$act9167),ylim=range(MergedData$gradient_mean), xlab="act9167", ylab = "gradient", main="hip")
# lines(MergedData$act9167[hfa], MergedData$gradient_mean[hfa], type="p", pch=20, col="green")
# lines(MergedData$act9167[hpa], MergedData$gradient_mean[hpa], type="p", pch=20, col="blue")
# 
# plot(MergedData$act9167[wpp], MergedData$gradient_mean[wpp], type="p", pch=20, col="red",
#      xlim=range(MergedData$act9167),ylim=range(MergedData$gradient_mean), xlab="act9167", ylab = "gradient", main="wrist")
# lines(MergedData$act9167[wfa], MergedData$gradient_mean[wfa], type="p", pch=20, col="green")
# lines(MergedData$act9167[wpa], MergedData$gradient_mean[wpa], type="p", pch=20, col="blue")

# plot(MergedData$act9167[hpp], MergedData$y_intercept_mean[hpp], type="p", pch=20, col="red",
#      xlim=range(MergedData$act9167),ylim=range(MergedData$y_intercept_mean), xlab="act9167", ylab = "intercept", main="hip")
# lines(MergedData$act9167[hfa], MergedData$y_intercept_mean[hfa], type="p", pch=20, col="green")
# lines(MergedData$act9167[hpa], MergedData$y_intercept_mean[hpa], type="p", pch=20, col="blue")
# 
# plot(MergedData$act9167[wpp], MergedData$y_intercept_mean[wpp], type="p", pch=20, col="red",
#      xlim=range(MergedData$act9167),ylim=range(MergedData$y_intercept_mean), xlab="act9167", ylab = "intercept", main="wrist")
# lines(MergedData$act9167[wfa], MergedData$y_intercept_mean[wfa], type="p", pch=20, col="green")
# lines(MergedData$act9167[wpa], MergedData$y_intercept_mean[wpa], type="p", pch=20, col="blue")
# 
# plot(MergedData$ENMO_fullRecordingMean[wpp], MergedData$y_intercept_mean[wpp], type="p", pch=20, col="red",
#      xlim=range(MergedData$act9167),ylim=range(MergedData$y_intercept_mean), xlab="act9167", ylab = "intercept", main="wrist")
# lines(MergedData$ENMO_fullRecordingMean[wfa], MergedData$y_intercept_mean[wfa], type="p", pch=20, col="green")
# lines(MergedData$ENMO_fullRecordingMean[wpa], MergedData$y_intercept_mean[wpa], type="p", pch=20, col="blue")
