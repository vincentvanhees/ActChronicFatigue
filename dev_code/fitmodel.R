rm(list=ls())
graphics.off()
show.training.performance = FALSE
library(psych)
library(ROCR)
# library(pROC) Orchestrating privacy-protected big data
#=====================================================
# Input needed:
#=====================================================
outputdir = "/home/vincent/Dropbox/Work/W22/DATA/output_nkcv_wrist" # GGIR output directory
mydatadir = "/home/vincent/Dropbox/Work/W22/DATA/actometer_nkcv" # directory where the labels.csv file is stored
# Specify location of file:
part5_summary_file = grep(dir(paste0(outputdir,"/results"), full.names = TRUE),pattern = "part5_personsummary_WW_", value = T)
# Specify location of file with the labels
# This file needs to have the columns names: id, label, loc (loc refers to body location)
filewithlabels = paste0(mydatadir,"/labels.csv") # specify file location


# Specifiy what abbraviations are used (update the character string on the right side of the =-sign)
pervasivepassive = "pp"
pervasiveactive = "pa"
fluctuationactive = "fa"
hip = "hip"
wrist = "wrist"
id_column_labels = "ID" # specify here the name where you store the id values in the filewithlabels
id_column_part5 = "ID2" # specify here the name where you store the id values in the part2_summary.csv
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
  pred = ROCR::prediction(statspredict,data$label)
  opt.cut = function(perf, pred){
    cut.ind = mapply(FUN=function(x, y, p){
      d = (x - 0)^2 + (y-1)^2
      ind = which(d == min(d))[1]
      c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
        cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, pred@cutoffs)
  }
  roc.perf = ROCR::performance(pred, cost.fp = 1, cost.fn = 3, measure = "tpr", x.measure = "fpr") #, ) #
  threshold = opt.cut(roc.perf, pred)[3]
}

# load data
labels = read.csv(filewithlabels, sep=separator, stringsAsFactors = TRUE)

# labels = labels[which(labels$ID %in% c(62038, 62058, 60601, 62001, 62056, 62036, 62061) == FALSE),]
# labels = labels[which(labels$ID %in% c(62038, 62001, 62056, 61837) == FALSE),] 
# labels = labels[which(labels$ID %in% c(62056) == FALSE),]

labels$label[which(labels$label == "pa")] = "fa"
labels = labels[which(as.character(labels$label) %in% c("pp","fa")),] #, "pa"
labels = droplevels(labels)
D = read.csv(file=part5_summary_file, sep=separator)

# "gradient_mean", "y_intercept_mean", "X1", "X2", "X3", "X4",
D = D[,c("act9167", "ID2", "filename", "Nvaliddays","Ndays_used", # these are the number of days used by the model
         "nonwear_perc_day_spt_pla", "ACC_day_mg_pla", "nonwear_perc_day_pla", "calendar_date")]


D = D[which(D$nonwear_perc_day_spt_pla <= 33 & D$nonwear_perc_day_pla <= 33 & D$Ndays_used >= 12),] #& D$calib_err < 0.02 #& D$Nvaliddays > 10

# Merge data with labels
NmatchingIDs = length(which(labels[,id_column_labels] %in% D[,id_column_part5] == TRUE))

if (NmatchingIDs == 0) {
  print("No matching id could be found, please check that correct column is specified")
  print(paste0("format of id in labels: ", labels[1,id_column_labels]))
  print(paste0("format of id in part2_summary.csv: ",  D[1,id_column_part5]))
} else {
  labels = labels[!duplicated(labels),]
  D = D[!duplicated(D),]
  labels = labels[which(labels[,id_column_labels] %in% D[,id_column_part5] == TRUE),]
  D = D[which(D[,id_column_part5] %in% labels[,id_column_labels] == TRUE),]
  
}
MergedData = merge(labels, D,by.x=id_column_labels,by.y=id_column_part5)

rm(D)
findwinner = function(x) {
  y = x[c(fluctuationactive,pervasivepassive,pervasiveactive)]
  if (sort(y)[2] > 0.5) {
    winner = paste0(names(sort(y)[2:3]), collapse="") # call it a combined class
  } else {
    win = which.max(y)
    winner = names(win)
  }
  return(winner)
}

MergedData$label2 = MergedData$label
MergedData$label <- stats::relevel(MergedData$label, ref = fluctuationactive)
MergedData$label = as.integer(MergedData$label) - 1L

labelkey = unique(MergedData[,c("label","label2")])
colnames(labelkey) = c("value","label")
print(labelkey)
MergedData = MergedData[,-which(colnames(MergedData) == "label2")]
MergedData =  MergedData[!is.na(MergedData$act9167),]
# MergedData = MergedData[-which(MergedData$ID == 61207),]
for (location in c(wrist)) { #,hip
  cat("\n===============================")
  cnt = 1
  # select subset of one sensor location
  Dloc = MergedData[which(MergedData$loc == location),]
  for (testind in sample(nrow(Dloc))) {
    S = Dloc # create copy, because in the second iteration of the loop we will need the original data again
    # split into training and test set
    testset = S[testind,]
    S = S[-testind,] # training set
    
    # Fit model, this where we decide what variables will be used:
    if (location == wrist) {
      fit = glm(label ~ act9167, data = S, family = binomial) #
    }
    #=============================================================================================
    # predict class probability with logistic regression model:
    pp_testset <- stats::predict(object = fit, newdata = testset, type = "response")
    if (derive.roc.cutoff == TRUE) {
      # Do roc analysis, to find best threshold to favor sensitivity over specificity.
      threshold = derive_threshold(fit = fit, data = S)
      # Use threshold to get class prediction
      testset$estimate = ifelse(test = pp_testset < threshold, yes = 0, no = 1) # give more weight to pp
    } else {
      # Round to get class prediction:
      testset$estimate = round(pp_testset)
    }
    if (cnt == 1) {
      output = testset
    } else {
      output = rbind(output,testset)
    }
    cnt = cnt + 1
  }
  cat(paste0("\n",location,": overall performance on leave one out test set (rows are labels, columns estimates)"))
  print(table(output$label,output$estimate)) # estimate are columns, label are rows
  print(psych::cohen.kappa(table(output$label,output$estimate)))
  
  output$result = FALSE
  output$result[which(output$estimate == output$label)] = TRUE
  cat("\nIDs corresponding to errors:\n")
  cat(sort(output$ID[which(output$result == FALSE)]))
  cat("\n")
  cat(paste0("auc: ",round(pROC::auc(pROC::roc(output$label,output$estimate, )), digits = 4)))
  
}

MergedData_wrist = MergedData[which(MergedData$loc == wrist),]
final_model_wrist = glm(label ~ act9167, data = MergedData_wrist , family = binomial)
MergedData_wrist$pred = stats::predict(object = final_model_wrist, newdata = MergedData_wrist, type = "response")

threshold_wrist = threshold = 0.5

if (derive.roc.cutoff == TRUE) {
  threshold_wrist = derive_threshold(fit = final_model_wrist, data=  MergedData_wrist)
  cat("\n")
  cat(paste0("\nThresholds for final models: Wrist ",threshold_wrist))
}
# you can now save and load these finalmodels with the save() and load() function
# or inspect them with summary()
save(final_model_wrist, threshold_wrist, file = "inst/extdata/final_model_wrist.Rdata")

data2store = MergedData[which(MergedData$loc=="wrist"),
                        c("ID", "act9167", "nonwear_perc_day_spt_pla", "ACC_day_mg_pla")]
colnames(data2store) = c("ID","percentile9167_acceleration","accelerometer_worn_duration_days","average_acceleration")
write.csv(data2store,
          file = paste0(mydatadir, "/actigraph_summary_wrist.csv") , row.names = F)

#====================================================
# Exploration plots

# x11()
# plot(MergedData_wrist$act9167, MergedData_wrist$pred - MergedData_wrist$label, type="p", pch=20)

x11()
boxplot(MergedData_wrist$pred ~ MergedData_wrist$label, type="p", pch=20)

cat("\nMisclassified:\n")
# print(MergedData_wrist[which(MergedData_wrist$ID%in% sort(output$ID[which(output$result == FALSE)]) == TRUE),])
# print(MergedData_wrist[which(round(MergedData_wrist$pred) != MergedData_wrist$label),])
# print(MergedData_wrist[which(round(MergedData_wrist$pred) != MergedData_wrist$label),])
# print(MergedData_wrist[which(MergedData_wrist$ID %in% output$ID[which(output$label != output$estimate)] == TRUE),]) # estimate are columns, label are rows

output = output[order(output$calendar_date),]
print(output[which(output$label != output$estimate),
             which(colnames(output) %in% c("loc", "Nvaliddays", "Ndays_used",
                                           "nonwear_perc_day_spt_pla",
                                           "nonwear_perc_day_pla", "year",
                                           "result", "error") == FALSE) ]) # estimate are columns, label are rows
output$calendar_date = as.Date(output$calendar_date)
output$year = format(output$calendar_date,"%Y")
output$error = abs(output$estimate - output$label)
output = output[order(output$calendar_date),]
# x11()
# plot(as.Date(output$calendar_date), output$error,
#      type="p", pch=20, xlab="datum", ylab="misclassificatie")

# table(output[,c("year","error")])

