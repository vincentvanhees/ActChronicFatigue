rm(list=ls())
#==========================================================
# Input needed:
outputdir = "~/data/output_rawactigraph" # specify output directory
separator = "," # Note: replace by "\t" if you are working with tab seperated data

#==========================================================
# load data
part2_daysummary_file = paste0(outputdir,"/results/part2_daysummary.csv")
part2_summary_file = paste0(outputdir,"/results/part2_summary.csv")
P2 =read.csv(part2_daysummary_file, stringsAsFactors = FALSE, sep=separator)

# Change ID to numeric:
convertID = function(idValues) {
  if (class(idValues) == "character") {
    idValues = as.character(sapply(idValues, FUN=function(x) unlist(strsplit(x," "))[1]))
    idValues = as.integer(idValues)
  }
  return(idValues)
}
P2$id = convertID(P2$filename)

# select valid days only
P2 = P2[which(P2$N.valid.hours >=16),]

# select subset of potentially relevant variables:
P2 = P2[,c("id", "mean_ENMO_mg_0.24hr", "X.0.50._ENMO_mg_0.24hr",
           "X.50.100._ENMO_mg_0.24hr",  "X.100.150._ENMO_mg_0.24hr",
           "X.150.200._ENMO_mg_0.24hr")]
colnames(P2) = c("id","act","X1","X2","X3","X4")

# calculate slope through intensity curve
getgradient = function(y) {
    # y: numeric vector of time spent in bins
    y = as.vector(unlist(y))
    x = c(25,75,125,175)
    y = ifelse(test = y <=0,yes = NA,no = y)
    ly = log(y)
    lx = log(x)
    y_intercept = NA
    gradient = NA
    rsquared = NA
    if (length(which(is.na(lx) == FALSE)) > 1 & length(which(is.na(ly) == FALSE)) > 1) {
      if (sd(lx,na.rm = TRUE) != 0 & sd(ly,na.rm = TRUE) != 0) {
        fitsum = summary(stats::lm(ly ~ lx))
        y_intercept = stats::coef(fitsum)[1,1]
        gradient = stats::coef(fitsum)[2,1]
      }
    }
    return(data.frame(gradient=gradient,y_intercept=y_intercept))
}
# derive intensity gradient
intgra = apply(P2[,c("X1","X2","X3","X4")],MARGIN = 1, FUN = getgradient)
intgra2 = data.frame(matrix(unlist(intgra), nrow=length(intgra), byrow=T))
colnames(intgra2) = c("gradient", "y_intercept")
# add to P2
P2 = cbind(P2, intgra2)
# Calculate the 90th percentile of the day level variables:
D90 = aggregate.data.frame(P2[,"act"], by = list(P2$id), FUN = function(x) {quantile(x,0.90, na.rm = TRUE) })
colnames(D90) = c("id","act90")
Dmean = aggregate.data.frame(P2[,c("gradient", "y_intercept",  "X1","X2","X3","X4")], by = list(P2$id), FUN = mean)
colnames(Dmean) = c("id","gradient_mean","y_intercept_mean",  "X1","X2","X3","X4")
D = merge(D90, Dmean, by.all="id")

# Add the new variables to the person level output calculated by GGIR part 2:
P2summary = read.csv(part2_summary_file, stringsAsFactors = FALSE, sep=separator)

# Change ID to numeric
P2summary$ID2 = convertID(P2summary$filename)

# Check whether data already has the expected variables
existingvars = which(colnames(P2summary) %in% c("id","act90","gradient_mean","y_intercept_mean", "X1","X2","X3","X4"))
if (length(existingvars) > 0) P2summary = P2summary[,-existingvars]
P2summary_updated = merge(P2summary,D,by.x="ID2",by.y="id")

# Save changes
if (nrow(P2summary_updated) == nrow(P2summary)) {
  write.csv(P2summary_updated,file=part2_summary_file,row.names = FALSE)
}

